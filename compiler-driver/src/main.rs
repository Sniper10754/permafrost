#![feature(never_type)]

use std::{env, fs, io::Write, path::PathBuf, process};

use clap::Parser;
use color_eyre::eyre;

use compile::call_compiler;
use permafrost_compiler::{
    codegen::{CodegenBackend, CodegenBackends, CodegenOutput, PrintableCodegenOutput},
    context::CompilerContext,
    Compiler, PERMAFROST_FILE_EXTENSION,
};
use permafrost_reports::printer::{DefaultPrintBackend, ReportPrinter};

mod compile;

#[derive(clap::Parser)]
struct CliArgs
{
    #[clap(subcommand)]
    subcommand: CliSubcommand,
}

#[derive(clap::Subcommand)]
enum CliSubcommand
{
    #[command(about = "Compile a file")]
    Compile
    {
        #[arg(help = "File to compile")]
        file: PathBuf,

        #[arg(short = 'o', long = "output")]
        output_file: Option<PathBuf>,

        #[arg(help = "Disassemble once compilation is done", long = "disassemble")]
        disassemble: bool,

        #[arg(help = "Codegen backend to use", long, short, value_enum, default_value_t=CodegenOptions::Bytecode)]
        codegen_option: CodegenOptions,
    },
}

#[derive(clap::ValueEnum, Clone, Debug)]
enum CodegenOptions
{
    Bytecode,
}

impl CodegenOptions
{
    pub fn into_codegen_backend(self) -> impl CodegenBackend
    {
        match self {
            CodegenOptions::Bytecode => CodegenBackends::bytecode_backend(),
        }
    }
}

fn setup_logger() -> eyre::Result<()>
{
    {
        use fern::colors::{Color, ColoredLevelConfig};
        use log::LevelFilter;

        let colors = ColoredLevelConfig::new()
            .info(Color::Green)
            .warn(Color::BrightYellow)
            .error(Color::BrightRed)
            .debug(Color::BrightMagenta)
            .trace(Color::BrightCyan);

        fern::Dispatch::new()
            .format(move |out, message, record| {
                out.finish(format_args!(
                    "[{} {}] {}",
                    colors.color(record.level()),
                    record.target(),
                    message,
                ))
            })
            .level(LevelFilter::Info)
            .chain(std::io::stdout())
            .apply()?;
    }

    Ok(())
}

fn main() -> eyre::Result<()>
{
    setup_logger()?;

    let args = CliArgs::try_parse()?;

    color_eyre::install()?;

    match args.subcommand {
        CliSubcommand::Compile {
            file,
            output_file,
            disassemble,
            codegen_option,
        } => compile_file(file, output_file, disassemble, codegen_option),
    }
}

fn compile_file(
    file: PathBuf,
    output_file: Option<PathBuf>,
    disassemble: bool,
    codegen_option: CodegenOptions,
) -> eyre::Result<()>
{
    match file
        .extension()
        .map(|extension| extension.to_string_lossy())
        .as_deref()
    {
        Some("pmf") => (),
        Some(..) | None => log::warn!(
            "The reccomended file extension is `{}`",
            PERMAFROST_FILE_EXTENSION
        ),
    }

    let src = fs::read_to_string(&file)?;

    let mut codegen = codegen_option.into_codegen_backend();
    let mut ctx = CompilerContext::new();
    let mut compiler = Compiler::new(&mut ctx);

    let Ok((file_src_key, codegen_output)) = call_compiler(&mut compiler, file, src, &mut codegen)
    else {
        bail(&compiler)
    };

    let file_codegen_output = codegen_output.get_file(file_src_key).unwrap();

    if disassemble {
        if let Some(printable) = file_codegen_output.as_printable() {
            disassemble_and_print(printable)?;
        } else {
            log::warn!("Codegen output doesnt support printing");
        }
    }

    write_output_to_file(file_codegen_output, output_file)?;

    Ok(())
}

fn bail(compiler: &Compiler) -> !
{
    print_reports(compiler.ctx());

    process::exit(1);
}

fn print_reports(ctx: &CompilerContext)
{
    let mut buf = String::new();

    let mut report_printer = ReportPrinter::new(&mut buf);

    report_printer
        .print_reports::<DefaultPrintBackend, _>(&ctx.src_map, &*ctx.report_ctx)
        .unwrap();

    println!("{buf}");
}

fn disassemble_and_print<C>(output: &C) -> eyre::Result<()>
where
    C: PrintableCodegenOutput + ?Sized,
{
    let mut buf = String::new();

    output.print(&mut buf)?;

    println!("{buf}");

    Ok(())
}

fn write_output_to_file(
    output: &impl CodegenOutput,
    output_file: Option<PathBuf>,
) -> eyre::Result<()>
{
    let mut buf = vec![];

    output.serialize(&mut buf);

    let output_file =
        output_file.unwrap_or_else(|| env::temp_dir().join("permafrost-compiler-output"));

    let mut fs_writer = fs::OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .append(false)
        .open(output_file)?;

    fs_writer.write_all(&buf)?;

    Ok(())
}
