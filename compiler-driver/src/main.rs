#![feature(never_type)]

use std::{
    env, fs,
    io::{self, Read, Write},
    path::PathBuf,
    process,
};

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
    #[clap(about = "Compile a file")]
    Compile
    {
        #[clap(help = "File to compile")]
        file: PathBuf,

        #[clap(short = 'o', long = "output")]
        output_file: Option<PathBuf>,

        #[clap(help = "Disassemble once compilation is done", long = "disassemble")]
        disassemble: bool,

        #[clap(help = "Codegen backend to use", long, short, value_enum, default_value_t=CodegenOptions::Bytecode)]
        codegen_option: CodegenOptions,
    },
}

#[derive(clap::ValueEnum, Clone, Copy, Debug)]
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
                    "[{} @ {}] {}",
                    colors.color(record.level()),
                    record
                        .file()
                        .map(|file| format!("{file}:{}", record.line().unwrap()))
                        .unwrap_or_else(|| record.target().into()),
                    message,
                ))
            })
            .level(LevelFilter::Trace)
            .chain(std::io::stdout())
            .apply()?;
    }

    Ok(())
}

fn main() -> eyre::Result<()>
{
    let args = CliArgs::parse();

    color_eyre::install()?;

    setup_logger()?;

    if env::var("DEBUG").is_ok() {
        println!("PID: {}", process::id());
        println!("Press any key to continue");

        _ = io::stdin().read(&mut [])?;
    }

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
        Some(extension) if extension == PERMAFROST_FILE_EXTENSION => (),
        Some(..) | None => log::warn!(
            "The recommended file extension is `{}`",
            PERMAFROST_FILE_EXTENSION
        ),
    }

    let src = fs::read_to_string(&file)?;

    let mut codegen = codegen_option.into_codegen_backend();

    let mut ctx = CompilerContext::new();

    let mut compiler = Compiler::new(&mut ctx);

    let Ok((file_src_key, codegen_output)) = call_compiler(&mut compiler, file, src, &mut codegen)
    else {
        bail(&ctx)
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

fn bail(context: &CompilerContext) -> !
{
    print_reports(context);

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
        .truncate(true)
        .open(output_file)?;

    fs_writer.write_all(&buf)?;

    Ok(())
}
