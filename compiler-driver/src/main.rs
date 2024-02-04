#![feature(never_type)]

use std::{env, fs, io::Write, path::PathBuf, process};

use clap::Parser;
use color_eyre::eyre;

use frostbite_bytecode::Module;
use frostbite_compiler::{codegen::CodegenBackends, context::CompilerContext, Compiler};
use frostbite_reports::printer::{DefaultPrintBackend, ReportPrinter};

mod compile;

#[derive(clap::Parser)]
pub struct CliArgs
{
    #[clap(subcommand)]
    subcommand: CliSubcommand,
}

#[derive(clap::Subcommand)]
pub enum CliSubcommand
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
    },
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
        } => {
            let src = fs::read_to_string(&file)?;

            let mut compiler = Compiler::new();

            let src_key = compiler.add_source(file.clone(), src);

            if compiler.analyze_module(src_key).is_err() {
                bail(compiler.move_ctx());
            }

            let mut codegen_backend = CodegenBackends::bytecode_backend();

            let codegen_outputs = compiler
                .compile(&mut codegen_backend)
                .map_err(bail)
                .unwrap();

            let codegen_output = &codegen_outputs[src_key];

            if disassemble {
                disassemble_and_print(codegen_output)?;
            }

            let mut buf = vec![];

            frostbite_bytecode::encode(codegen_output, &mut buf);

            let output_file =
                output_file.unwrap_or_else(|| env::temp_dir().join("frostbite-compiler-output"));

            let mut fs_writer = fs::OpenOptions::new()
                .read(true)
                .write(true)
                .create(true)
                .append(false)
                .open(output_file)?;

            fs_writer.write_all(&buf)?;
        }
    }

    Ok(())
}

fn bail(ctx: CompilerContext) -> !
{
    print_reports(&ctx);

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

fn disassemble_and_print(module: &Module) -> eyre::Result<()>
{
    let mut buf = String::new();

    frostbite_bytecode::text_repr::print_bytecode(&mut buf, module)?;

    println!("{buf}");

    Ok(())
}
