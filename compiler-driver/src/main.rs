use std::{env, fs, io::Write, path::PathBuf, process};

use clap::Parser;
use color_eyre::eyre;

use frostbite_bytecode::Module;
use frostbite_compiler::{
    codegen::CodegenBackends, context::CompilerContext, CompilationResults, Compiler,
};
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

            let src_id = compiler.add_source(file.display().to_string(), src);

            let Ok(module_key) = compiler.compile_module(src_id) else {
                bail_on_compiler_err(compiler.ctx());

                unreachable!();
            };

            let output =
                compiler.codegen_module(module_key, &mut CodegenBackends::bytecode_backend());

            let Ok(CompilationResults { codegen_output }) = output else {
                bail_on_compiler_err(compiler.ctx());

                unreachable!();
            };

            if disassemble {
                disassemble_and_print(&codegen_output)?;
            }

            let mut buf = vec![];

            frostbite_bytecode::encode(&codegen_output, &mut buf);

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

fn bail_on_compiler_err(ctx: &CompilerContext)
{
    if ctx.report_ctx.has_errors() {
        print_reports(ctx);

        process::exit(1);
    }
}

fn print_reports(ctx: &CompilerContext)
{
    let mut buf = String::new();

    let mut report_printer = ReportPrinter::new(&mut buf);

    ctx.report_ctx.iter().for_each(|report| {
        report_printer
            .print::<DefaultPrintBackend>(&ctx.src_map, report)
            .unwrap();
    });

    println!("{buf}");
}

fn disassemble_and_print(module: &Module) -> eyre::Result<()>
{
    let mut buf = String::new();

    frostbite_bytecode::text_repr::print_bytecode(&mut buf, module)?;

    println!("{buf}");

    Ok(())
}