use std::{
    env, fs,
    io::Write,
    path::PathBuf,
    process,
};

use clap::Parser;
use color_eyre::eyre;

use frostbite_compiler::{
    codegen::CodegenBackends,
    tir::{FunctionType, Type},
    CompilationResults, Compiler,
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
            .level(LevelFilter::Trace)
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
        CliSubcommand::Compile { file, output_file } => {
            let src = fs::read_to_string(&file)?;

            let mut compiler = Compiler::new();

            #[allow(unsafe_code)]
            (unsafe { compiler.ctx_mut() }).insert_intrinsic("print", |types_arena| {
                let return_type = types_arena.insert(Type::Unit);

                types_arena.insert(Type::Function(FunctionType {
                    arguments: [].into(),
                    return_type,
                }))
            });

            let src_id = compiler.add_source(file.display().to_string(), src);

            let output = compiler.compile_source_code(src_id, CodegenBackends::bytecode_backend());

            let CompilationResults { codegen_output } = match output {
                Ok(output) => output,
                Err(_) => {
                    let mut buf = String::new();

                    for report in compiler.ctx().report_ctx.iter() {
                        ReportPrinter::new(&mut buf)
                            .print::<DefaultPrintBackend>(&compiler.ctx().src_map, report)
                            .unwrap();
                    }

                    println!("{buf}");

                    process::exit(1);
                }
            };

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
