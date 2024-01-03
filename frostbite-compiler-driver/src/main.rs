use std::{env, fs, io::Write, path::PathBuf, process};

use clap::Parser;
use color_eyre::eyre;
use frostbite_compiler::{codegen::CodegenBackends, CompilationResults, Compiler};
use frostbite_reports::{
    diagnostic_printer::{DefaultPrintBackend, DiagnosticPrinter},
    Report,
};

#[derive(clap::Parser)]
pub struct CliArgs {
    #[clap(subcommand)]
    subcommand: CliSubcommand,
}

#[derive(clap::Subcommand)]
pub enum CliSubcommand {
    #[command(about = "Compile a file")]
    Compile {
        #[arg(help = "File to compile")]
        file: PathBuf,

        #[arg(short = 'o', long = "output")]
        output_file: Option<PathBuf>,
    },
}

fn main() -> eyre::Result<()> {
    let args = CliArgs::try_parse()?;

    color_eyre::install()?;

    match args.subcommand {
        CliSubcommand::Compile { file, output_file } => {
            let src = fs::read_to_string(&file)?;

            let mut compiler = Compiler::new();

            let src_id = compiler.add_source(file.display().to_string(), src);

            let output = compiler.compile_source_code(src_id, CodegenBackends::bytecode_backend());

            let CompilationResults {
                t_ast: _,
                codegen_output,
            } = match output {
                Ok(output) => output,
                Err(_) => {
                    let mut buf = String::new();

                    for report in compiler.ctx().report_ctx.iter() {
                        match report {
                            Report::Diagnostic(diagnostic) => {
                                DiagnosticPrinter::new(&mut buf)
                                    .print::<DefaultPrintBackend>(
                                        &compiler.ctx().src_map,
                                        diagnostic,
                                    )
                                    .unwrap();
                            }
                            Report::Backtrace(_) => unreachable!(),
                        }
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
