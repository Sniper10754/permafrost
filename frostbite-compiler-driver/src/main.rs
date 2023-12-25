use std::{env, fs, io::Write, path::PathBuf, process};

use clap::Parser;
use color_eyre::eyre;
use frostbite_compiler::{codegen::CodegenBackends, Compiler};
use frostbite_reports::{
    diagnostic_printer::{DefaultPrintBackend, DiagnosticPrinter},
    sourcemap::{SourceDescription, SourceMap},
    ReportContext,
};

#[derive(clap::Parser)]
pub struct CliArgs {
    #[clap(subcommand)]
    subcommand: CliSubcommand,
}

#[derive(clap::Subcommand)]
pub enum CliSubcommand {
    Compile {
        file: PathBuf,

        output_file: Option<PathBuf>,
    },
}

fn main() -> eyre::Result<()> {
    color_eyre::install()?;

    let debug = env::var("DEBUG").is_ok();
    let args = CliArgs::try_parse()?;

    match args.subcommand {
        CliSubcommand::Compile { file, output_file } => {
            let src = fs::read_to_string(&file)?;

            let mut source_map = SourceMap::new();

            let src_id = source_map.push(SourceDescription {
                url: file.clone().into(),
                source_code: src,
            });

            let mut report_ctx = ReportContext::default();

            let output = Compiler::compile_source(
                &mut report_ctx,
                src_id,
                &mut source_map,
                CodegenBackends::bytecode_backend(),
            );

            if output.is_err() {
                
            }

            if debug {
                println!("{hir}");
            }

            let module = match output {
                Ok(output) => output,
                Err(reports) => {
                    let mut buf = String::new();

                    for report in reports {
                        match report {
                            frostbite_reports::Report::Diagnostic(diagnostic) => {
                                DiagnosticPrinter::new(&mut buf)
                                    .print::<DefaultPrintBackend>(&source_map, &diagnostic)
                                    .unwrap();
                            }
                            frostbite_reports::Report::Backtrace(_) => todo!(),
                        }
                    }

                    println!("{buf}");

                    process::exit(1);
                }
            };

            let mut buf = vec![];

            frostbite_bytecode::encode(&module, &mut buf);

            let output_file = output_file.unwrap_or("./output".into());

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
