use std::{
    fs,
    io::{stdout, BufWriter, Write},
    path::PathBuf,
    process,
};

use clap::Parser;
use color_eyre::eyre;
use frostbite_compiler::{codegen::CodegenBackends, Compiler};
use frostbite_reports::{
    diagnostic_printer::{DefaultPrintBackend, DiagnosticPrinter},
    sourcemap::{SourceDescription, SourceMap},
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

    let args = CliArgs::try_parse()?;

    match args.subcommand {
        CliSubcommand::Compile { file, output_file } => {
            let src = fs::read_to_string(&file)?;

            let mut source_map = SourceMap::new();

            let src_id = source_map.push(SourceDescription {
                url: file.clone().into(),
                source_code: src,
            });

            let output = match Compiler::compile_source(
                src_id,
                &mut source_map,
                CodegenBackends::bytecode_backend(),
            ) {
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

            frostbite_bytecode::encode(&output, &mut buf);

            let mut fs_writer = fs::File::open(output_file.unwrap_or("./output".into()))?;

            fs_writer.write_all(&buf)?;
        }
    }

    Ok(())
}
