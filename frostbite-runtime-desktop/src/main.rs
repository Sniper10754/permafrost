#![feature(fmt_internals)]

use std::{error::Error, fs, process::exit};

use clap::Parser;
use frostbite_reports::IntoReport;
use frostbite_runtime::Runtime;
use helper::{lex_and_parse, print_report};

mod cli;
mod helper;
mod intrinsics;

fn main() -> Result<(), Box<dyn Error>> {
    let args = cli::CliArgs::parse();

    match args.subcommand {
        cli::CliSubcommand::Run { filepath } => {
            let source = fs::read_to_string(&filepath)?;

            let ast = match lex_and_parse(&source) {
                Ok(ast) => ast,
                Err(reports) => {
                    reports.into_iter().for_each(|report| {
                        print_report(filepath.display().into(), &source, &report)
                    });

                    exit(1);
                }
            };

            let mut runtime = Runtime::new();

            intrinsics::insert_intrinsics(&mut runtime);

            let interpretation_result = runtime.eval_program(&ast);

            if let Err(error) = interpretation_result {
                let report = IntoReport::into_report(error, ());

                print_report(Some(filepath.display()), &source, &report);

                exit(1);
            }

            Ok(())
        }
    }
}
