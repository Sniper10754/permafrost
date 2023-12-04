use std::{error::Error, fs, process::exit};

use clap::Parser;
use frostbite_reports::IntoReport;
use helper::{lex_and_parse, print_report};
use interpreter::Interpreter;

mod cli;
mod error;
mod helper;
mod interpreter;

fn main() -> Result<(), Box<dyn Error>> {
    let args = cli::CliArgs::parse();

    match args.subcommand {
        cli::CliSubcommand::Run { filepath } => {
            let source = fs::read_to_string(&filepath)?;

            let ast = match lex_and_parse(&source) {
                Ok(ast) => ast,
                Err(reports) => {
                    reports.into_iter().for_each(|report| {
                        print_report(Some(filepath.display()), &source, &report)
                    });

                    exit(1);
                }
            };

            let mut interpreter = Interpreter::new();

            let interpretation_result = interpreter.eval_program(&ast);

            if let Err(error) = interpretation_result {
                let report = IntoReport::into_report(error, ());

                print_report(Some(filepath.display()), &source, &report);

                exit(1);
            }

            Ok(())
        }
    }
}
