mod arithmetic;
mod error;
mod interpreter;
mod repl;
mod rt_value;

use interpreter::Interpreter;

use std::{
    error::Error,
    fs,
    io::{stdin, stdout, Read},
    path::PathBuf,
    process,
};

use error::InterpreterError;

use frostbite_report_interface::{print::ReportPrinter, print_backend::DefaultBackend};

#[derive(Debug, clap::Parser)]
struct CliArgs {
    #[command(subcommand)]
    subcommand: CliSubommand,
}

#[derive(Debug, Clone, clap::Subcommand)]
enum CliSubommand {
    #[command(about = "Run a file")]
    Run {
        #[arg(help = "File to run")]
        path: PathBuf,
    },
    #[command(about = "Evaluate a string passed either from stdin or from the command line")]
    Evaluate {
        #[arg(help = "String to evaluate")]
        string: Option<String>,
    },
    #[command(about = "Start a REPL")]
    Repl,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: CliArgs = clap::Parser::parse();

    match args.subcommand {
        CliSubommand::Run { path } => {
            let interpreter = Interpreter::default();

            let source = fs::read_to_string(&path)?;

            let result = interpreter.run_source_in_thread(&source);

            let mut buf = String::new();

            if let Err(error) = result {
                match error {
                    InterpreterError::Runtime { report } => {
                        ReportPrinter::new(&mut buf)
                            .print::<DefaultBackend>(Some(path.display()), &source, &report)
                            .unwrap();
                    }
                    InterpreterError::Io(error) => {
                        println!("IO Error: {error}");
                    }
                }

                process::exit(1);
            }

            println!("{buf}");
        }
        CliSubommand::Evaluate { string } => {
            let string = match string {
                Some(string) => string,
                None => {
                    let mut buf = String::new();

                    stdin().lock().read_to_string(&mut buf)?;

                    buf
                }
            };
        }
        CliSubommand::Repl => todo!(),
    }

    Ok(())
}
