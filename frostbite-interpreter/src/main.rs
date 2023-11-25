mod arithmetic;
mod error;
mod interpreter;
mod rt_value;

use std::{env::args, error::Error, fs, path::PathBuf, process};

use error::InterpreterError;
use frostbite_parser::{lexer, Parser};
use frostbite_report_interface::print_backend::DefaultBackend;

fn main() -> Result<(), Box<dyn Error>> {
    let mut args = args();

    if args.len() != 2 {
        println!("usage: frostbite-interpreter [PATH]");

        process::exit(1);
    }

    let path = PathBuf::from(args.nth(1).expect("Unreachable: checked earlier"));
    let input = fs::read_to_string(&path)?;

    let token_stream = lexer::tokenize(&input).expect("Prototyping");
    let ast = Parser::with_tokenstream(token_stream).parse();

    let ast = match ast {
        Some(ast) => ast,
        None => {
            todo!()
        }
    };

    match interpreter::Interpreter::default().run(&ast) {
        Ok(_) => (),
        Err(err) => match err {
            InterpreterError::Panic(report) => {
                let mut buf = String::new();

                frostbite_report_interface::print::ReportPrinter::new()
                    .print::<_, DefaultBackend>(&mut buf, Some(path.display()), input, &report)?;

                println!("{buf}")
            }
        },
    };

    Ok(())
}
