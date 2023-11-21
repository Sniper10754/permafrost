mod interpreter;
mod rt_value;
mod helper;

use std::{env::args, error::Error, fs, path::PathBuf, process};

use frostbite_parser::Parser;

fn main() -> Result<(), Box<dyn Error>> {
    let mut args = args();

    if args.len() != 2 {
        println!("usage: frostbite-interpreter [PATH]");

        process::exit(1);
    }

    let path = PathBuf::from(args.nth(1).unwrap());
    let content = fs::read_to_string(path)?;

    let ast = Parser::new().parse(&content);

    let ast = match ast {
        Ok(ast) => ast,
        Err(err) => {
            return Err(format!("parser: {err:?}").into());
        }
    };

    interpreter::Interpreter::default().run(&ast);

    Ok(())
}
