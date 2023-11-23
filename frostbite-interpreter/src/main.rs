mod interpreter;
mod rt_value;

use std::{env::args, error::Error, fs, path::PathBuf, process};

use frostbite_parser::{lexer, Parser};

fn main() -> Result<(), Box<dyn Error>> {
    let mut args = args();

    if args.len() != 2 {
        println!("usage: frostbite-interpreter [PATH]");

        process::exit(1);
    }

    let path = PathBuf::from(args.nth(1).unwrap());
    let input = fs::read_to_string(path)?;

    let token_stream = lexer::tokenize(&input).expect("Prototyping");
    let ast = Parser::with_tokenstream(token_stream).parse();

    let ast = match ast {
        Some(ast) => ast,
        None => {
            todo!()
        }
    };

    interpreter::Interpreter::default().run(&ast);

    Ok(())
}
