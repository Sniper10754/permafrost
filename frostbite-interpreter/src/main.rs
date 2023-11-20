use std::{env::args, error::Error, fs, path::PathBuf, process};

use frostbite_parser::{ast::Expr, parser::*};

fn main() -> Result<(), Box<dyn Error>> {
    let mut args = args();

    if args.len() != 1 {
        println!("Usage: frostbite-interpreter [PATH]");

        process::exit(1);
    }

    let path = PathBuf::from(args.next().unwrap());
    let content = fs::read_to_string(path)?;

    let ast = ExprParser::new().parse(&content);

    match ast {
        Ok(_) => todo!(),
        Err(parse_error) => {
            frostbite_parser::error::Error::from(parse_error);
        }
    }

    Ok(())
}

fn run_expr(expr: Expr<'_>) {}
