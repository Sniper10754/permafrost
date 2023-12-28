use std::{env::args, fs};

use color_eyre::eyre::{self, bail};

fn main() -> eyre::Result<()> {
    let args: Vec<String> = args().collect();

    if args.len() != 2 {
        bail!(
            r#"
    Usage: {} [FILEPATH]
    Expected 1 command line argument, found {}
            "#,
            &args[0],
            args.len() - 1
        );
    }

    color_eyre::install()?;

    let file_content = fs::read(&args[1])?;

    let bytecode = frostbite_bytecode::decode(&file_content)?;

    {
        let mut buf = String::new();

        frostbite_bytecode::text_repr::print_bytecode(&mut buf, &bytecode)?;

        println!("{buf}")
    }

    Ok(())
}
