use std::{fs, path::PathBuf};

use color_eyre::eyre::{self};

#[derive(clap::Parser)]
enum CliArgs {
    #[command(about = "Disassemble a file")]
    Disassemble {
        #[arg(help = "The file path to disassemble")]
        filepath: PathBuf,
    },
}

fn main() -> eyre::Result<()> {
    let cil_args: CliArgs = clap::Parser::parse();

    color_eyre::install()?;

    match cil_args {
        CliArgs::Disassemble { ref filepath } => {
            let file_content = fs::read(filepath)?;

            let bytecode = frostbite_bytecode::decode(&file_content)?;

            {
                let mut buf = String::new();

                frostbite_bytecode::text_repr::print_bytecode(&mut buf, &bytecode)?;

                println!("{buf}")
            }
        }
    }

    Ok(())
}
