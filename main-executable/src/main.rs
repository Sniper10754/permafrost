#![feature(never_type)]

use clap::Parser;
use color_eyre::eyre;

mod cli;
mod compile;
mod logging;

fn main() -> eyre::Result<()>
{
    let args = cli::Arguments::parse();

    if !args.no_logo {
        println!("{}", cli::LOGO)
    }

    color_eyre::install()?;

    logging::setup_logger()?;

    if args.debug {
        cli::hand_execution()?;
    }

    match args.subcommand {
        cli::Subcommand::Compile {
            file,
            output_file,
            disassemble,
            codegen_option,
        } => compile::compile_file_entry(file, output_file, disassemble, codegen_option),
    }
}
