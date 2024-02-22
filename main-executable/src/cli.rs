use std::path::PathBuf;

use color_eyre::eyre;
use permafrost_compiler::codegen::{CodegenBackend, CodegenBackends};

pub static LOGO: &str = r#"
              
._   _  ._ ._ _   _. _|_ ._ _   _ _|_ 
|_) (/_ |  | | | (_|  |  | (_) _>  |_ 
|                                     
"#;

#[derive(clap::Parser)]
pub struct Arguments
{
    #[clap(subcommand)]
    pub subcommand: Subcommand,

    #[clap(help = "Dont show the permafrost logo")]
    pub no_logo: bool,

    #[clap(help = "Hangs the program and prints the PID, resumes execution on user input..")]
    pub debug: bool,
}

pub fn hand_execution() -> eyre::Result<()>
{
    use std::{io::Read, process};

    println!("PID: {}", process::id());
    println!("Press any key to continue");

    _ = std::io::stdin().read(&mut [])?;

    Ok(())
}

#[derive(clap::Subcommand)]
pub enum Subcommand
{
    #[clap(about = "Compile a file")]
    Compile
    {
        #[clap(help = "File to compile")]
        file: PathBuf,

        #[clap(short = 'o', long = "output")]
        output_file: Option<PathBuf>,

        #[clap(help = "Disassemble once compilation is done", long = "disassemble")]
        disassemble: bool,

        #[clap(help = "Codegen backend to use", long, short, value_enum, default_value_t=CodegenKind::Bytecode)]
        codegen_option: CodegenKind,
    },
}

#[derive(clap::ValueEnum, Clone, Copy, Debug)]
pub enum CodegenKind
{
    Bytecode,
}

impl CodegenKind
{
    pub fn into_codegen_backend(self) -> impl CodegenBackend
    {
        match self {
            CodegenKind::Bytecode => CodegenBackends::bytecode_backend(),
        }
    }
}
