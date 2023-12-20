use std::path::PathBuf;

#[derive(Debug, clap::Parser)]
#[clap()]
pub struct CliArgs {
    #[command(subcommand)]
    pub subcommand: CliSubcommand,
}

#[derive(Debug, clap::Subcommand)]
pub enum CliSubcommand {
    #[command(about = "Run a frostbite language file")]
    Run { filepath: PathBuf },
}
