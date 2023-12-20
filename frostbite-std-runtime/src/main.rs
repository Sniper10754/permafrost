#![feature(fmt_internals)]

use std::{error::Error, fs, process::exit};

use clap::Parser;
use frostbite_reports::{
    sourcemap::{SourceId, SourceMap},
    IntoReport,
};
use frostbite_runtime::{intrinsic::IntrinsicContext, Runtime};
use helper::{lex_and_parse, print_report};

mod cli;
mod helper;
mod intrinsics;

pub struct StdRuntime<'id, 'src> {
    source_map: SourceMap<'id, 'src>,
    runtime: Runtime<'id, 'src>,
}

impl<'id, 'src> StdRuntime<'id, 'src> {
    pub fn new() -> Self {
        Self {
            source_map: SourceMap::new(),
            runtime: Runtime::new(),
        }
    }
}

impl<'id, 'src> Default for StdRuntime<'id, 'src> {
    fn default() -> Self {
        Self::new()
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = cli::CliArgs::parse();

    match args.subcommand {
        cli::CliSubcommand::Run { filepath } => {
            let source = fs::read_to_string(&filepath)?;
            let main_src_id = SourceId::Filepath(&filepath);

            let mut std_runtime = StdRuntime::new();

            std_runtime.source_map.insert(main_src_id, &source);
            *std_runtime.runtime.sandbox_mut().main_file_source_id_mut() = main_src_id;

            let ast = match lex_and_parse(&source, main_src_id) {
                Ok(ast) => ast,
                Err(reports) => {
                    reports
                        .into_iter()
                        .for_each(|report| print_report(&std_runtime.source_map, &report));

                    exit(1);
                }
            };

            let mut intrinsic_ctx = IntrinsicContext::new();

            intrinsics::insert_intrinsics(&mut intrinsic_ctx);

            let mut runtime = Runtime::with_intrinsic_ctx(intrinsic_ctx);

            let interpretation_result = runtime.eval_ast_tree(&ast);

            if let Err(error) = interpretation_result {
                let report = IntoReport::into_report(error, main_src_id);

                print_report(&std_runtime.source_map, &report);

                exit(1);
            }

            Ok(())
        }
    }
}
