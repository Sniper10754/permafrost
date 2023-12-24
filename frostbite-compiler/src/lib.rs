#![no_std]

extern crate alloc;

use alloc::vec::Vec;
use codegen::CodegenBackend;
use frostbite_parser::{lexer::tokenize, Parser};
use frostbite_reports::{
    sourcemap::{SourceId, SourceMap},
    IntoReport, Report,
};
use semantic::run_semantic_checks;

pub mod codegen;
pub mod hir;
pub mod semantic;

pub struct Compiler;

impl Compiler {
    pub fn compile_source<C: CodegenBackend>(
        source_id: SourceId,
        src_map: &mut SourceMap,
        codegen: C,
    ) -> Result<C::Output, Vec<Report>> {
        let source = &src_map.get(source_id).unwrap().source_code;

        let token_stream = tokenize(source_id, source).map_err(|err| {
            err.into_iter()
                .map(|lex_err| lex_err.into_report())
                .collect::<Vec<_>>()
        })?;

        let ast = Parser::with_tokenstream(token_stream, source_id)
            .parse()
            .map_err(|errors| {
                errors
                    .into_iter()
                    .map(|error| error.into_report())
                    .collect::<Vec<_>>()
            })?;

        let hir = run_semantic_checks(source_id, src_map, &ast)?;

        match codegen.codegen(&hir) {
            Ok(output) => Ok(output),
            Err(errors) => Err(errors.into_iter().map(|err| err.into_report()).collect()),
        }
    }
}
