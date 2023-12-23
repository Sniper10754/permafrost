#![no_std]

extern crate alloc;

use alloc::{vec, vec::Vec};
use codegen::CodegenBackend;
use frostbite_parser::{
    lexer::tokenize,
    Parser,
};
use frostbite_reports::{
    sourcemap::{SourceId, SourceMap},
    IntoReport, Report,
};
use frostbite_semantic::run_semantic_checks;

pub mod codegen;

pub struct Compiler {
    source_map: SourceMap,
}

impl Compiler {
    pub fn compile_source<C: CodegenBackend>(
        &mut self,
        source_id: SourceId,
        source: impl AsRef<str>,
    ) -> Result<C::Output, Vec<Report>> {
        let source = source.as_ref();

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

        run_semantic_checks(source_id, &self.source_map, &ast)?;

        let mut codegen_backend = C::default();
        let mut codegen_errors = vec![];

        for expr in ast.exprs.iter() {
            if let Err(err) = codegen_backend.codegen(expr) {
                codegen_errors.push(err);
            }
        }

        if !codegen_errors.is_empty() {
            return Err(codegen_errors
                .into_iter()
                .map(|error| error.into_report())
                .collect());
        }

        if codegen_errors.is_empty() {
            Ok(codegen_backend.finalize())
        } else {
            Err(codegen_errors
                .into_iter()
                .map(|error| error.into_report())
                .collect())
        }
    }
}
