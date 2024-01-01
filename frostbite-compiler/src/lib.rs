#![no_std]

extern crate alloc;

use codegen::{CodegenBackend, CodegenError};
use frostbite_parser::{lexer::tokenize, Parser};
use frostbite_reports::{
    sourcemap::{SourceId, SourceMap},
    ReportContext,
};
use semantic::run_semantic_checks;
use tir::TypedAst;

pub mod codegen;
pub mod semantic;
pub mod tir;

mod utils {
    use frostbite_reports::ReportContext;

    use crate::codegen::CodegenError;

    pub fn bail_on_errors(report_ctx: &ReportContext) -> Result<(), CodegenError> {
        if report_ctx.has_errors() {
            Err(CodegenError)
        } else {
            Ok(())
        }
    }
}

#[derive(Debug)]
pub struct CompilationResults<C: CodegenBackend> {
    pub t_ast: TypedAst,
    pub codegen_output: C::Output,
}

pub struct Compiler;

impl Compiler {
    pub fn compile_source<C: CodegenBackend>(
        report_ctx: &mut ReportContext,
        source_id: SourceId,
        src_map: &mut SourceMap,
        codegen: C,
    ) -> Result<CompilationResults<C>, CodegenError> {
        let source = &src_map.get(source_id).unwrap().source_code;

        let token_stream = tokenize(report_ctx, source_id, source);

        utils::bail_on_errors(report_ctx)?;

        let ast = Parser::with_tokenstream(report_ctx, token_stream, source_id).parse();

        utils::bail_on_errors(report_ctx)?;

        let (t_ast,) = run_semantic_checks(report_ctx, source_id, src_map, &ast);

        utils::bail_on_errors(report_ctx)?;

        let codegen_output = C::codegen(codegen, report_ctx, &t_ast)?;

        utils::bail_on_errors(report_ctx)?;

        let compilation_results = CompilationResults {
            codegen_output,
            t_ast,
        };

        Ok(compilation_results)
    }
}
