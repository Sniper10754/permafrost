#![no_std]

extern crate alloc;

use codegen::{CodegenBackend, CodegenError};
use context::CompilerContext;
use frostbite_parser::{lexer::tokenize, Parser};
use frostbite_reports::{
    sourcemap::{SourceId, SourceMap},
    ReportContext,
};
use semantic::run_semantic_checks;
use tir::TypedAst;

pub mod codegen;
pub(crate) mod context;
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
    pub fn compile_source_code<C: CodegenBackend>(
        report_ctx: &mut ReportContext,
        src_map: &mut SourceMap,
        source_id: SourceId,
        codegen: C,
    ) -> Result<CompilationResults<C>, CodegenError> {
        let mut ctx = CompilerContext::new(src_map, report_ctx);

        let source = ctx.src_map.get(source_id).unwrap().source_code.as_str();
        let token_stream = tokenize(ctx.report_ctx, source_id, source);
        utils::bail_on_errors(ctx.report_ctx)?;

        let ast = Parser::with_tokenstream(ctx.report_ctx, token_stream, source_id).parse();
        ctx.asts.insert(source_id, ast);
        utils::bail_on_errors(ctx.report_ctx)?;

        let (t_ast,) = run_semantic_checks(&mut ctx, source_id);
        utils::bail_on_errors(ctx.report_ctx)?;

        let codegen_output = C::codegen(codegen, ctx.report_ctx, &t_ast)?;
        utils::bail_on_errors(ctx.report_ctx)?;

        let compilation_results = CompilationResults {
            t_ast,
            codegen_output,
        };

        Ok(compilation_results)
    }
}
