#![no_std]

extern crate alloc;

use alloc::vec::Vec;
use codegen::CodegenBackend;
use frostbite_parser::{lexer::tokenize, Parser};
use frostbite_reports::{
    sourcemap::{SourceId, SourceMap},
    IntoReport, Report, ReportContext,
};
use hir::HirTree;
use semantic::run_semantic_checks;

pub mod codegen;
pub mod hir;
pub mod semantic;

mod utils {
    use frostbite_reports::ReportContext;

    pub fn bail_on_errors(report_ctx: &ReportContext) -> Result<(), ()> {
        if report_ctx.has_errors() {
            Err(())
        } else {
            Ok(())
        }
    }
}

#[derive(Debug)]
pub struct CompilationResults<C: CodegenBackend> {
    hir: HirTree,
    codegen_output: C::Output,
}

pub struct Compiler;

impl Compiler {
    pub fn compile_source<C: CodegenBackend>(
        report_ctx: &mut ReportContext,
        source_id: SourceId,
        src_map: &mut SourceMap,
        codegen: C,
    ) -> Result<CompilationResults<C>, ()> {
        let source = &src_map.get(source_id).unwrap().source_code;

        let token_stream = tokenize(report_ctx, source_id, source);

        utils::bail_on_errors(report_ctx)?;

        let ast = Parser::with_tokenstream(report_ctx, token_stream, source_id).parse();

        utils::bail_on_errors(report_ctx)?;

        let mut hir = HirTree::default();

        run_semantic_checks(report_ctx, source_id, src_map, &ast, &mut hir);

        utils::bail_on_errors(report_ctx)?;

        let codegen_output = C::codegen(codegen, report_ctx, &hir)?;

        utils::bail_on_errors(report_ctx)?;

        let compilation_results = CompilationResults {
            codegen_output,
            hir,
        };

        Ok(compilation_results)
    }
}
