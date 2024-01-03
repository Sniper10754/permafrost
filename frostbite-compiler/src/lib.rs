#![no_std]

extern crate alloc;

use alloc::string::String;
use codegen::{CodegenBackend, CodegenError};
use context::CompilerContext;
use frostbite_parser::{lexer::tokenize, Parser};
use frostbite_reports::sourcemap::{SourceDescription, SourceId, SourceUrl};
use semantic::run_semantic_checks;
use tir::TypedAst;

pub mod codegen;
pub mod context;
pub mod intrinsic;
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
pub struct CompilationResults<'a, C: CodegenBackend> {
    pub t_ast: &'a TypedAst,
    pub codegen_output: C::Output,
}

#[derive(Debug, Default)]
pub struct Compiler {
    ctx: CompilerContext,
}

impl Compiler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_source(&mut self, url: impl Into<SourceUrl>, source: impl Into<String>) -> SourceId {
        self.ctx.src_map.insert(SourceDescription {
            url: url.into(),
            source_code: source.into(),
        })
    }

    pub fn compile_source_code<C: CodegenBackend>(
        &mut self,
        source_id: SourceId,
        codegen: C,
    ) -> Result<CompilationResults<'_, C>, CodegenError> {
        let source = self
            .ctx
            .src_map
            .get(source_id)
            .unwrap()
            .source_code
            .as_str();

        let token_stream = tokenize(&mut self.ctx.report_ctx, source_id, source);
        utils::bail_on_errors(&self.ctx.report_ctx)?;

        let ast =
            Parser::with_tokenstream(&mut self.ctx.report_ctx, token_stream, source_id).parse();
        self.ctx.asts.insert(source_id, ast);
        utils::bail_on_errors(&self.ctx.report_ctx)?;

        run_semantic_checks(&mut self.ctx, source_id);

        let t_ast = &self.ctx.t_asts[source_id];

        utils::bail_on_errors(&self.ctx.report_ctx)?;

        let codegen_output = C::codegen(codegen, &mut self.ctx.report_ctx, t_ast)?;
        utils::bail_on_errors(&self.ctx.report_ctx)?;

        let compilation_results = CompilationResults {
            t_ast,
            codegen_output,
        };

        Ok(compilation_results)
    }

    pub fn ctx(&self) -> &CompilerContext {
        &self.ctx
    }

    pub fn explode(self) -> CompilerContext {
        self.ctx
    }
}
