#![no_std]

extern crate alloc;

use alloc::string::String;
use codegen::CodegenBackend;
use context::CompilerContext;
use frostbite_parser::{
    lexer::{tokenize, TokenStream},
    Parser,
};
use frostbite_reports::{
    sourcemap::{SourceDescription, SourceId, SourceUrl},
    ReportContext,
};
use semantic::run_semantic_checks;

pub mod codegen;
pub mod context;
pub mod intrinsic;
pub mod semantic;
pub mod tir;
pub mod utils;

fn bail_on_errors(report_ctx: &ReportContext) -> Result<(), CompilerError>
{
    if report_ctx.has_errors() {
        Err(CompilerError)
    } else {
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct CompilerError;

#[derive(Debug)]
pub struct CompilationResults<C: CodegenBackend>
{
    pub codegen_output: C::Output,
}

#[derive(Debug, Default)]
pub struct Compiler
{
    ctx: CompilerContext,
}

impl Compiler
{
    pub fn new() -> Self
    {
        Self::default()
    }

    pub fn add_source(
        &mut self,
        url: impl Into<SourceUrl>,
        source: impl Into<String>,
    ) -> SourceId
    {
        self.ctx.src_map.insert(SourceDescription {
            url: url.into(),
            source_code: source.into(),
        })
    }

    pub fn compile_source_code<C: CodegenBackend>(
        &mut self,
        source_id: SourceId,
        codegen: C,
    ) -> Result<CompilationResults<C>, CompilerError>
    {
        let source = self
            .ctx
            .src_map
            .get(source_id)
            .unwrap()
            .source_code
            .as_str();

        log::trace!("Lexing...");
        let token_stream = Self::lex(&mut self.ctx.report_ctx, source_id, source)?;

        log::trace!("Parsing...");
        // AST is stored in the compiler context
        Self::parse(&mut self.ctx, token_stream, source_id)?;

        // TAST is stored in the compiler context
        log::trace!("Running semantic checks...");
        run_semantic_checks(&mut self.ctx, source_id);

        log::debug!(
            "Typed Internal representation: {}",
            tir::display::display_tree(&self.ctx.t_asts[source_id])
        );

        bail_on_errors(&self.ctx.report_ctx)?;

        log::trace!("Codegenning...");
        let codegen_output = Self::codegen(&mut self.ctx, source_id, codegen)?;

        let compilation_results = CompilationResults { codegen_output };

        log::trace!("Done...");
        Ok(compilation_results)
    }

    fn lex(
        report_ctx: &mut ReportContext,
        source_id: SourceId,
        source: &str,
    ) -> Result<TokenStream, CompilerError>
    {
        let token_stream = tokenize(report_ctx, source_id, source);

        bail_on_errors(report_ctx)?;

        Ok(token_stream)
    }

    fn parse(
        compiler_ctx: &mut CompilerContext,
        token_stream: TokenStream,
        source_id: SourceId,
    ) -> Result<(), CompilerError>
    {
        let ast =
            Parser::with_tokenstream(&mut compiler_ctx.report_ctx, token_stream, source_id).parse();

        compiler_ctx.asts.insert(source_id, ast);

        bail_on_errors(&compiler_ctx.report_ctx)?;

        Ok(())
    }

    fn codegen<C: CodegenBackend>(
        compiler_ctx: &mut CompilerContext,
        main_source_id: SourceId,
        codegen: C,
    ) -> Result<C::Output, CompilerError>
    {
        let t_ast = &compiler_ctx.t_asts[main_source_id];

        let output = codegen.codegen(&mut compiler_ctx.report_ctx, t_ast);

        bail_on_errors(&compiler_ctx.report_ctx)?;

        Ok(output)
    }

    pub fn ctx(&self) -> &CompilerContext
    {
        &self.ctx
    }

    pub fn explode(self) -> CompilerContext
    {
        self.ctx
    }
}
