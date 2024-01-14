#![no_std]

extern crate alloc;

use alloc::string::String;
use codegen::CodegenBackend;
use context::CompilerContext;
use frostbite_parser::{
    lexer::{tokenize, TokenStream},
    Parser,
};
use frostbite_reports::sourcemap::{SourceDescription, SourceId, SourceUrl};
use semantic::run_semantic_checks;

pub mod codegen;
pub mod context;
pub mod intrinsic;
pub mod modules;
pub mod semantic;
pub mod tir;
pub mod utils;

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
        log::trace!("Lexing...");
        let token_stream = Self::lex(&mut self.ctx, source_id)?;

        log::trace!("Parsing...");
        // AST is stored in the compiler context
        Self::parse(&mut self.ctx, token_stream, source_id)?;

        // TAST is stored in the compiler context
        log::trace!("Running semantic checks...");
        run_semantic_checks(&mut self.ctx, source_id);

        {
            use core::fmt::Write;

            log::debug!(
                "Typed Internal representation:\n{}",
                tir::display::display_tree(&self.ctx.t_asts[source_id], &self.ctx.types_arena)
                    .split('\n')
                    .fold(String::new(), |mut acc, line| {
                        writeln!(acc, "| {line}").unwrap();

                        acc
                    }),
            );
        }
        self.ctx.errors_as_result()?;

        log::trace!("Codegenning...");
        let codegen_output = Self::codegen(&mut self.ctx, source_id, codegen)?;

        let compilation_results = CompilationResults { codegen_output };

        log::trace!("Done...");
        Ok(compilation_results)
    }

    fn lex(
        compiler_ctx: &mut CompilerContext,
        source_id: SourceId,
    ) -> Result<TokenStream, CompilerError>
    {
        let source = compiler_ctx.src_map[source_id].source_code.as_str();

        let token_stream = tokenize(&mut compiler_ctx.report_ctx, source_id, source);

        compiler_ctx.errors_as_result()?;

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

        compiler_ctx.errors_as_result()?;

        Ok(())
    }

    fn codegen<C: CodegenBackend>(
        compiler_ctx: &mut CompilerContext,
        main_source_id: SourceId,
        codegen: C,
    ) -> Result<C::Output, CompilerError>
    {
        let output = codegen.codegen(main_source_id, compiler_ctx);

        compiler_ctx.errors_as_result()?;

        Ok(output)
    }

    pub fn ctx(&self) -> &CompilerContext
    {
        &self.ctx
    }

    /// # Safety
    /// Doesnt cause ub, can break invariants
    #[allow(unsafe_code)]
    pub unsafe fn ctx_mut(&mut self) -> &mut CompilerContext
    {
        &mut self.ctx
    }

    pub fn move_ctx(self) -> CompilerContext
    {
        self.ctx
    }
}
