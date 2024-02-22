#![no_std]

extern crate alloc;

#[cfg(feature = "std")]
extern crate std;

use alloc::{string::String, vec::Vec};

use derive_more::*;
use log::trace;
use slotmap::SecondaryMap;

use permafrost_parser::{
    lexer::{tokenize, TokenStream},
    Parser,
};
use permafrost_reports::sourcemap::{SourceDescription, SourceKey, SourceUrl};

use codegen::CodegenBackend;
use context::CompilerContext;
use semantic::{nameresolution, typecheck};
use utils::CompilationResult;

pub const PERMAFROST_FILE_EXTENSION: &str = "pmf";

pub mod codegen;
pub mod context;
pub mod ir;
pub mod semantic;
pub mod utils;

#[derive(Debug, Clone, Default, Copy, Constructor)]
pub struct CompilerError;

#[derive(Debug)]
pub struct Compiler<'ctx>
{
    ctx: &'ctx mut CompilerContext,
}

#[derive(Debug, Clone, Copy, Default, IsVariant)]
pub enum CompilerState
{
    #[default]
    Ready,
    Exhausted,
}

impl<'ctx> Compiler<'ctx>
{
    pub fn new(ctx: &'ctx mut CompilerContext) -> Self
    {
        Self { ctx }
    }

    pub fn __add_source(
        &mut self,
        url: impl Into<SourceUrl>,
        source: impl Into<String>,
    ) -> SourceKey
    {
        self.ctx.src_map.insert(SourceDescription {
            url: url.into(),
            source_code: source.into(),
        })
    }

    pub fn add_source(
        &mut self,
        url: impl Into<SourceUrl>,
        source: impl Into<String>,
    ) -> Result<SourceKey, CompilerError>
    {
        let src_key = self.__add_source(url, source);

        self.analyze_module(src_key)?;

        Ok(src_key)
    }

    fn analyze_module(
        &mut self,
        src_key: SourceKey,
    ) -> Result<(), CompilerError>
    {
        trace!("Lexing...");
        let token_stream = self.lex(src_key)?;

        trace!("Parsing...");
        self.parse(token_stream, src_key)?;

        trace!("Running semantic checks...");

        self.run_semantic_checks(src_key)?;

        trace!("Done...");

        Ok(())
    }

    pub fn run_semantic_checks(
        &mut self,
        source_key: SourceKey,
    ) -> Result<(), CompilerError>
    {
        nameresolution::check_names(self, source_key);
        self.errors_as_result()?;

        typecheck::check_types(self, source_key);
        self.errors_as_result()?;

        Ok(())
    }

    fn parse(
        &mut self,
        token_stream: TokenStream,
        source_key: SourceKey,
    ) -> Result<(), CompilerError>
    {
        let ast =
            Parser::with_tokenstream(&mut self.ctx.report_ctx, token_stream, source_key).parse();

        self.errors_as_result()?;

        self.ctx.asts.insert(source_key, ast);

        Ok(())
    }

    fn lex(
        &mut self,
        source_key: SourceKey,
    ) -> Result<TokenStream, CompilerError>
    {
        let source = self.ctx.src_map[source_key].source_code.as_str();

        let token_stream = tokenize(&mut self.ctx.report_ctx, source_key, source);

        self.errors_as_result()?;

        Ok(token_stream)
    }

    pub fn compile<C>(
        &mut self,
        codegen: &mut C,
    ) -> Result<CompilationResult<C>, CompilerError>
    where
        C: CodegenBackend,
    {
        let mut compiled_files = SecondaryMap::new();

        for source_key in self.ctx.src_map.keys().collect::<Vec<_>>() {
            self.analyze_module(source_key)?;

            let codegen_output = self.codegen(source_key, codegen)?;

            compiled_files.insert(source_key, codegen_output);
        }

        Ok(CompilationResult { compiled_files })
    }

    fn codegen<C>(
        &mut self,
        main_source_key: SourceKey,
        codegen: &mut C,
    ) -> Result<C::Output, CompilerError>
    where
        C: CodegenBackend,
    {
        let output = CodegenBackend::codegen(codegen, main_source_key, self.ctx);

        self.errors_as_result()?;

        Ok(output)
    }

    pub fn ctx(&self) -> &CompilerContext
    {
        self.ctx
    }

    pub fn ctx_mut(&mut self) -> &mut CompilerContext
    {
        self.ctx
    }

    fn errors_as_result(&self) -> Result<(), CompilerError>
    {
        self.ctx.has_errors_fallible(CompilerError)
    }
}
