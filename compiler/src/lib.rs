#![no_std]

extern crate alloc;

#[cfg(feature = "std")]
extern crate std;

use crate::semantic::{nameresolution, typecheck};

use alloc::string::String;
use codegen::CodegenBackend;
use context::CompilerContext;
use frostbite_parser::{
    lexer::{tokenize, TokenStream},
    Parser,
};
use frostbite_reports::sourcemap::{SourceDescription, SourceKey, SourceUrl};
use log::{debug, trace};

pub mod codegen;
pub mod context;
pub mod ir;
pub mod semantic;
pub mod utils;

#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct CompilerError;

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
    ) -> SourceKey
    {
        self.ctx.src_map.insert(SourceDescription {
            url: url.into(),
            source_code: source.into(),
        })
    }

    pub fn compile_module<C>(
        &mut self,
        src_key: SourceKey,
        codegen: &mut C,
    ) -> Result<C::Output, CompilerError>
    where
        C: CodegenBackend,
    {
        trace!("Lexing...");
        let token_stream = self.lex(src_key)?;

        trace!("Parsing...");
        self.parse(token_stream, src_key)?;

        trace!("Running semantic checks...");

        self.run_semantic_checks(src_key)?;

        let module_key = self.ctx.named_ctx.modules_by_src_keys[src_key];

        let codegen_output =
            self.codegen(self.ctx.named_ctx.modules[module_key].src_key, codegen)?;

        trace!("Done...");

        Ok(codegen_output)
    }

    pub fn run_semantic_checks(
        &mut self,
        source_key: SourceKey,
    ) -> Result<(), CompilerError>
    {
        nameresolution::check_names(self, source_key);
        self.ctx.has_errors_fallible_default()?;

        debug!(
            "Name resoluted & Import resoluted IR:\n{}",
            dbg_pls::color(&self.ctx.named_ctx.named_asts[source_key])
        );

        typecheck::check_types(self, source_key);
        self.ctx.has_errors_fallible_default()?;

        debug!(
            "Typed IR:\n{}",
            dbg_pls::color(&self.ctx.type_ctx.t_asts[source_key]),
        );

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

        self.ctx.asts.insert(source_key, ast);

        self.ctx.has_errors_fallible_default()?;

        Ok(())
    }

    fn lex(
        &mut self,
        source_key: SourceKey,
    ) -> Result<TokenStream, CompilerError>
    {
        let source = self.ctx.src_map[source_key].source_code.as_str();

        let token_stream = tokenize(&mut self.ctx.report_ctx, source_key, source);

        self.ctx.has_errors_fallible_default()?;

        Ok(token_stream)
    }

    fn codegen<C: CodegenBackend>(
        &mut self,
        main_source_key: SourceKey,
        codegen: &mut C,
    ) -> Result<C::Output, CompilerError>
    {
        let output = codegen.codegen(main_source_key, &mut self.ctx);

        self.ctx.has_errors_fallible_default()?;

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
