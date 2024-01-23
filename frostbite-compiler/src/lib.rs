#![no_std]

extern crate alloc;

use crate::{modules::Module, semantic::typecheck};
use alloc::string::{String, ToString};
use codegen::CodegenBackend;
use context::CompilerContext;
use frostbite_parser::{
    lexer::{tokenize, TokenStream},
    Parser,
};
use frostbite_reports::sourcemap::{SourceDescription, SourceKey, SourceUrl};
use modules::ModuleKey;

pub mod codegen;
pub mod context;
pub mod ir;
pub mod modules;
pub mod semantic;
pub mod types;
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
    ) -> SourceKey
    {
        self.ctx.src_map.insert(SourceDescription {
            url: url.into(),
            source_code: source.into(),
        })
    }

    pub fn compile_module(
        &mut self,
        src_id: SourceKey,
    ) -> Result<ModuleKey, CompilerError>
    {
        log::trace!("Lexing...");
        let token_stream = Self::lex(&mut self.ctx, src_id)?;

        log::trace!("Parsing...");
        Self::parse(&mut self.ctx, token_stream, src_id)?;

        log::trace!("Running semantic checks...");

        self.run_semantic_checks(src_id)?;

        log::trace!("Done...");

        let name = self.ctx.src_map[src_id].url.to_string();

        let module_key = self.ctx.named_ctx.modules.insert(Module { name, src_id });

        self.ctx
            .named_ctx
            .modules_to_srcs
            .insert(module_key, src_id);

        Ok(module_key)
    }

    pub fn codegen_module<C: CodegenBackend>(
        &mut self,
        module_key: ModuleKey,
        codegen: &mut C,
    ) -> Result<CompilationResults<C>, CompilerError>
    {
        let module = &self.ctx.named_ctx.modules[module_key];
        let main_source_id = module.src_id;

        let codegen_output = Self::codegen(&mut self.ctx, main_source_id, codegen)?;

        Ok(CompilationResults { codegen_output })
    }

    pub fn run_semantic_checks(
        &mut self,
        source_id: SourceKey,
    ) -> Result<(), CompilerError>
    {
        typecheck::check_types(self, source_id);
        self.ctx.errors_as_result()?;

        log::debug!(
            "Typed Internal representation:\n{}",
            dbg_pls::color(&self.ctx.type_ctx.t_asts[source_id]),
        );

        Ok(())
    }

    fn parse(
        compiler_ctx: &mut CompilerContext,
        token_stream: TokenStream,
        source_id: SourceKey,
    ) -> Result<(), CompilerError>
    {
        let ast =
            Parser::with_tokenstream(&mut compiler_ctx.report_ctx, token_stream, source_id).parse();

        compiler_ctx.asts.insert(source_id, ast);

        compiler_ctx.errors_as_result()?;

        Ok(())
    }

    fn lex(
        compiler_ctx: &mut CompilerContext,
        source_id: SourceKey,
    ) -> Result<TokenStream, CompilerError>
    {
        let source = compiler_ctx.src_map[source_id].source_code.as_str();

        let token_stream = tokenize(&mut compiler_ctx.report_ctx, source_id, source);

        compiler_ctx.errors_as_result()?;

        Ok(token_stream)
    }

    fn codegen<C: CodegenBackend>(
        compiler_ctx: &mut CompilerContext,
        main_source_id: SourceKey,
        codegen: &mut C,
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
