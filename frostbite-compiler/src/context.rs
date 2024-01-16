use frostbite_parser::ast::Program;
use frostbite_reports::{
    sourcemap::{SourceId, SourceMap},
    ReportContext,
};
use slotmap::SecondaryMap;

use crate::{modules::ModuleContext, semantic::typecheck::TypeContext};

#[derive(Debug, Default)]
pub struct CompilerContext
{
    pub src_map: SourceMap,
    pub report_ctx: ReportContext,
    pub module_ctx: ModuleContext,
    pub type_ctx: TypeContext,

    pub asts: SecondaryMap<SourceId, Program>,
}

impl CompilerContext
{
    pub fn new(
        src_map: SourceMap,
        report_ctx: ReportContext,
    ) -> Self
    {
        Self {
            src_map,
            report_ctx,
            module_ctx: ModuleContext::default(),
            type_ctx: TypeContext::default(),

            asts: SecondaryMap::new(),
        }
    }

    // pub fn insert_intrinsic<F>(
    //     &mut self,
    //     name: impl Into<String>,
    //     f: F,
    // ) where
    //     F: Fn(&mut TypesArena) -> TypeKey,
    // {
    //     let value = f(&mut self.types_arena);
    // }

    pub fn has_errors(&self) -> bool
    {
        self.report_ctx.has_errors()
    }

    pub fn errors_as_result<E>(&self) -> Result<(), E>
    where
        E: Default,
    {
        // Equivalent of
        if self.has_errors() {
            Err(E::default())
        } else {
            Ok(())
        }
    }
}
