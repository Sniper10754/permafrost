use frostbite_parser::ast::Program;
use frostbite_reports::{
    sourcemap::{SourceKey, SourceMap},
    ReportContext,
};
use slotmap::SecondaryMap;

pub use self::{names::NamedContext, types::TypeContext};

pub mod names;
pub mod types;

#[derive(Debug, Default)]
pub struct CompilerContext
{
    pub src_map: SourceMap,
    pub report_ctx: ReportContext,
    pub named_ctx: NamedContext,
    pub type_ctx: TypeContext,

    pub asts: SecondaryMap<SourceKey, Program>,
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

            ..Default::default()
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

    pub fn has_errors_fallible_default<E>(&self) -> Result<(), E>
    where
        E: Default,
    {
        self.has_errors_fallible_lazy(|| E::default())
    }

    pub fn has_errors_fallible<E>(
        &self,
        error: E,
    ) -> Result<(), E>
    {
        self.has_errors_fallible_lazy(|| error)
    }

    pub fn has_errors_fallible_lazy<F, E>(
        &self,
        err: F,
    ) -> Result<(), E>
    where
        F: FnOnce() -> E,
    {
        if self.has_errors() {
            Err(err())
        } else {
            Ok(())
        }
    }
}
