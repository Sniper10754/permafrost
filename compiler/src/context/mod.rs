use alloc::vec::Vec;
use delegate::delegate;
use permafrost_ast::Program;
use permafrost_reports::{
    sourcemap::{SourceKey, SourceMap},
    ReportContext,
};
use slotmap::SecondaryMap;

use crate::ir::typed::{Type, TypeKey, TypedAst};

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
    pub files_to_compile: Vec<SourceKey>,
    pub asts: SecondaryMap<SourceKey, Program>,
}

impl CompilerContext
{
    delegate! {
        to self.type_ctx {
            pub fn get_ast(
                &self,
                source_key: SourceKey,
            ) -> &TypedAst;

            pub fn get_ast_mut(
                &mut self,
                source_key: SourceKey,
            ) -> &mut TypedAst;

            pub fn get_type(
                &self,
                type_key: TypeKey,
            ) -> &Type;

            pub fn get_type_mut(
                &mut self,
                type_key: TypeKey,
            ) -> &mut Type;

            pub fn insert_type(
                &mut self,
                ty: Type,
            ) -> TypeKey;

            pub fn insert_ast(
                &mut self,
                source_key: SourceKey,
                ast: TypedAst,
            );
        }
        to self.named_ctx {

        }
    }

    pub fn new() -> Self
    {
        Self::default()
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

    pub fn clear(&mut self)
    {
        self.asts.clear();
        self.files_to_compile.clear();
        self.named_ctx.clear();
        self.report_ctx.clear();
        self.src_map.clear();
        self.type_ctx.clear();
    }
}
