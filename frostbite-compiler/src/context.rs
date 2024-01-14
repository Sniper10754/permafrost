use alloc::string::String;
use frostbite_bytecode::Module;
use frostbite_parser::ast::Program;
use frostbite_reports::{
    sourcemap::{SourceId, SourceMap},
    ReportContext,
};
use slotmap::{SecondaryMap, SlotMap};

use crate::{
    intrinsic::IntrinsicContext,
    modules::ModuleKey,
    tir::{Type, TypeKey, TypedAst, TypesArena},
};

#[derive(Debug, Default)]
pub struct CompilerContext
{
    pub src_map: SourceMap,
    pub report_ctx: ReportContext,
    pub intrinsic_ctx: IntrinsicContext,

    pub asts: SecondaryMap<SourceId, Program>,
    pub t_asts: SecondaryMap<SourceId, TypedAst>,
    pub types_arena: SlotMap<TypeKey, Type>,
    pub modules: SlotMap<ModuleKey, Module>,
    pub modules_to_srcs: SecondaryMap<SourceId, ModuleKey>,
}

impl CompilerContext
{
    pub fn new(
        src_map: SourceMap,
        report_ctx: ReportContext,
        intrinsic_ctx: IntrinsicContext,
    ) -> Self
    {
        Self {
            src_map,
            report_ctx,
            intrinsic_ctx,
            asts: SecondaryMap::new(),
            t_asts: SecondaryMap::new(),
            types_arena: SlotMap::default(),
            modules: SlotMap::default(),
            modules_to_srcs: SecondaryMap::default(),
        }
    }

    pub fn insert_intrinsic<F>(
        &mut self,
        name: impl Into<String>,
        f: F,
    ) where
        F: Fn(&mut TypesArena) -> TypeKey,
    {
        let value = f(&mut self.types_arena);

        self.intrinsic_ctx.symbols.insert(name.into(), value);
    }

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
