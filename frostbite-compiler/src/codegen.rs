use frostbite_reports::ReportContext;

mod bytecode;

pub use bytecode::BytecodeCodegenBackend;

use crate::tir::{TypedAst, TypesArena};

pub trait CodegenBackend
{
    type Output;

    fn codegen(
        self,
        reports: &mut ReportContext,
        tree: &TypedAst,
        types_arena: &TypesArena,
    ) -> Self::Output;
}

pub struct CodegenBackends;

impl CodegenBackends
{
    pub fn bytecode_backend() -> BytecodeCodegenBackend
    {
        BytecodeCodegenBackend::default()
    }
}
