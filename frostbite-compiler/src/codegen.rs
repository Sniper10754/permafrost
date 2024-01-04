use frostbite_reports::ReportContext;

mod bytecode;

pub use bytecode::BytecodeCodegenBackend;

use crate::tir::TypedAst;

pub trait CodegenBackend {
    type Output;

    fn codegen(self, reports: &mut ReportContext, tree: &TypedAst) -> Self::Output;
}

pub struct CodegenBackends;

impl CodegenBackends {
    pub fn bytecode_backend() -> BytecodeCodegenBackend {
        BytecodeCodegenBackend::default()
    }
}
