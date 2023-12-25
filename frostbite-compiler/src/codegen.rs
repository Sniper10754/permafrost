use frostbite_reports::ReportContext;

mod bytecode;

pub use bytecode::BytecodeCodegenBackend;

use crate::tir::TirTree;

pub trait CodegenBackend {
    type Output;

    fn codegen(self, reports: &mut ReportContext, program: &TirTree) -> Result<Self::Output, ()>;
}

pub struct CodegenBackends;

impl CodegenBackends {
    pub fn bytecode_backend() -> BytecodeCodegenBackend {
        BytecodeCodegenBackend
    }
}
