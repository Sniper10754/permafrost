use alloc::vec::Vec;
use frostbite_reports::IntoReport;

mod bytecode;

pub use bytecode::BytecodeCodegenBackend;

use crate::hir::Hir;

pub trait CodegenBackend {
    type Output;
    type Error: IntoReport;

    fn codegen(self, program: &Hir) -> Result<Self::Output, Vec<Self::Error>>;
}

pub struct CodegenBackends;

impl CodegenBackends {
    pub fn bytecode_backend() -> BytecodeCodegenBackend {
        BytecodeCodegenBackend
    }
}
