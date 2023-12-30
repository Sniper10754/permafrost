use frostbite_reports::ReportContext;

mod bytecode;

pub use bytecode::BytecodeCodegenBackend;

use crate::tir::TirTree;

#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct CodegenError;

pub trait CodegenBackend {
    type Output;

    fn codegen(
        self,
        reports: &mut ReportContext,
        tree: &TirTree,
    ) -> Result<Self::Output, CodegenError>;
}

pub struct CodegenBackends;

impl CodegenBackends {
    pub fn bytecode_backend() -> BytecodeCodegenBackend {
        BytecodeCodegenBackend::default()
    }
}
