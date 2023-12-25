use core::convert::Infallible;

use alloc::{collections::BTreeMap, vec::Vec};
use frostbite_bytecode::{BytecodeVersion, Manifest, Module};
use frostbite_reports::ReportContext;

use crate::hir::HirTree;

use super::CodegenBackend;

#[derive(Debug, Default)]
pub struct BytecodeCodegenBackend;

impl BytecodeCodegenBackend {
    fn compile_program(&mut self, _module: &mut Module) {}
}

impl CodegenBackend for BytecodeCodegenBackend {
    type Output = Module;

    fn codegen(
        self,
        report_ctx: &mut ReportContext,
        _program: &HirTree,
    ) -> Result<Self::Output, ()> {
        let module = Module {
            manifest: Manifest {
                bytecode_version: BytecodeVersion::Number(0.1),
            },
            constants_pool: BTreeMap::new(),
            symbols_pool: BTreeMap::new(),
            functions: Vec::new(),
            instructions: Vec::new(),
        };

        Ok(module)
    }
}
