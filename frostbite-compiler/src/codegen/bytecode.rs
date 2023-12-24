use core::convert::Infallible;

use alloc::{collections::BTreeMap, vec::Vec};
use frostbite_bytecode::{BytecodeVersion, Manifest, Module};

use crate::hir::Hir;

use super::CodegenBackend;

#[derive(Debug, Default)]
pub struct BytecodeCodegenBackend;

impl BytecodeCodegenBackend {
    fn compile_program(&mut self, module: &mut Module) {}
}

impl CodegenBackend for BytecodeCodegenBackend {
    type Output = Module;
    type Error = Infallible;

    fn codegen(self, program: &Hir) -> Result<Self::Output, Vec<Self::Error>> {
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
