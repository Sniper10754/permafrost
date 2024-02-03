#![no_std]

extern crate alloc;

use alloc::vec::Vec;
use frostbite_compiler::{
    codegen::{BytecodeCodegenBackend, CodegenBackends},
    context::CompilerContext,
    Compiler,
};
use frostbite_reports::{sourcemap::SourceUrl, Report};
use frostbite_vm_core::VM;

pub struct Runtime
{
    codegen: BytecodeCodegenBackend,
    vm: VM,
}

impl Runtime
{
    pub fn new() -> Self
    {
        Self {
            codegen: CodegenBackends::bytecode_backend(),
            vm: VM::new(),
        }
    }

    pub fn eval_code(
        &mut self,
        code: &str,
    ) -> Result<(), CompilerContext>
    {
        let mut compiler = Compiler::new();
        let source_key = compiler.add_source(SourceUrl::Sparse("()".into()), code);

        let result = compiler.compile(&mut self.codegen);

        let bytecode_module = result.map(|mut files| files.remove(source_key).unwrap())?;

        self.vm.execute(&bytecode_module.body, &bytecode_module);

        Ok(())
    }
}

impl Default for Runtime
{
    fn default() -> Self
    {
        Self::new()
    }
}
