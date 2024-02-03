#![no_std]

use frostbite_compiler::{
    codegen::{BytecodeCodegenBackend, CodegenBackends},
    context::CompilerContext,
    Compiler,
};
use frostbite_reports::sourcemap::SourceUrl;
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
        source_url: impl Into<SourceUrl>,
        code: &str,
    ) -> Result<(), CompilerContext>
    {
        let mut compiler = Compiler::new();

        let source_key = compiler.add_source(source_url.into(), code);

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
