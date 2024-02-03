#![no_std]

use frostbite_bytecode::Module;
use frostbite_compiler::{
    codegen::{BytecodeCodegenBackend, CodegenBackends},
    context::CompilerContext,
    Compiler,
};
use frostbite_reports::sourcemap::SourceUrl;
use frostbite_vm_core::{value::Value, Vm};

pub struct Runtime
{
    codegen: BytecodeCodegenBackend,
    vm: Vm,
}

impl Runtime
{
    pub fn new() -> Self
    {
        Self {
            codegen: CodegenBackends::bytecode_backend(),
            vm: Vm::new(),
        }
    }

    pub fn eval_code(
        &mut self,
        source_url: impl Into<SourceUrl>,
        code: &str,
    ) -> Result<Value, CompilerContext>
    {
        let bytecode_module = self.compile_code(source_url, code)?;

        let Module {
            manifest: _,
            globals: _,
            body,
        } = &bytecode_module;

        self.vm.execute(body, &bytecode_module);

        Ok(self.vm.stack.pop().unwrap_or(Value::Nil))
    }

    fn compile_code(
        &mut self,
        source_url: impl Into<SourceUrl>,
        code: &str,
    ) -> Result<Module, CompilerContext>
    {
        let mut compiler = Compiler::new();

        let source_key = compiler.add_source(source_url.into(), code);

        let result = compiler.compile(&mut self.codegen);

        result.map(|mut files| files.remove(source_key).unwrap())
    }
}

impl Default for Runtime
{
    fn default() -> Self
    {
        Self::new()
    }
}
