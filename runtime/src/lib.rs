#![no_std]

use permafrost_bytecode::Module;
use permafrost_compiler::{
    codegen::{BytecodeCodegenBackend, CodegenBackends},
    context::CompilerContext,
    Compiler, CompilerError,
};
use permafrost_reports::sourcemap::SourceUrl;
use permafrost_vm_core::{value::Value, Vm};

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
        ctx: &mut CompilerContext,
        source_url: impl Into<SourceUrl>,
        code: &str,
    ) -> Result<Value, CompilerError>
    {
        let mut compiler = Compiler::new(ctx);

        let bytecode = self.compile_code(&mut compiler, source_url, code)?;

        let Module {
            manifest: _,
            globals: _,
            ref body,
        } = bytecode;

        self.vm.execute(body, &bytecode);

        Ok(self.vm.stack().last().cloned().unwrap_or(Value::Nil))
    }

    fn compile_code(
        &mut self,
        compiler: &mut Compiler<'_>,
        source_url: impl Into<SourceUrl>,
        code: &str,
    ) -> Result<Module, CompilerError>
    {
        let source_key = compiler.add_source(source_url.into(), code)?;

        let result = compiler.compile(&mut self.codegen);

        result.map(|mut files| files.retrieve(source_key).unwrap())
    }
}

impl Default for Runtime
{
    fn default() -> Self
    {
        Self::new()
    }
}
