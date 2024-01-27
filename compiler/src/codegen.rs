mod bytecode;

pub use bytecode::BytecodeCodegenBackend;
use frostbite_reports::sourcemap::SourceKey;

use crate::context::CompilerContext;

pub trait CodegenBackend
{
    type Output;

    fn codegen(
        &mut self,
        source_key: SourceKey,
        compiler_ctx: &mut CompilerContext,
    ) -> Self::Output;
}

impl<T: CodegenBackend> CodegenBackend for &mut T
{
    type Output = T::Output;

    fn codegen(
        &mut self,
        source_key: SourceKey,
        compiler_ctx: &mut CompilerContext,
    ) -> Self::Output
    {
        T::codegen(self, source_key, compiler_ctx)
    }
}

pub struct CodegenBackends;

impl CodegenBackends
{
    pub fn bytecode_backend() -> BytecodeCodegenBackend
    {
        BytecodeCodegenBackend::default()
    }
}
