mod bytecode;

use alloc::{fmt, string::String, vec::Vec};
pub use bytecode::BytecodeCodegenBackend;
use permafrost_reports::sourcemap::SourceKey;

use crate::context::CompilerContext;

pub trait CodegenBackend
{
    type Output: CodegenOutput;

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
        BytecodeCodegenBackend::new()
    }
}

#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct SerializationError;

pub trait CodegenOutput: Sized
{
    fn serialize(
        &self,
        buf: &mut Vec<u8>,
    );

    fn deserialize(buf: &[u8]) -> Result<Self, SerializationError>;

    fn as_printable(&self) -> Option<&dyn PrintableCodegenOutput>;
}

pub trait PrintableCodegenOutput
{
    fn print(
        &self,
        buf: &mut String,
    ) -> fmt::Result;
}
