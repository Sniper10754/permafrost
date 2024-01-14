mod bytecode;

pub use bytecode::BytecodeCodegenBackend;
use frostbite_reports::sourcemap::SourceId;

use crate::context::CompilerContext;

pub trait CodegenBackend
{
    type Output;

    fn codegen(
        self,
        source_id: SourceId,
        compiler_ctx: &mut CompilerContext,
    ) -> Self::Output;
}

pub struct CodegenBackends;

impl CodegenBackends
{
    pub fn bytecode_backend() -> BytecodeCodegenBackend
    {
        BytecodeCodegenBackend::default()
    }
}
