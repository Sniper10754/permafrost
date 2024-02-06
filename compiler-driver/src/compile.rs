use permafrost_compiler::{
    codegen::CodegenBackend, context::CompilerContext, utils::CompilationResults, Compiler,
};

use derive_more::*;
use permafrost_reports::sourcemap::{SourceKey, SourceUrl};

#[derive(Debug, From, Constructor)]
pub struct CompilingError
{
    pub context: CompilerContext,
    pub kind: CompilingErrorKind,
}

#[derive(Debug, Clone, Copy, From)]
pub enum CompilingErrorKind
{
    Semantic,
    Codegen,
}

pub fn call_compiler<C>(
    src_url: impl Into<SourceUrl>,
    src_code: impl Into<String>,
    codegen: &mut C,
) -> Result<(SourceKey, CompilationResults<C>), CompilingError>
where
    C: CodegenBackend,
{
    let mut compiler = Compiler::new();

    let Ok(source_key) = compiler.add_source(src_url, src_code) else {
        return Err(CompilingError::new(
            compiler.move_ctx(),
            CompilingErrorKind::Semantic,
        ));
    };

    let compilation_results = match compiler.compile(codegen) {
        Ok(compilation_results) => compilation_results,
        Err(compiler_ctx) => {
            return Err(CompilingError::new(
                compiler_ctx,
                CompilingErrorKind::Codegen,
            ));
        }
    };

    Ok((source_key, compilation_results))
}
