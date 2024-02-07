use permafrost_compiler::{
    codegen::CodegenBackend, utils::CompilationResults, Compiler, CompilerError,
};

use permafrost_reports::sourcemap::{SourceKey, SourceUrl};

pub fn call_compiler<C>(
    compiler: &mut Compiler<'_>,
    src_url: impl Into<SourceUrl>,
    src_code: impl Into<String>,
    codegen: &mut C,
) -> Result<(SourceKey, CompilationResults<C>), CompilerError>
where
    C: CodegenBackend,
{
    let source_key = compiler.add_source(src_url, src_code)?;

    let compilation_results = compiler.compile(codegen)?;

    Ok((source_key, compilation_results))
}
