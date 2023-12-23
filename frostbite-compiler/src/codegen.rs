use frostbite_parser::ast::Program;
use frostbite_reports::IntoReport;

mod bytecode;

pub use bytecode::BytecodeCodegenBackend;

pub trait CodegenBackend: Default {
    type Output;
    type Error: IntoReport;

    fn codegen(&mut self, expr: &Program<'_>) -> Result<Self::Output, Self::Error>;
}

pub struct CodegenBackends;

impl CodegenBackends {
    pub fn bytecode_backend() -> BytecodeCodegenBackend {
        BytecodeCodegenBackend::default()
    }
}
