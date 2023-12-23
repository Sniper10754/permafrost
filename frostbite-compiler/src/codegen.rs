use frostbite_parser::ast::Expr;
use frostbite_reports::IntoReport;

mod bytecode;

pub use bytecode::BytecodeCodegenBackend;

pub trait CodegenBackend: Default {
    type Output;
    type Error: IntoReport;

    fn codegen(&mut self, expr: &Expr<'_>) -> Result<(), Self::Error>;

    fn finalize(self) -> Self::Output;
}

pub struct CodegenBackends;

impl CodegenBackends {
    pub fn bytecode_backend() -> BytecodeCodegenBackend {
        BytecodeCodegenBackend::default()
    }
}
