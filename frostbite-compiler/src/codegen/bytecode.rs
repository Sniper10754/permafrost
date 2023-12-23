use core::convert::Infallible;

use alloc::vec::Vec;
use frostbite_parser::ast::Expr;

use super::CodegenBackend;

type Buffer = Vec<u8>;

#[derive(Debug, Default)]
pub struct BytecodeCodegenBackend(Buffer);

impl CodegenBackend for BytecodeCodegenBackend {
    type Output = Buffer;
    type Error = Infallible;

    fn codegen(&mut self, _expr: &Expr<'_>) -> Result<(), Self::Error> {
        match expr {
            
        }

        Ok(())
    }

    fn finalize(self) -> Self::Output {
        self.0
    }
}
