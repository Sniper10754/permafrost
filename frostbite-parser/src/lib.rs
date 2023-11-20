#![no_std]

extern crate alloc;

pub mod ast;
pub mod error;

use ast::Program;
pub use frostbite_report_interface as report;

mod __internal {
    use lalrpop_util::lalrpop_mod;

    lalrpop_mod!(pub parser);
}

/// A Backend-agnostic wrapper around lalrpop
pub struct Parser(__internal::parser::ProgramParser);

impl<'input> Parser {
    pub fn new() -> Self {
        Self(__internal::parser::ProgramParser::new())
    }

    pub fn parse(&self, input: &'input str) -> Result<Program<'input>, error::Error> {
        self.0.parse(input).map_err(Into::into)
    }
}

#[cfg(test)]
mod tests {
    use alloc::vec;

    use crate::ast::{BinaryOperator, Expr, Program};

    extern crate std;

    macro_rules! parse_expr {
        ($text_to_parse:expr) => {
            super::Parser::new().parse($text_to_parse)
        };
    }

    macro_rules! boxed {
        ($value:expr) => {
            std::boxed::Box::new($value)
        };
    }

    #[test]
    fn test_parser_operation() {
        assert_eq!(
            parse_expr!("1 + 2 + 3"),
            Ok(Program {
                exprs: vec![Expr::BinaryOperation {
                    lhs: boxed!(Expr::Int(0..1, 1)),
                    operator: BinaryOperator::Add,
                    rhs: boxed!(Expr::BinaryOperation {
                        lhs: boxed!(Expr::Int(4..5, 2)),
                        operator: BinaryOperator::Add,
                        rhs: boxed!(Expr::Int(8..9, 3))
                    }),
                }]
            })
        );
    }
}
