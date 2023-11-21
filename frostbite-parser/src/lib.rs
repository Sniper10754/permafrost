#![no_std]

extern crate alloc;

pub mod ast;
pub mod error;
pub mod lexer;

use ast::{Expr, Program};
pub use frostbite_report_interface as report;
use lexer::TokenStream;

/// A Backend-agnostic wrapper around lalrpop
pub struct Parser<'input> {
    token_stream: TokenStream<'input>,
}

impl<'input> Parser<'input> {
    pub fn with_tokenstream(token_stream: TokenStream<'input>) -> Self {
        Self { token_stream }
    }

    pub fn parse(&mut self) -> Option<Program<'input>> {
        todo!()
    }

    pub fn parse_expr(&mut self) -> Option<Expr<'input>> {
        match self.token_stream.next() {
            Some(_) => todo!(),
            None => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use alloc::vec;

    use crate::ast::{BinaryOperator, Expr, Program};

    extern crate std;

    macro_rules! parse_expr {
        ($text_to_parse:expr) => {
            super::Parser::new($text_to_parse).parse()
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
