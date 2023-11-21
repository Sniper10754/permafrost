#![no_std]

extern crate alloc;

pub mod ast;
pub mod error;
pub mod lexer;

use alloc::vec;
use alloc::vec::Vec;

use ast::{Expr, Program};
use error::Error;
use lexer::{Token, TokenStream};

macro_rules! consume_token {
    (parser: $parser:expr, token: $token:ident, description: $token_description:expr) => {
        match $parser.token_stream.next() {
            Some(pattern @ (_, Token::$token)) => Some(pattern),
            Some((span, _)) => {
                $parser.errors.push(Error::UnrecognizedToken {
                    location: span,
                    expected: $token_description,
                });

                None
            }
            None => {
                $parser.errors.push(Error::UnrecognizedEof {
                    expected: &[$token_description],
                });

                None
            }
        }
    };
}

/// A Backend-agnostic wrapper around lalrpop
pub struct Parser<'input> {
    token_stream: TokenStream<'input>,
    errors: Vec<Error>,
}

impl<'input> Parser<'input> {
    pub fn with_tokenstream(token_stream: TokenStream<'input>) -> Self {
        Self {
            token_stream,
            errors: vec![],
        }
    }

    pub fn parse(&mut self) -> Option<Program<'input>> {
        let exprs = vec![];

        Some(Program { exprs })
    }

    pub fn parse_expr(&mut self) -> Option<Expr<'input>> {
        match self.token_stream.next() {
            Some((span, Token::Int(value))) => Some(Expr::Int(span, value)),
            Some((span, Token::Float(value))) => Some(Expr::Float(span, value)),
            Some((span, Token::Ident(value))) => Some(Expr::Ident(span, value)),
            Some((span, Token::String(value))) => Some(Expr::String(span, value)),

            Some((_, Token::LParen)) => {
                let expr = self.parse_expr()?;

                consume_token!(
                    parser: self,
                    token: LParen,
                    description: ")"
                )?;

                Some(expr)
            }

            Some((span, _)) => {
                self.errors.push(Error::UnrecognizedToken {
                    location: span,
                    expected: "Expression",
                });

                None
            }

            None => {
                self.errors.push(Error::UnrecognizedEof {
                    expected: &["an expression"],
                });

                None
            } // Handle cases where the token doesn't match any expected pattern or there are no more tokens
        }
    }
}

#[cfg(test)]
mod tests {
    use alloc::vec;

    use crate::ast::{tokens::BinaryOperator, Expr, Program};

    extern crate std;

    macro_rules! parse_expr {
        ($text_to_parse:expr) => {
            crate::Parser::with_tokenstream(crate::lexer::tokenize($text_to_parse).unwrap()).parse()
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
            Some(Program {
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
