#![no_std]

extern crate alloc;

pub mod ast;
pub mod error;
pub mod lexer;

use alloc::vec::Vec;
use alloc::{boxed::Box, vec};

use ast::tokens::Operator;
use ast::{tokens, Expr, Program};
use error::Error;
use lexer::{Token, TokenStream};

mod utils {
    #[macro_export]
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
        let mut exprs = vec![];

        while self.token_stream.peek().is_some() {
            match self.parse_expr() {
                Some(expr) => {
                    exprs.push(expr);

                    consume_token!(
                        parser: self,
                        token: Semicolon,
                        description: "semicolon"
                    )?;
                }
                None => {
                    // recover

                    TokenStream::take_while(&mut self.token_stream, |(_, token)| {
                        matches!(token, Token::Semicolon)
                    });
                }
            }
        }

        Some(Program { exprs })
    }

    pub fn parse_expr(&mut self) -> Option<Expr<'input>> {
        let mut expression = self.parse_atom_expr()?;

        match self.token_stream.peek() {
            Some((op_span, Token::BinaryOperator(operator))) => {
                let operator = Operator(op_span.clone(), *operator);

                self.token_stream.skip_token();

                let rhs = self.parse_expr()?;

                expression = Expr::BinaryOperation {
                    lhs: Box::new(expression),
                    operator,
                    rhs: Box::new(rhs),
                }
            }

            Some((eq_span, Token::Eq)) => {
                let eq_token = tokens::Eq(eq_span.clone());

                self.token_stream.skip_token();

                let rhs = self.parse_expr()?;

                expression = Expr::Assign {
                    lhs: Box::new(expression),
                    eq_token,
                    value: Box::new(rhs),
                }
            }
            _ => (),
        }

        Some(expression)
    }

    fn parse_atom_expr(&mut self) -> Option<Expr<'input>> {
        match self.token_stream.next() {
            Some((span, Token::Int(value))) => Some(Expr::Int(span, value)),
            Some((span, Token::Float(value))) => Some(Expr::Float(span, value)),
            Some((span, Token::Ident(value))) => Some(Expr::Ident(span, value)),
            Some((span, Token::String(value))) => Some(Expr::String(span, value)),

            Some((_, Token::LParen)) => {
                let expr = self.parse_atom_expr()?;

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
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::dbg;

    use alloc::vec;

    use crate::ast::{
        tokens::{Operator, OperatorKind},
        Expr, Program,
    };

    extern crate std;

    macro_rules! parser {
        ($text_to_parse:expr) => {
            crate::Parser::with_tokenstream(crate::lexer::tokenize($text_to_parse).unwrap())
        };
    }

    macro_rules! boxed {
        ($value:expr) => {
            std::boxed::Box::new($value)
        };
    }

    #[test]
    fn test_parser_operation() {
        let mut parser = parser!("1 + 2 + 3;");

        assert_eq!(
            parser.parse(),
            Some(Program {
                exprs: vec![Expr::BinaryOperation {
                    lhs: boxed!(Expr::Int(0..1, 1)),
                    operator: Operator(2..3, OperatorKind::Add),
                    rhs: boxed!(Expr::BinaryOperation {
                        lhs: boxed!(Expr::Int(4..5, 2)),
                        operator: Operator(6..7, OperatorKind::Add),
                        rhs: boxed!(Expr::Int(8..9, 3))
                    }),
                }]
            })
        );

        assert!(parser.errors.is_empty());
    }
}
