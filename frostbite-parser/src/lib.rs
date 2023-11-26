#![no_std]

extern crate alloc;

pub mod ast;
pub mod error;
pub mod lexer;

use alloc::vec::Vec;
use alloc::{boxed::Box, vec};

use ast::tokens::{
    Arrow, Eq, LeftParenthesisToken, Operator, RightParenthesisToken, TypeAnnotation,
};
use ast::{tokens, Argument, Expr, Program, Spanned};
use error::Error;
use lexer::{Token, TokenStream};

use crate::ast::tokens::FunctionToken;

mod utils {
    #[macro_export]
    macro_rules! consume_token {
        (parser: $parser:expr, token: $token:pat, description: $token_description:expr) => {
            match $parser.token_stream.next() {
                Some(pattern @ (_, $token)) => Some(pattern),
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
                        token: Token::Semicolon,
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
                    token: Token::LParen,
                    description: ")"
                )?;

                Some(expr)
            }

            Some((fn_token_span, Token::Fn)) => {
                let fn_token = FunctionToken(fn_token_span);

                let name = Spanned::from(consume_token!(
                    parser: self,
                    token: Token::Ident(_),
                    description: "Function Identifier"
                )?)
                .map(|token| {
                    let Token::Ident(name) = token else {
                        unreachable!();
                    };

                    name
                });

                let (left_paren_span, _) = consume_token!(
                    parser: self,
                    token: Token::LParen,
                    description: "left parenthesis"
                )?;

                let mut arguments = vec![];

                let right_paren_span;

                loop {
                    match self.token_stream.next() {
                        Some((name_span, Token::Ident(name))) => {
                            let name = Spanned(name_span, name);

                            consume_token!(
                                parser: self,
                                token: Token::Colon,
                                description: "a colon"
                            )?;

                            let type_annotation = self.parse_type_annotation()?;

                            arguments.push(Argument {
                                name,
                                type_annotation,
                            });
                        }
                        Some((rps, Token::RParen)) => {
                            right_paren_span = rps;

                            break;
                        }
                        Some((_, Token::Comma)) => continue,
                        Some((span, _)) => {
                            self.errors.push(Error::UnrecognizedToken {
                                location: span,
                                expected: "an identifier",
                            });

                            return None;
                        }
                        None => {
                            self.errors.push(Error::UnrecognizedEof {
                                expected: &["an identifier"],
                            });

                            return None;
                        }
                    }
                }

                let (return_type_token, return_type_annotation) =
                    if let Some((span, Token::Arrow)) = self.token_stream.peek().cloned() {
                        self.token_stream.next();

                        let return_type_annotation = self.parse_type_annotation()?;

                        (Some(Arrow(span.clone())), Some(return_type_annotation))
                    } else {
                        (None, None)
                    };

                let (equals_span, _) = consume_token!(
                    parser: self,
                    token: Token::Eq,
                    description: "equals sign"
                )?;

                let body = self.parse_expr()?;

                Some(Expr::Function {
                    fn_token,
                    name,
                    lpt: LeftParenthesisToken(left_paren_span),
                    arguments,
                    rpt: RightParenthesisToken(right_paren_span),
                    return_type_token,
                    return_type_annotation,
                    equals: Eq(equals_span),
                    body: Box::new(body),
                })
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

    fn parse_type_annotation(&mut self) -> Option<Spanned<TypeAnnotation<'input>>> {
        match self.token_stream.next() {
            Some((span, Token::Ident("int"))) => Some(Spanned(span, TypeAnnotation::Int)),
            Some((span, Token::Ident("float"))) => Some(Spanned(span, TypeAnnotation::Float)),
            Some((span, Token::Ident("str"))) => Some(Spanned(span, TypeAnnotation::String)),
            Some((span, Token::Ident(other))) => Some(Spanned(span, TypeAnnotation::Other(other))),
            Some((span, _)) => {
                self.errors.push(Error::UnrecognizedToken {
                    location: span,
                    expected: "A type",
                });

                None
            }
            None => {
                self.errors.push(Error::UnrecognizedEof {
                    expected: &["type annotation"],
                });

                None
            }
        }
    }

    pub fn errors(&self) -> &[Error] {
        self.errors.as_ref()
    }
}

#[cfg(test)]
mod tests {

    use alloc::vec;

    use crate::ast::{
        tokens::{
            Arrow, Eq, FunctionToken, LeftParenthesisToken, Operator, OperatorKind,
            RightParenthesisToken, TypeAnnotation,
        },
        Argument, Expr, Program, Spanned,
    };

    extern crate std;

    macro_rules! parser {
        ($text_to_parse:expr) => {
            crate::Parser::with_tokenstream(
                crate::lexer::tokenize($text_to_parse)
                    .expect(alloc::format!("Failed to tokenize `{}`", { $text_to_parse }).as_str()),
            )
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

    #[test]
    fn test_parser_assign() {
        let mut parser = parser!("a = 1;");

        assert_eq!(
            parser.parse(),
            Some(Program {
                exprs: vec![Expr::Assign {
                    lhs: boxed!(Expr::Ident(0..1, "a")),
                    eq_token: Eq(2..3),
                    value: boxed!(Expr::Int(4..5, 1)),
                }]
            })
        );

        assert!(parser.errors.is_empty());
    }

    #[test]
    fn test_parser_function() {
        let mut parser = parser!("function test(x: int, y: int) = x + y;");

        assert_eq!(
            parser.parse(),
            Some(Program {
                exprs: vec![Expr::Function {
                    fn_token: FunctionToken(0..8),
                    name: Spanned(9..13, "test"),
                    lpt: LeftParenthesisToken(13..14),
                    arguments: vec![
                        Argument {
                            name: Spanned(14..15, "x"),
                            type_annotation: Spanned(17..20, TypeAnnotation::Int)
                        },
                        Argument {
                            name: Spanned(22..23, "y"),
                            type_annotation: Spanned(25..28, TypeAnnotation::Int)
                        }
                    ],
                    rpt: RightParenthesisToken(28..29),
                    return_type_token: None,
                    return_type_annotation: None,
                    equals: Eq(30..31),
                    body: boxed!(Expr::BinaryOperation {
                        lhs: boxed!(Expr::Ident(32..33, "x")),
                        operator: Operator(34..35, OperatorKind::Add),
                        rhs: boxed!(Expr::Ident(36..37, "y"))
                    })
                }]
            })
        );

        assert!(parser.errors().is_empty());
    }

    #[test]
    fn test_parser_function_with_return() {
        let mut parser = parser!("function test(x: int, y: int) -> int = x + y;");

        assert_eq!(
            parser.parse(),
            Some(Program {
                exprs: vec![Expr::Function {
                    fn_token: FunctionToken(0..8),
                    name: Spanned(9..13, "test"),
                    lpt: LeftParenthesisToken(13..14),
                    arguments: vec![
                        Argument {
                            name: Spanned(14..15, "x"),
                            type_annotation: Spanned(17..20, TypeAnnotation::Int)
                        },
                        Argument {
                            name: Spanned(22..23, "y"),
                            type_annotation: Spanned(25..28, TypeAnnotation::Int)
                        }
                    ],
                    rpt: RightParenthesisToken(28..29),
                    return_type_token: Some(Arrow(30..32)),
                    return_type_annotation: Some(Spanned(33..36, TypeAnnotation::Int)),
                    equals: Eq(30..31),
                    body: boxed!(Expr::BinaryOperation {
                        lhs: boxed!(Expr::Ident(32..33, "x")),
                        operator: Operator(34..35, OperatorKind::Add),
                        rhs: boxed!(Expr::Ident(36..37, "y"))
                    })
                }]
            })
        );

        assert!(parser.errors().is_empty());
    }
}
