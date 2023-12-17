#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

use alloc::{boxed::Box, vec, vec::Vec};

pub mod ast;
pub mod error;
pub mod lexer;

use ast::{
    tokens,
    tokens::{Arrow, Eq, LeftParenthesisToken, Operator, RightParenthesisToken, TypeAnnotation},
    Argument, Expr, Program, Spanned,
};
use error::ErrorKind;
use frostbite_reports::sourcemap::SourceId;
use lexer::{Token, TokenStream};

use crate::{ast::tokens::FunctionToken, error::Error};

mod utils {
    #[macro_export]
    macro_rules! consume_token {
        (parser: $parser:expr, token: $token:pat, description: $token_description:expr) => {
            match $parser.token_stream.next() {
                Some(pattern @ Spanned(_, $token)) => Some(pattern),
                Some(Spanned(span, _)) => {
                    $parser.errors.push(error!(parser: $parser, ErrorKind::UnrecognizedToken {
                        location: span,
                        expected: $token_description,
                    }));

                    None
                }
                None => {
                    $parser.errors.push(error!(
                        parser: $parser,
                        ErrorKind::UnrecognizedEof {
                            expected: &[$token_description],
                            previous_element_span: $parser.token_stream.previous().unwrap().0.clone(),
                        }
                    ));

                    None
                }
            }
        };
    }

    #[macro_export]
    macro_rules! error {
        (parser: $parser:expr, $kind:expr) => {
            Error {
                kind: $kind,
                source_id: $parser.source_id,
            }
        };
    }
}

/// A Backend-agnostic wrapper around lalrpop
#[derive(Debug)]
pub struct Parser<'input, 'id> {
    token_stream: TokenStream<'input>,
    errors: Vec<Error<'id>>,
    source_id: SourceId<'id>,
}

impl<'input, 'id> Parser<'input, 'id> {
    pub fn with_tokenstream(token_stream: TokenStream<'input>, source_id: SourceId<'id>) -> Self {
        Self {
            token_stream,
            errors: vec![],
            source_id,
        }
    }

    pub fn parse(mut self) -> Result<Program<'input>, Vec<Error<'id>>> {
        let mut exprs = vec![];

        while self.token_stream.peek().is_some() {
            match self.parse_expr() {
                Some(expr) => {
                    exprs.push(expr);

                    if consume_token!(
                        parser: self,
                        token: Token::Semicolon,
                        description: "semicolon"
                    )
                    .is_none()
                    {
                        TokenStream::take_while(&mut self.token_stream, |Spanned(_, token)| {
                            matches!(token, Token::Semicolon)
                        });
                    }
                }
                None => {
                    // recover

                    TokenStream::take_while(&mut self.token_stream, |Spanned(_, token)| {
                        matches!(token, Token::Semicolon)
                    });
                }
            }
        }

        if self.errors.is_empty() {
            Ok(Program { exprs })
        } else {
            Err(self.errors)
        }
    }

    pub fn parse_expr(&mut self) -> Option<Expr<'input>> {
        let mut expression = self.parse_atom_expr()?;

        loop {
            match self.token_stream.peek() {
                Some(Spanned(op_span, Token::BinaryOperator(operator))) => {
                    let operator = Operator(op_span.clone(), *operator);

                    self.token_stream.skip_token();

                    let rhs = self.parse_expr()?;

                    expression = Expr::BinaryOperation {
                        lhs: Box::new(expression),
                        operator,
                        rhs: Box::new(rhs),
                    }
                }

                Some(Spanned(eq_span, Token::Eq)) => {
                    let eq_token = tokens::Eq(eq_span.clone());

                    self.token_stream.skip_token();

                    let rhs = self.parse_expr()?;

                    expression = Expr::Assign {
                        lhs: Box::new(expression),
                        eq_token,
                        value: Box::new(rhs),
                    }
                }

                Some(Spanned(_, Token::LParen)) => {
                    let lpt = consume_token!(
                        parser: self,
                        token: Token::LParen,
                        description: "Left pharentesis"
                    )?
                    .0
                    .into();

                    let mut arguments = vec![];
                    let rpt;

                    loop {
                        match self.token_stream.peek() {
                            Some(Spanned(_, Token::Comma)) => {
                                self.token_stream.skip_token();
                            }
                            Some(Spanned(_, Token::RParen)) => {
                                rpt = consume_token!(
                                    parser: self,
                                    token: Token::RParen,
                                    description: "Right pharentesis"
                                )?
                                .0
                                .into();

                                break;
                            }
                            Some(Spanned(_, _)) => {
                                arguments.push(self.parse_expr()?);
                            }
                            None => self.errors.push(
                                error!(parser: self, ErrorKind::UnrecognizedEof {
                                    expected: &["a comma", "an argument"],
                                    previous_element_span: self.token_stream.previous().unwrap().0.clone()
                                }),
                            ),
                        }
                    }

                    expression = Expr::Call {
                        callee: Box::new(expression),
                        lpt,
                        arguments,
                        rpt,
                    }
                }

                _ => break,
            }
        }

        Some(expression)
    }

    fn parse_atom_expr(&mut self) -> Option<Expr<'input>> {
        match self.token_stream.next() {
            Some(Spanned(span, Token::Int(value))) => Some(Expr::Int(span, value)),
            Some(Spanned(span, Token::Float(value))) => Some(Expr::Float(span, value)),
            Some(Spanned(span, Token::Ident(value))) => Some(Expr::Ident(span, value)),
            Some(Spanned(span, Token::String(value))) => Some(Expr::String(span, value)),

            Some(Spanned(_, Token::LParen)) => {
                let expr = self.parse_expr()?;

                consume_token!(
                    parser: self,
                    token: Token::RParen,
                    description: ")"
                )?;

                Some(expr)
            }

            Some(Spanned(fn_token_span, Token::Fn)) => {
                let fn_token = FunctionToken(fn_token_span);

                let name = consume_token!(
                    parser: self,
                    token: Token::Ident(_),
                    description: "Function Identifier"
                )?
                .map(|token| {
                    let Token::Ident(name) = token else {
                        unreachable!();
                    };

                    name
                });

                let Spanned(left_paren_span, _) = consume_token!(
                    parser: self,
                    token: Token::LParen,
                    description: "left parenthesis"
                )?;

                let mut arguments = vec![];

                let right_paren_span;

                loop {
                    match self.token_stream.next() {
                        Some(Spanned(name_span, Token::Ident(name))) => {
                            let name = Spanned(name_span, name);

                            let mut type_annotation =
                                Spanned(name.0.clone(), TypeAnnotation::NotSpecified);

                            if matches!(self.token_stream.peek(), Some(Spanned(_, Token::Colon))) {
                                consume_token!(
                                    parser: self,
                                    token: Token::Colon,
                                    description: "a colon"
                                )?;

                                type_annotation = self.parse_type_annotation()?;
                            }
                            arguments.push(Argument {
                                name,
                                type_annotation,
                            });
                        }
                        Some(Spanned(rps, Token::RParen)) => {
                            right_paren_span = rps;

                            break;
                        }
                        Some(Spanned(_, Token::Comma)) => continue,
                        Some(Spanned(span, _)) => {
                            self.errors
                                .push(error!(parser: self, ErrorKind::UnrecognizedToken {
                                    location: span,
                                    expected: "an identifier",
                                }));

                            return None;
                        }
                        None => {
                            self.errors
                                .push(error!(parser: self, ErrorKind::UnrecognizedEof {
                                    expected: &["an identifier"],
                                    previous_element_span: self.token_stream.previous().unwrap().0.clone()
                                }));

                            return None;
                        }
                    }
                }

                let (return_type_token, return_type_annotation) =
                    if let Some(Spanned(span, Token::Arrow)) = self.token_stream.peek().cloned() {
                        self.token_stream.skip_token();

                        let return_type_annotation = self.parse_type_annotation()?;

                        (Some(Arrow(span.clone())), Some(return_type_annotation))
                    } else {
                        (None, None)
                    };

                let Spanned(equals_span, _) = consume_token!(
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

            Some(Spanned(span, _)) => {
                self.errors
                    .push(error!(parser: self, ErrorKind::UnrecognizedToken {
                        location: span,
                        expected: "Expression",
                    }));

                None
            }

            None => {
                self.errors
                    .push(error!(parser: self, ErrorKind::UnrecognizedEof {
                        expected: &["an expression"],
                        previous_element_span: self.token_stream.previous().unwrap().0.clone()
                    }));

                None
            }
        }
    }

    fn parse_type_annotation(&mut self) -> Option<Spanned<TypeAnnotation<'input>>> {
        match self.token_stream.next() {
            Some(Spanned(span, Token::Ident("int"))) => Some(Spanned(span, TypeAnnotation::Int)),
            Some(Spanned(span, Token::Ident("float"))) => {
                Some(Spanned(span, TypeAnnotation::Float))
            }
            Some(Spanned(span, Token::Ident("str"))) => Some(Spanned(span, TypeAnnotation::String)),
            Some(Spanned(span, Token::Ident(other))) => {
                Some(Spanned(span, TypeAnnotation::Other(other)))
            }
            Some(Spanned(span, _)) => {
                self.errors
                    .push(error!(parser: self, ErrorKind::UnrecognizedToken {
                        location: span,
                        expected: "A type",

                    }));

                None
            }
            None => {
                self.errors
                    .push(error!(parser: self, ErrorKind::UnrecognizedEof {
                        expected: &["type annotation"],
                        previous_element_span: self.token_stream.previous().unwrap().0.clone(),
                    }));

                None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    #[allow(unused_extern_crates)]
    extern crate std;

    use std::{boxed::Box, vec};

    use crate::ast::{
        tokens::{
            Arrow, Eq, FunctionToken, LeftParenthesisToken, Operator, OperatorKind,
            RightParenthesisToken, TypeAnnotation,
        },
        Argument, Expr, Program, Spanned,
    };

    macro_rules! parser {
        ($text_to_parse:expr) => {
            crate::Parser::with_tokenstream(
                crate::lexer::tokenize($text_to_parse)
                    .expect(alloc::format!("Failed to tokenize `{}`", { $text_to_parse }).as_str()),
                frostbite_reports::sourcemap::SourceId::Undefined,
            )
        };
    }

    macro_rules! boxed {
        ($value:expr) => {
            Box::new($value)
        };
    }

    #[test]
    fn test_parser_operation() {
        let parser = parser!("1 + 2 + 3;");

        assert_eq!(
            parser.parse(),
            Ok(Program {
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
    }

    #[test]
    fn test_parser_assign() {
        let parser = parser!("a = 1;");

        assert_eq!(
            parser.parse(),
            Ok(Program {
                exprs: vec![Expr::Assign {
                    lhs: boxed!(Expr::Ident(0..1, "a")),
                    eq_token: Eq(2..3),
                    value: boxed!(Expr::Int(4..5, 1)),
                }]
            })
        );
    }

    #[test]
    fn test_parser_function() {
        let parser = parser!("function test(x: int, y: int) = x + y;");

        assert_eq!(
            parser.parse(),
            Ok(Program {
                exprs: vec![Expr::Function {
                    fn_token: FunctionToken(0..8),
                    name: Spanned(9..13, "test"),
                    lpt: LeftParenthesisToken(13..14),
                    arguments: vec![
                        Argument {
                            name: Spanned(14..15, "x"),
                            type_annotation: Spanned(17..20, TypeAnnotation::Int),
                        },
                        Argument {
                            name: Spanned(22..23, "y"),
                            type_annotation: Spanned(25..28, TypeAnnotation::Int),
                        },
                    ],
                    rpt: RightParenthesisToken(28..29),
                    return_type_token: None,
                    return_type_annotation: None,
                    equals: Eq(30..31),
                    body: boxed!(Expr::BinaryOperation {
                        lhs: boxed!(Expr::Ident(32..33, "x")),
                        operator: Operator(34..35, OperatorKind::Add),
                        rhs: boxed!(Expr::Ident(36..37, "y"))
                    }),
                }]
            })
        );
    }

    #[test]
    fn test_parser_function_with_return() {
        let parser = parser!("function test(x: int, y: int) -> int = x + y;");

        assert_eq!(
            parser.parse(),
            Ok(Program {
                exprs: vec![Expr::Function {
                    fn_token: FunctionToken(0..8),
                    name: Spanned(9..13, "test"),
                    lpt: LeftParenthesisToken(13..14),
                    arguments: vec![
                        Argument {
                            name: Spanned(14..15, "x"),
                            type_annotation: Spanned(17..20, TypeAnnotation::Int),
                        },
                        Argument {
                            name: Spanned(22..23, "y"),
                            type_annotation: Spanned(25..28, TypeAnnotation::Int),
                        },
                    ],
                    rpt: RightParenthesisToken(28..29),
                    return_type_token: Some(Arrow(30..32)),
                    return_type_annotation: Some(Spanned(33..36, TypeAnnotation::Int)),
                    equals: Eq(37..38),
                    body: boxed!(Expr::BinaryOperation {
                        lhs: boxed!(Expr::Ident(39..40, "x")),
                        operator: Operator(41..42, OperatorKind::Add),
                        rhs: boxed!(Expr::Ident(43..44, "y"))
                    }),
                }]
            })
        );
    }

    #[test]
    fn test_parser_call() {
        let mut parser = parser!("bilo(a)");

        let ast = parser.parse_expr();

        assert_eq!(
            ast,
            Some(Expr::Call {
                callee: Box::new(Expr::Ident(0..4, "bilo")),
                lpt: LeftParenthesisToken(4..5),
                arguments: vec![Expr::Ident(5..6, "a")],
                rpt: RightParenthesisToken(6..7)
            })
        );
    }
}
