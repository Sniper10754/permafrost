#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

pub mod error;
pub mod lexer;

use alloc::{boxed::Box, vec, vec::Vec};
use core::ops::Range;

use permafrost_ast::{
    tokens::{
        ArrowToken, Eq, FunctionToken, LeftBraceToken, LeftParenthesisToken, Operator, ReturnToken,
        RightBraceToken, RightParenthesisToken, TypeAnnotation,
    },
    Argument, Expr, ItemVisibility, NamespaceDirectiveKind, OwnedNamespacePath, Program, Spannable,
    Spanned,
};
use permafrost_reports::{sourcemap::SourceKey, ReportContext};

use error::ErrorKind;
use lexer::{Token, TokenStream};

use crate::error::Error;

mod utils
{
    #[macro_export]
    macro_rules! consume_token {
        (parser: $parser:expr, token: $token:pat, description: $token_description:expr) => {

            match $parser.token_stream.next() {
                Some(pattern @ Spanned(_, $token)) => Some(pattern),
                Some(Spanned(span, _)) => {
                    $parser.report_ctx.push(report!(parser: $parser, ErrorKind::UnrecognizedToken {
                        span,
                        expected: $token_description,
                    }));
                    None
                }
                None => {
                    $parser.report_ctx.push(report!(
                        parser: $parser,
                        ErrorKind::UnrecognizedEof {
                            expected: &[$token_description],
                            previous_element_span: $parser.token_stream.previous().unwrap().span().clone(),
                        }
                    ));
                    None
                }
            }

        };
    }

    #[macro_export]
    macro_rules! report {
        (parser: $parser:expr, $kind:expr) => {{
            use permafrost_reports::IntoReport as _;

            Error {
                kind: $kind,
                source_key: $parser.source_key,
            }
            .into_report()
        }};
    }
}

/// A Backend-agnostic wrapper around lalrpop
#[derive(Debug)]
pub struct Parser<'report_context>
{
    token_stream: TokenStream,
    report_ctx: &'report_context mut ReportContext,
    source_key: SourceKey,
}

impl<'report_context> Parser<'report_context>
{
    #[must_use]
    pub fn with_tokenstream(
        report_ctx: &'report_context mut ReportContext,
        token_stream: TokenStream,
        source_key: SourceKey,
    ) -> Self
    {
        Self {
            token_stream,
            report_ctx,
            source_key,
        }
    }

    pub fn parse(mut self) -> Program
    {
        let mut exprs = vec![];

        while self.token_stream.peek().is_some() {
            match self.parse_expr() {
                Some(expr) => {
                    if !matches!(expr, Expr::Block { .. })
                        && consume_token!(
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

                    exprs.push(expr);
                }
                None => {
                    // recover

                    TokenStream::take_while(&mut self.token_stream, |Spanned(_, token)| {
                        matches!(token, Token::Semicolon)
                    });
                }
            }
        }

        Program { exprs }
    }

    pub fn parse_expr(&mut self) -> Option<Expr>
    {
        let mut expression = self.parse_atom_expr()?;

        loop {
            match self.token_stream.peek() {
                Some(Spanned(op_span, Token::BinaryOperator(operator))) => {
                    let operator = Operator {
                        span: op_span.clone(),
                        kind: *operator,
                    };

                    self.token_stream.skip_token();

                    let rhs = self.parse_expr()?;

                    expression = Expr::BinaryOperation {
                        lhs: Box::new(expression),
                        operator,
                        rhs: Box::new(rhs),
                    }
                }
                Some(Spanned(eq_span, Token::Eq)) => {
                    let eq_token = Eq(eq_span.clone());

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
                    .span()
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
                                .span()
                                .into();

                                break;
                            }
                            Some(Spanned(_, _)) => {
                                arguments.push(self.parse_expr()?);
                            }
                            None => self.report_ctx.push(
                                report!(parser: self, ErrorKind::UnrecognizedEof {
                                    expected: &["a comma", "an argument"],
                                    previous_element_span: self.token_stream.previous().unwrap().span().clone()
                                }),
                            ),
                        }
                    }

                    expression = Expr::Call {
                        callee: Box::new(expression),
                        left_paren: lpt,
                        arguments,
                        right_paren: rpt,
                    }
                }

                _ => break,
            }
        }

        Some(expression)
    }

    fn parse_atom_expr(&mut self) -> Option<Expr>
    {
        match self.token_stream.next() {
            Some(Spanned(span, Token::Int(value))) => Some(Expr::Int(Spanned(span, value))),
            Some(Spanned(span, Token::Float(value))) => Some(Expr::Float(Spanned(span, value))),
            Some(Spanned(span, Token::Ident(value))) => Some(Expr::Ident(Spanned(span, value))),
            Some(Spanned(span, Token::String(value))) => Some(Expr::String(Spanned(span, value))),

            Some(Spanned(Range { start, end: _ }, Token::Use)) => {
                let Spanned(Range { start: _, end }, namespace_path) =
                    self.parse_namespace_path()?;

                Some(Expr::ImportDirective {
                    namespace_path: Spanned(start..end, namespace_path),
                })
            }

            Some(Spanned(mod_token_span, Token::Module)) => {
                let Spanned(name_span, Token::Ident(module_name)) = consume_token!(
                    parser: self,
                    token: Token::Ident(_),
                    description: "Identifier"
                )?
                else {
                    unreachable!()
                };

                Some(Expr::NamespaceDirective(
                    mod_token_span.into(),
                    NamespaceDirectiveKind::ImportLocalNamespace(Spanned(name_span, module_name)),
                ))
            }

            Some(Spanned(span, Token::True)) => Some(Expr::Bool(Spanned(span, true))),
            Some(Spanned(span, Token::False)) => Some(Expr::Bool(Spanned(span, false))),

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
                let visibility = self.parse_visibility()?;

                let fn_token = FunctionToken(fn_token_span);

                let mut name = None;

                if let Some(Spanned(_, Token::Ident(_))) = self.token_stream.peek() {
                    name = Some(
                        consume_token!(
                            parser: self,
                            token: Token::Ident(..),
                            description: "identifier"
                        )?
                        .map(|token| {
                            if let Token::Ident(ident) = token {
                                ident
                            } else {
                                unreachable!()
                            }
                        }),
                    );
                }

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
                                Spanned(name.span().clone(), TypeAnnotation::Any);

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
                            self.report_ctx.push(
                                report!(parser: self, ErrorKind::UnrecognizedToken {
                                    span,
                                    expected: "an identifier",
                                }),
                            );

                            return None;
                        }
                        None => {
                            self.report_ctx
                                .push(report!(parser: self, ErrorKind::UnrecognizedEof {
                                    expected: &["an identifier"],
                                    previous_element_span: self.token_stream.previous().unwrap().span().clone()
                                }));

                            return None;
                        }
                    }
                }

                let (return_type_token, return_type_annotation) =
                    if let Some(Spanned(span, Token::Arrow)) = self.token_stream.peek().cloned() {
                        self.token_stream.skip_token();

                        let return_type_annotation = self.parse_type_annotation()?;

                        (Some(ArrowToken(span.clone())), Some(return_type_annotation))
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
                    visibility,

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

            Some(Spanned(left_brace_span, Token::LBrace)) => {
                let mut expressions = vec![];

                loop {
                    match self.token_stream.peek() {
                        Some(Spanned(_, Token::RBrace)) => break,
                        Some(_) => match self.parse_expr() {
                            Some(expr) => {
                                expressions.push(expr);

                                consume_token!(
                                    parser: self,
                                    token: Token::Semicolon,
                                    description: "Semicolon"
                                )?;
                            }
                            None => {
                                TokenStream::take_while(
                                    &mut self.token_stream,
                                    |Spanned(_, token)| {
                                        !matches!(token, Token::Semicolon | Token::RBrace)
                                    },
                                );

                                return None;
                            }
                        },
                        None => self.report_ctx.push(report!(
                            parser: self,
                            ErrorKind::UnrecognizedEof {
                                expected: &["An expression", "`}`"],
                                previous_element_span: self.token_stream.previous().unwrap().span()
                            }
                        )),
                    }
                }
                let Spanned(right_brace_span, _) = consume_token!(
                    parser: self,
                    token: Token::RBrace,
                    description: "right brace"
                )?;

                Some(Expr::Block {
                    left_brace: LeftBraceToken(left_brace_span),
                    expressions,
                    right_brace: RightBraceToken(right_brace_span),
                })
            }

            Some(Spanned(span, Token::Return)) => {
                let mut value = None;

                if !matches!(self.token_stream.peek(), Some(Spanned(_, Token::Semicolon))) {
                    value = Some(self.parse_expr()?);
                }

                Some(Expr::Return(ReturnToken(span), value.map(Box::new)))
            }

            Some(Spanned(span, _)) => {
                self.report_ctx
                    .push(report!(parser: self, ErrorKind::UnrecognizedToken {
                        span,
                        expected: "Expression",
                    }));

                None
            }

            None => {
                self.report_ctx
                    .push(report!(parser: self, ErrorKind::UnrecognizedEof {
                        expected: &["an expression"],
                        previous_element_span: self.token_stream.previous().unwrap().span().clone()
                    }));

                None
            }
        }
    }

    fn parse_type_annotation(&mut self) -> Option<Spanned<TypeAnnotation>>
    {
        match self.token_stream.next() {
            Some(Spanned(span, Token::Ident(ident))) if ident == "int" => {
                Some(Spanned(span, TypeAnnotation::Int))
            }
            Some(Spanned(span, Token::Ident(ident))) if ident == "float" => {
                Some(Spanned(span, TypeAnnotation::Float))
            }
            Some(Spanned(span, Token::Ident(ident))) if ident == "str" => {
                Some(Spanned(span, TypeAnnotation::String))
            }
            Some(Spanned(span, Token::Ident(ident))) if ident == "bool" => {
                Some(Spanned(span, TypeAnnotation::Bool))
            }
            Some(Spanned(span, Token::Ident(ident))) if ident == "any" => {
                Some(Spanned(span, TypeAnnotation::Any))
            }
            Some(Spanned(span, Token::Ident(ident))) if ident == "unit" => {
                Some(Spanned(span, TypeAnnotation::Unit))
            }
            Some(Spanned(span, Token::Ident(other))) => {
                Some(Spanned(span, TypeAnnotation::Object(other)))
            }

            Some(Spanned(span, _)) => {
                self.report_ctx.push(report!(
                    parser: self,
                    ErrorKind::UnrecognizedToken {
                        span,
                        expected: "A type",
                    }
                ));

                None
            }
            None => {
                self.report_ctx.push(report!(
                    parser: self,
                    ErrorKind::UnrecognizedEof {
                        expected: &["type annotation"],
                        previous_element_span: self.token_stream.previous().unwrap().span().clone(),
                    }
                ));

                None
            }
        }
    }

    fn parse_namespace_path(&mut self) -> Option<Spanned<OwnedNamespacePath>>
    {
        let mut names = Vec::new();

        let name = consume_token!(
            parser: self,
            token: Token::Ident(..),
            description: "Module name"
        )?;

        let Range {
            start: span_start,
            end: mut span_end,
        } = name.span();

        names.push(name);

        loop {
            if matches!(
                self.token_stream.peek(),
                Some(Spanned(_, Token::DoubleColon))
            ) {
                self.token_stream.skip_token();

                let name = consume_token!(
                    parser: self,
                    token: Token::Ident(..),
                    description: "Module name"
                )?;

                span_end = name.span().end;

                names.push(name);
            } else {
                break;
            }
        }

        Some(Spanned(
            span_start..span_end,
            names
                .into_iter()
                .map(|spanned_name| {
                    spanned_name.map(|name| {
                        let Token::Ident(name) = name else {
                            unreachable!()
                        };

                        name
                    })
                })
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        ))
    }

    fn parse_visibility(&mut self) -> Option<ItemVisibility>
    {
        match self.token_stream.peek() {
            Some(Spanned(_, Token::Public)) => {
                let Spanned(span, _) = self.token_stream.skip_token()?;

                Some(ItemVisibility::Public(span.into()))
            }
            Some(_) | None => Some(ItemVisibility::Unspecified),
        }
    }
}
