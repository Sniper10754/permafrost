use std::borrow::Cow;

use const_format::formatcp;
use itertools::Itertools;
use lalrpop_util::ParseError;
use permafrost_ast::Span;
use permafrost_reports::{sourcemap::SourceKey, IntoReport, Label, Level, Location, Report};

use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub struct LexicalError
{
    pub source_key: SourceKey,
    pub kind: LexicalErrorKind,
}

#[derive(Debug, PartialEq, Clone, Default)]
pub enum LexicalErrorKind
{
    UnrecognizedEof
    {
        location: usize,

        expected: Vec<String>,
    },

    UnrecognizedToken
    {
        token: Token,
        span: Span,

        expected: Vec<String>,
    },

    ExtraToken
    {
        token: Token, span: Span
    },

    InvalidToken
    {
        location: usize
    },

    NumberTooBig
    {
        span: Span
    },

    UnknownToken
    {
        span: Span
    },

    #[default]
    GenericLexerError,
}

impl IntoReport for LexicalError
{
    fn into_report(self) -> permafrost_reports::Report
    {
        let source_key = self.source_key;

        let location;
        let title;
        let description: Cow<'static, _>;
        let mut expected_outer = None;

        match self.kind {
            LexicalErrorKind::UnrecognizedEof {
                location: column,
                expected,
            } => {
                location = Location::Column(column);
                expected_outer = Some(expected);
            }
            LexicalErrorKind::UnrecognizedToken {
                token,
                span,
                expected,
            } => {
                title = "Unrecognized Token";
                description = format!("Found token {}", token.description()).into();
                location = Location::Span(span);
                expected_outer = Some(expected);
            }
            LexicalErrorKind::ExtraToken { token, span } => {}
            LexicalErrorKind::InvalidToken { location: column } => {
                location = Location::Column(column);
                title = "Invalid token";
                description = "This token wasnt expected here".into();
            }
            LexicalErrorKind::NumberTooBig { span } => {
                location = Location::Span(span);
                title = "Number too big";
                description = "Number can't be lexed because is too big".into();
            }
            LexicalErrorKind::UnknownToken { span } => {
                location = Location::Span(span);
                title = "Unknown token";
                description = "Token was not recognized".into();
            }
            LexicalErrorKind::GenericLexerError => unreachable!(),
        }

        let mut report = Report::new(Level::Error, location, source_key, title, Some(description));

        if let Some(expected) = expected_outer {
            report = report.with_label(Label::new(
                Itertools::intersperse(expected.into_iter(), ", ".into()).collect::<String>(),
                None,
                source_key,
            ));
        }

        report
    }
}

impl From<ParseError<usize, Token, LexicalErrorKind>> for LexicalErrorKind
{
    fn from(value: ParseError<usize, Token, LexicalErrorKind>) -> Self
    {
        match value {
            ParseError::UnrecognizedEof { location, expected } => {
                Self::UnrecognizedEof { location, expected }
            }
            ParseError::UnrecognizedToken {
                token: (left, token, right),
                expected,
            } => Self::UnrecognizedToken {
                token,
                span: left..right,
                expected,
            },
            ParseError::ExtraToken {
                token: (left, token, right),
            } => Self::ExtraToken {
                token,
                span: left..right,
            },
            ParseError::User { error } => error,
            ParseError::InvalidToken { location } => unimplemented!(),
        }
    }
}
