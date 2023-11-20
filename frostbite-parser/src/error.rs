use alloc::{string::String, vec::Vec};
use lalrpop_util::ParseError;

use crate::ast::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    InvalidToken {
        location: usize,
    },
    UnrecognizedEof {
        location: usize,
        expected: Vec<String>,
    },
    UnrecognizedToken {
        token: Span,
        expected: Vec<String>,
    },
    ExtraToken {
        token: Span,
    },
    NumberTooBig {
        span: Span,
    },
}

impl<L: Into<usize>, T> From<ParseError<L, T, Self>> for Error {
    fn from(value: ParseError<L, T, Self>) -> Self {
        match value {
            ParseError::InvalidToken { location } => Error::InvalidToken {
                location: location.into(),
            },
            ParseError::UnrecognizedEof { location, expected } => Error::UnrecognizedEof {
                location: location.into(),
                expected,
            },
            ParseError::UnrecognizedToken { token, expected } => Error::UnrecognizedToken {
                token: (token.0.into())..(token.2.into()),
                expected,
            },
            ParseError::ExtraToken { token } => Error::ExtraToken {
                token: (token.0.into())..(token.2.into()),
            },
            ParseError::User { error } => error,
        }
    }
}
