use core::ops::Range;

use alloc::format;
use frostbite_reports::{IntoReport, Label, Level, Report};

use crate::ast::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    UnrecognizedToken {
        location: Span,
        expected: &'static str,
    },
    UnrecognizedEof {
        expected: &'static [&'static str],
    },
    NumberTooBig {
        span: Span,
    },
}

impl IntoReport for Error {
    type Arguments = ();

    fn into_report(self, _: Self::Arguments) -> frostbite_reports::Report {
        match self {
            Error::UnrecognizedToken { location, expected } => Report::new(
                Level::Error,
                location,
                "Token in invalid position",
                None::<&str>,
                [Label::new(format!("Expected {expected}"), None::<Range<_>>)],
                [],
            ),
            Error::UnrecognizedEof { expected } => Report::new(
                Level::Error,
                Span::default(),
                "Unexpected EOF",
                None::<&str>,
                [],
                [Label::new(
                    format!("Expected one of: {}", expected.join(", ")),
                    None::<Range<_>>,
                )],
            ),
            Error::NumberTooBig { span } => Report::new(
                Level::Error,
                span,
                "Number is too big",
                Some("Number is too big to lex"),
                [Label::new(
                    const_format::formatcp!("Maximum limit is {}", i32::MAX),
                    None::<Range<_>>,
                )],
                [],
            ),
        }
    }
}
