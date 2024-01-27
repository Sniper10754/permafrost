use core::ops::Range;

use alloc::format;
use frostbite_reports::{sourcemap::SourceKey, IntoReport, Label, Level, Report};

use crate::ast::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Error
{
    pub kind: ErrorKind,
    pub source_key: SourceKey,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind
{
    UnrecognizedToken
    {
        span: Span, expected: &'static str
    },
    UnrecognizedEof
    {
        expected: &'static [&'static str],
        previous_element_span: Span,
    },
    NumberTooBig
    {
        span: Span
    },
}

impl IntoReport for Error
{
    fn into_report(self) -> frostbite_reports::Report
    {
        let source_key = self.source_key;

        match self.kind {
            ErrorKind::UnrecognizedToken { span, expected } => Report::new(
                Level::Error,
                span,
                source_key,
                "Token in invalid position",
                None::<&str>,
                [Label::new(
                    format!("Expected {expected}"),
                    None::<Range<_>>,
                    source_key,
                )],
                [],
            ),
            ErrorKind::UnrecognizedEof {
                expected,
                previous_element_span,
            } => Report::new(
                Level::Error,
                previous_element_span,
                source_key,
                "Unexpected EOF",
                None::<&str>,
                [],
                [Label::new(
                    format!("Expected one of: {}", expected.join(", ")),
                    None::<Range<_>>,
                    source_key,
                )],
            ),
            ErrorKind::NumberTooBig { span } => Report::new(
                Level::Error,
                span,
                source_key,
                "Number is too big",
                Some("Number is too big to lex"),
                [Label::new(
                    const_format::formatcp!("Maximum limit is {}", i32::MAX),
                    None::<Range<_>>,
                    source_key,
                )],
                [],
            ),
        }
    }
}
