use permafrost_ast::Span;
use permafrost_reports::{sourcemap::SourceKey, IntoReport, Label, Level, Report};

use alloc::format;

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
    UnusedVisibilityModifier
    {
        span: Span
    },
}

impl IntoReport for Error
{
    fn into_report(self) -> Report
    {
        let source_key = self.source_key;

        match self.kind {
            ErrorKind::UnrecognizedToken { span, expected } => Report::new(
                Level::Error,
                span,
                source_key,
                "Token in invalid position",
                None::<&str>,
            )
            .with_label(Label::new(format!("Expected {expected}"), None, source_key)),
            ErrorKind::UnrecognizedEof {
                expected,
                previous_element_span,
            } => Report::new(
                Level::Error,
                previous_element_span,
                source_key,
                "Unexpected EOF",
                None::<&str>,
            )
            .with_label(Label::new(
                format!("Expected one of: {}", expected.join(", ")),
                None,
                source_key,
            )),
            ErrorKind::NumberTooBig { span } => Report::new(
                Level::Error,
                span,
                source_key,
                "Number is too big",
                Some("Number is too big to lex"),
            )
            .with_label(Label::new(
                const_format::formatcp!("Maximum limit is {}", i32::MAX),
                None,
                source_key,
            )),
            ErrorKind::UnusedVisibilityModifier { span } => Report::new(
                Level::Error,
                span,
                source_key,
                "Unused visibility modifier",
                Some("This visibility modifier precedes an item who's visibility cant be modified"),
            ),
        }
    }
}
