use core::ops::Range;

use alloc::format;
use frostbite_reports::{sourcemap::SourceId, IntoReport, Label, Level, Report};

use crate::ast::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Error<'id> {
    pub kind: ErrorKind,
    pub source_id: SourceId<'id>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    UnrecognizedToken {
        span: Span,
        expected: &'static str,
    },
    UnrecognizedEof {
        expected: &'static [&'static str],
        previous_element_span: Span,
    },
    NumberTooBig {
        span: Span,
    },
}

impl<'id> IntoReport<'id> for Error<'id> {
    type Arguments = ();

    fn into_report(self, _: Self::Arguments) -> frostbite_reports::Report<'id> {
        let source_id = self.source_id;

        match self.kind {
            ErrorKind::UnrecognizedToken { span, expected } => Report::new_diagnostic(
                Level::Error,
                span,
                source_id,
                "Token in invalid position",
                None::<&str>,
                [Label::new(
                    format!("Expected {expected}"),
                    None::<Range<_>>,
                    source_id,
                )],
                [],
            ),
            ErrorKind::UnrecognizedEof {
                expected,
                previous_element_span,
            } => Report::new_diagnostic(
                Level::Error,
                previous_element_span,
                source_id,
                "Unexpected EOF",
                None::<&str>,
                [],
                [Label::new(
                    format!("Expected one of: {}", expected.join(", ")),
                    None::<Range<_>>,
                    source_id,
                )],
            ),
            ErrorKind::NumberTooBig { span } => Report::new_diagnostic(
                Level::Error,
                span,
                source_id,
                "Number is too big",
                Some("Number is too big to lex"),
                [Label::new(
                    const_format::formatcp!("Maximum limit is {}", i32::MAX),
                    None::<Range<_>>,
                    source_id,
                )],
                [],
            ),
        }
    }
}
