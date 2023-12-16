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

impl<'id> IntoReport<'id> for Error<'id> {
    type Arguments = ();

    fn into_report(self, _: Self::Arguments) -> frostbite_reports::Report<'id> {
        match self.kind {
            ErrorKind::UnrecognizedToken { location, expected } => Report::new_diagnostic(
                Level::Error,
                location,
                "Token in invalid position",
                None::<&str>,
                [Label::new(
                    format!("Expected {expected}"),
                    None::<Range<_>>,
                    self.source_id,
                )],
                [],
            ),
            ErrorKind::UnrecognizedEof { expected } => Report::new_diagnostic(
                Level::Error,
                Span::default(),
                "Unexpected EOF",
                None::<&str>,
                [],
                [Label::new(
                    format!("Expected one of: {}", expected.join(", ")),
                    None::<Range<_>>,
                    self.source_id,
                )],
            ),
            ErrorKind::NumberTooBig { span } => Report::new_diagnostic(
                Level::Error,
                span,
                "Number is too big",
                Some("Number is too big to lex"),
                [Label::new(
                    const_format::formatcp!("Maximum limit is {}", i32::MAX),
                    None::<Range<_>>,
                    self.source_id,
                )],
                [],
            ),
        }
    }
}
