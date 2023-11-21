use alloc::{format, vec};
use frostbite_report_interface::{Help, Info, IntoReport, Level, Location, Report};

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

    fn into_report(self, _: Self::Arguments) -> frostbite_report_interface::Report {
        match self {
            Error::UnrecognizedToken { location, expected } => Report::new(
                Level::Error,
                Some(location),
                "Invalid Token",
                Some("Token in wrong place"),
                vec![Info::new(format!("Expected {expected}"), None::<Location>)],
                vec![],
            ),
            Error::UnrecognizedEof { expected } => Report::new(
                Level::Error,
                None::<Location>,
                "Unexpected EOF",
                None::<&str>,
                vec![],
                vec![Help::new(
                    format!("Expected one of: {}", expected.join(", ")),
                    None::<Location>,
                )],
            ),
            Error::NumberTooBig { span } => Report::new(
                Level::Error,
                Some(span),
                "Number is too big",
                Some("Number is too big to lex"),
                vec![Info::new(
                    const_format::formatcp!("Maximum limit is {}", i32::MAX),
                    None::<Location>,
                )],
                vec![],
            ),
        }
    }
}
