use alloc::{borrow::Cow, vec, vec::Vec};
use frostbite_report_interface::{Info, IntoReport, Level, Location, Report};

use crate::ast::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    InvalidToken { location: Span },
    UnrecognizedEof { expected: &'static [&'static str] },
    NumberTooBig { span: Span },
}

impl IntoReport for Error {
    type Arguments = ();

    fn into_report(self, arguments: Self::Arguments) -> frostbite_report_interface::Report {
        match self {
            Error::InvalidToken { location } => Report::new(
                Level::Error,
                Some(location),
                "Invalid Token",
                Some("Detected an invalid token"),
                vec![],
                vec![],
            ),
            Error::UnrecognizedEof { expected } => Report::new(
                Level::Error,
                None::<Location>,
                "Unexpected EOF",
                None::<&str>,
                vec![],
                vec![],
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
