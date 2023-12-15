mod ariadne;

use alloc::string::ToString;
use core::fmt::{self, Display, Write};

use derive_more::*;

use crate::Report;

#[derive(Debug, Display, From)]
pub enum PrintingError {
    Fmt(core::fmt::Error),
}

pub struct ReportPrinter<'output, O: fmt::Write>(&'output mut O);

impl<'output, O: fmt::Write> ReportPrinter<'output, O> {
    pub fn new(output: &'output mut O) -> Self {
        Self(output)
    }

    /// # Errors
    /// May return error if the backend fails writing the report
    pub fn print<B: PrintBackend>(
        self,
        source_id: Option<impl Display + ToString>,
        source: impl AsRef<str>,
        report: &Report,
    ) -> Result<(), PrintingError> {
        B::write_report_to(
            self.0,
            source_id.map(|string| string.to_string()).as_deref(),
            source.as_ref(),
            report,
        )
    }
}

pub type DefaultPrintBackend = ariadne::AriadnePrintBackend;

pub trait PrintBackend {
    fn write_report_to<W: Write>(
        destination: &mut W,
        source_id: Option<&str>,
        source: &str,
        report: &Report,
    ) -> Result<(), PrintingError>;
}
