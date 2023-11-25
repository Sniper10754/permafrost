use core::fmt::Display;

use alloc::{fmt, string::ToString};
use derive_more::*;

use crate::{print_backend::PrintBackend, Report};

#[derive(Debug, Display, From)]
#[cfg_attr(feature = "std", derive(Error))]
pub enum PrintingError {
    #[cfg(feature = "std")]
    Io(std::io::Error),

    Fmt(core::fmt::Error),
}

#[derive(Debug, Default, Display)]
pub struct ReportPrinter;

impl ReportPrinter {
    pub fn new() -> Self {
        Self
    }

    /// # Errors
    /// May return error if the backend fails writing the report
    pub fn print<W: fmt::Write, B: PrintBackend>(
        self,
        destination: &mut W,
        source_id: Option<impl Display + ToString>,
        source: impl AsRef<str>,
        report: &Report,
    ) -> Result<(), PrintingError> {
        B::write_report_to(
            destination,
            source_id.map(|string| string.to_string()).as_deref(),
            source.as_ref(),
            report,
        )
    }
}
