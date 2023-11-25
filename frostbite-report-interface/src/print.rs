use core::fmt::{self, Display};

use alloc::string::ToString;
use derive_more::*;

use crate::{print_backend::PrintBackend, Report};

#[derive(Debug, Display, From)]
#[cfg_attr(feature = "std", derive(Error))]
pub enum PrintingError {
    #[cfg(feature = "std")]
    Io(std::io::Error),

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
