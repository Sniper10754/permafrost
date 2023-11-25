use alloc::fmt;
use derive_more::*;

use crate::{
    print_backend::{DefaultBackend, PrintBackend},
    Report,
};

#[derive(Debug, From)]
#[cfg_attr(std, derive(Error))]
pub enum PrintError {
    #[cfg(std)]
    Io(std::io::Error),
    Fmt(core::fmt::Error),
}

#[derive(Debug, Default, Display)]
pub struct ReportPrinter;

impl ReportPrinter {
    pub fn new() -> Self {
        Self
    }

    pub fn print<W: fmt::Write, B: PrintBackend>(
        self,
        destination: &mut W,
        source_id: Option<impl AsRef<str>>,
        source: impl AsRef<str>,
        report: &Report,
    ) -> Result<(), PrintError> {
        B::write_report_to(
            destination,
            source_id.as_ref().map(|string| string.as_ref()),
            source.as_ref(),
            report,
        )
    }
}
