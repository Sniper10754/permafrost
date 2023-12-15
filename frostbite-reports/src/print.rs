mod ariadne;

use alloc::string::ToString;
use core::fmt::{self, Display, Write};

use derive_more::*;

use crate::{
    sourcemap::{SourceId, SourceMap},
    Report,
};

#[derive(Debug, Display, From)]
pub enum PrintingError {
    Fmt(core::fmt::Error),

    Other(&'static str),
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
        report_source_id: &SourceId<'_>,
        sources: SourceMap<'_, '_>,
        report: &Report,
    ) -> Result<(), PrintingError> {
        B::write_report_to(self.0, report_source_id, sources, report)
    }
}

pub type DefaultPrintBackend = ariadne::AriadnePrintBackend;

pub trait PrintBackend {
    fn write_report_to<W: Write>(
        destination: &mut W,
        report_source_id: &SourceId<'_>,
        sources: SourceMap<'_, '_>,
        report: &Report,
    ) -> Result<(), PrintingError>;
}
