#[cfg(feature = "ariadne")]
mod ariadne;

use core::fmt::{self, Write};

use derive_more::*;

use crate::{sourcemap::SourceMap, Report};

#[cfg(feature = "ariadne")]
pub type DefaultPrintBackend = ariadne::AriadnePrintBackend;

#[cfg(not(any(feature = "ariadne")))]
compile_error!("No backend selected");

#[derive(Debug, Display, From)]
#[cfg_attr(feature = "std", derive(Error))]
pub enum PrintingError
{
    Fmt(core::fmt::Error),
}

pub struct ReportPrinter<'output, O: fmt::Write>(&'output mut O);

impl<'output, O: fmt::Write> ReportPrinter<'output, O>
{
    pub fn new(output: &'output mut O) -> Self
    {
        Self(output)
    }

    /// # Errors
    /// May return error if the backend fails writing the report
    pub fn print<B>(
        &mut self,
        source_map: &SourceMap,
        report: &Report,
    ) -> Result<(), PrintingError>
    where
        B: PrintBackend,
    {
        B::write_report_to(self.0, source_map, report)
    }

    pub fn print_reports<'a, B, I>(
        &mut self,
        source_map: &SourceMap,
        reports: I,
    ) -> Result<(), PrintingError>
    where
        B: PrintBackend,
        I: IntoIterator<Item = &'a Report>,
    {
        reports
            .into_iter()
            .try_for_each(|report| self.print::<B>(source_map, report))
    }
}

pub trait PrintBackend
{
    fn write_report_to<W: Write>(
        destination: &mut W,
        source_map: &SourceMap,
        report: &Report,
    ) -> Result<(), PrintingError>;
}
