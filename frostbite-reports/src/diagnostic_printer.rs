#[cfg(feature = "ariadne")]
mod ariadne;

use core::fmt::{self, Write};

use derive_more::*;

use crate::{sourcemap::SourceMap, Diagnostic};

#[cfg(feature = "ariadne")]
pub type DefaultPrintBackend = ariadne::AriadnePrintBackend;

#[cfg(not(any(feature = "ariadne")))]
compile_error!("No backend selected");

#[derive(Debug, Display, From)]
pub enum PrintingError {
    Fmt(core::fmt::Error),

    Other(&'static str),
}

pub struct DiagnosticPrinter<'output, O: fmt::Write>(&'output mut O);

impl<'output, O: fmt::Write> DiagnosticPrinter<'output, O> {
    pub fn new(output: &'output mut O) -> Self {
        Self(output)
    }

    /// # Errors
    /// May return error if the backend fails writing the report
    pub fn print<B: PrintBackend>(
        self,
        source_map: &SourceMap<'_, '_>,
        report: &Diagnostic<'_>,
    ) -> Result<(), PrintingError> {
        B::write_report_to(self.0, source_map, report)
    }
}

pub trait PrintBackend {
    fn write_report_to<'id, W: Write>(
        destination: &mut W,
        source_map: &SourceMap<'id, '_>,
        report: &Diagnostic<'id>,
    ) -> Result<(), PrintingError>;
}
