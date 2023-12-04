mod custom;

use core::fmt::Write;

use crate::{print::PrintingError, print_backend::custom::CustomPrintBackend, Report};

pub type DefaultPrintBackend = CustomPrintBackend;

pub trait PrintBackend {
    fn write_report_to<W: Write>(
        destination: &mut W,
        source_id: Option<&str>,
        source: &str,
        report: &Report,
    ) -> Result<(), PrintingError>;
}
