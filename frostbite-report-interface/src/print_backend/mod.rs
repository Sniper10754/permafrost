mod custom;

use crate::{print::PrintingError, print_backend::custom::CustomBackend, Report};

use core::fmt::Write;

pub type DefaultBackend = CustomBackend;

pub trait PrintBackend {
    fn write_report_to<W: Write>(
        destination: &mut W,
        source_id: Option<&str>,
        source: &str,
        report: &Report,
    ) -> Result<(), PrintingError>;
}
