mod custom;

use core::fmt::Write;

use alloc::fmt;

use crate::{print::PrintError, Report};

use self::custom::CustomBackend;

pub type DefaultBackend = CustomBackend;

pub trait PrintBackend {
    fn write_report_to<W: Write>(
        destination: &mut W,
        source_id: Option<&str>,
        source: &str,
        report: &Report,
    ) -> Result<(), PrintError>;
}
