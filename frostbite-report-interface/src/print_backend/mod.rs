mod custom;

use core::fmt::Write;

use crate::Report;

use self::custom::CustomBackend;

pub trait PrintBackend {
    type Error;

    fn write_report_to(
        destination: &mut dyn Write,
        source_id: Option<&str>,
        source: &str,
        report: &Report,
    ) -> Result<(), Self::Error>;
}

pub fn default_backend() -> impl PrintBackend {
    CustomBackend
}
