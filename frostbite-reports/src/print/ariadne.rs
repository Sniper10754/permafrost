use core::fmt::Debug;

use alloc::{boxed::Box, string::ToString};

use crate::{print, Level, Report};

use super::PrintBackend;

pub struct AriadnePrintBackend;

mod utils {
    use alloc::string::String;

    pub struct FmtWriteAsIoWrite<'a, W: std::fmt::Write + ?Sized> {
        pub destination: &'a mut W,
    }

    impl<'a, W: std::fmt::Write + ?Sized> std::io::Write for FmtWriteAsIoWrite<'a, W> {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            let uft8_string = String::from_utf8_lossy(buf);

            self.destination
                .write_str(&uft8_string)
                .map_err(|_| std::io::Error::new(std::io::ErrorKind::Other, ""))?;

            Ok(buf.len())
        }

        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }
}

impl PrintBackend for AriadnePrintBackend {
    fn write_report_to<W: core::fmt::Write + ?Sized>(
        destination: &mut W,
        source_id: Option<&str>,
        source: &str,
        report: &Report,
    ) -> Result<(), print::PrintingError> {
        let source_id = source_id.unwrap_or("Unknown");

        let report_kind = report.level.into();
        let mut report_builder =
            ariadne::Report::build(report_kind, source_id, report.location.start);

        report_builder.set_message(&report.title);
        report_builder.add_label(ariadne::Label::new((source_id, report.location.clone())));

        let report = report_builder.finish();

        let cache = ariadne::FnCache::new(|id: &&str| {
            if **id == *source_id {
                Ok(source.to_string())
            } else {
                Err(Box::new("Hello madafaka") as Box<dyn Debug>)
            }
        });

        let wrapper = utils::FmtWriteAsIoWrite { destination };

        report.write(cache, wrapper).unwrap();

        Ok(())
    }
}

impl From<Level> for ariadne::ReportKind<'static> {
    fn from(value: Level) -> Self {
        match value {
            Level::Error => ariadne::ReportKind::Error,
            Level::Warn => ariadne::ReportKind::Warning,
            Level::Info => ariadne::ReportKind::Advice,
        }
    }
}
