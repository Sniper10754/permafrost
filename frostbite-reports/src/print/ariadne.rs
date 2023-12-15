use std::fmt;

use alloc::string::ToString;

use crate::{
    print,
    sourcemap::{SourceId, SourceMap},
    Level, Report,
};

use super::PrintBackend;

pub struct AriadnePrintBackend;

mod utils {
    use std::{fmt, io, string::String};

    pub struct FmtWriteAsIoWrite<'a, W: fmt::Write + ?Sized> {
        pub destination: &'a mut W,
    }

    impl<'a, W: fmt::Write + ?Sized> io::Write for FmtWriteAsIoWrite<'a, W> {
        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            let uft8_string = String::from_utf8_lossy(buf);

            self.destination
                .write_str(&uft8_string)
                .map_err(|_| io::Error::new(io::ErrorKind::Other, ""))?;

            Ok(buf.len())
        }

        fn flush(&mut self) -> io::Result<()> {
            Ok(())
        }
    }
}

impl PrintBackend for AriadnePrintBackend {
    fn write_report_to<'id, W: fmt::Write + ?Sized>(
        destination: &mut W,
        report_source_id: SourceId<'id>,
        sources: &SourceMap<'id, '_>,
        report: &Report,
    ) -> Result<(), print::PrintingError> {
        let source_id = report_source_id;

        let report_kind = report.level.into();
        let mut report_builder =
            ariadne::Report::build(report_kind, source_id, report.location.start);

        report_builder.set_message(&report.title);
        report_builder.add_label(ariadne::Label::new((source_id, report.location.clone())));

        let report = report_builder.finish();

        let cache = ariadne::FnCache::new(|id: &SourceId<'_>| {
            if let Some((_, source)) = sources.iter().find(|(src_id, _)| *src_id == id) {
                Ok(source.to_string())
            } else {
                unreachable!()
            }
        });

        let mut wrapper = utils::FmtWriteAsIoWrite { destination };

        report.write(cache, &mut wrapper).unwrap();

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
