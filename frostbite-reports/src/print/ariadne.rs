use std::{dbg, fmt};

use crate::{
    print,
    sourcemap::{SourceId, SourceMap},
    Level, Report,
};

use self::utils::SourceMapCache;

use super::PrintBackend;

pub struct AriadnePrintBackend;

mod utils {
    use core::fmt::Debug;
    use std::{fmt, io, string::String};

    use alloc::{borrow::ToOwned, boxed::Box};
    use ariadne::{Cache, FnCache};

    use crate::sourcemap::{SourceId, SourceMap};

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

    #[allow(clippy::type_complexity)]
    pub struct SourceMapCache<'src_map, 'id> {
        fn_cache: FnCache<
            SourceId<'id>,
            Box<dyn Fn(&SourceId<'id>) -> Result<String, Box<dyn Debug>> + 'src_map>,
        >,
    }

    impl<'src_map, 'id, 'src> SourceMapCache<'src_map, 'id> {
        pub fn new(source_map: &'src_map SourceMap<'id, 'src>) -> Self {
            Self {
                fn_cache: FnCache::new(Box::new(|id: &_| {
                    let Some((_, source)) = source_map.iter().find(|(src_id, _)| *src_id == id)
                    else {
                        unreachable!()
                    };

                    Ok(source.to_owned())
                }) as Box<_>),
            }
        }
    }

    impl<'src_map, 'id> Cache<SourceId<'id>> for SourceMapCache<'src_map, 'id> {
        fn fetch(
            &mut self,
            id: &SourceId<'id>,
        ) -> Result<&ariadne::Source, Box<dyn fmt::Debug + '_>> {
            self.fn_cache.fetch(id)
        }

        fn display<'a>(&self, id: &'a SourceId<'id>) -> Option<Box<dyn fmt::Display + 'a>> {
            self.fn_cache.display(id)
        }
    }
}

impl PrintBackend for AriadnePrintBackend {
    fn write_report_to<'id, W: fmt::Write + ?Sized>(
        destination: &mut W,
        report_source_id: SourceId<'id>,
        source_map: &SourceMap<'id, '_>,
        report: &Report,
    ) -> Result<(), print::PrintingError> {
        let report_kind = report.level.into();
        let mut report_builder =
            ariadne::Report::build(report_kind, report_source_id, dbg!(&report.location).start)
                .with_message(&report.title);

        if let Some(desc) = &report.description {
            let mut report_description =
                ariadne::Label::new((report_source_id, report.location.clone())).with_order(1);

            report_description = report_description.with_message(desc);

            report_builder.add_label(report_description);
        }

        for label in Iterator::chain(report.infos.iter(), report.helps.iter()) {
            report_builder.add_label(
                ariadne::Label::new((
                    report_source_id,
                    label
                        .location
                        .clone()
                        .unwrap_or_else(|| report.location.clone()),
                ))
                .with_message(&label.info),
            );
        }

        report_builder
            .finish()
            .write(
                // Cache that interfaces to SourceMap
                SourceMapCache::new(source_map),
                // Adapter for
                utils::FmtWriteAsIoWrite { destination },
            )
            .unwrap();

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
