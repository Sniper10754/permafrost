extern crate std;

use std::fmt;

use self::utils::SourceMapCache;
use crate::{printer, printer::PrintBackend, sourcemap::SourceMap, Level, Report};

pub struct AriadnePrintBackend;

mod utils
{
    use super::std;

    use std::{borrow::ToOwned, boxed::Box, fmt, fmt::Debug, io, string::String};

    use ariadne::{Cache, FnCache, Source};

    use crate::sourcemap::{SourceKey, SourceMap};

    pub struct FmtToIoBridge<'a, W: fmt::Write + ?Sized>
    {
        pub destination: &'a mut W,
    }

    impl<'a, W: fmt::Write + ?Sized> io::Write for FmtToIoBridge<'a, W>
    {
        fn write(
            &mut self,
            buf: &[u8],
        ) -> io::Result<usize>
        {
            let utf8_string = String::from_utf8_lossy(buf);

            self.destination
                .write_str(&utf8_string)
                .map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;

            Ok(buf.len())
        }

        // Flushing is a no-op since every write is flushed immidiately
        fn flush(&mut self) -> io::Result<()>
        {
            Ok(())
        }
    }

    #[allow(clippy::type_complexity)]
    pub struct SourceMapCache<'src_map>
    {
        fn_cache: FnCache<
            SourceKey,
            Box<dyn Fn(&SourceKey) -> Result<String, Box<dyn Debug>> + 'src_map>,
            String,
        >,
        src_map: &'src_map SourceMap,
    }

    impl<'src_map> SourceMapCache<'src_map>
    {
        pub fn new(src_map: &'src_map SourceMap) -> Self
        {
            Self {
                fn_cache: FnCache::new(Box::new(|id: &_| Ok(src_map[*id].source_code.to_owned()))),
                src_map,
            }
        }
    }

    impl<'src_map> Cache<SourceKey> for SourceMapCache<'src_map>
    {
        type Storage = String;

        fn fetch(
            &mut self,
            id: &SourceKey,
        ) -> Result<&Source<Self::Storage>, Box<dyn fmt::Debug + '_>>
        {
            self.fn_cache.fetch(id)
        }

        fn display<'a>(
            &self,
            id: &'a SourceKey,
        ) -> Option<Box<dyn fmt::Display + 'a>>
        {
            self.src_map
                .get(*id)
                .map(|source_description| source_description.url.clone())
                .map(Box::new)
                .map(|boxed| boxed as Box<_>)
        }
    }
}

impl PrintBackend for AriadnePrintBackend
{
    fn write_report_to<'id, W: fmt::Write + ?Sized>(
        destination: &mut W,
        source_map: &SourceMap,
        report: &Report,
    ) -> Result<(), printer::PrintingError>
    {
        let Report {
            level,
            span,
            source_key,
            title,
            description,
            infos,
        } = report;

        let report_source_key = *source_key;

        let report_kind = (*level).into();
        let mut report_builder =
            ariadne::Report::build(report_kind, report_source_key, span.start).with_message(title);

        let mut report_description =
            ariadne::Label::new((report_source_key, span.clone())).with_order(1);

        report_description = report_description.with_message(
            description
                .as_ref()
                .map(|string| string.as_ref())
                .unwrap_or("Here"),
        );

        report_builder.add_label(report_description);

        for label in infos {
            report_builder.add_label(
                ariadne::Label::new((
                    report_source_key,
                    label.span.clone().unwrap_or_else(|| span.clone()),
                ))
                .with_message(&label.info),
            );
        }

        report_builder
            .finish()
            .write(
                // Cache that interfaces to SourceMap
                SourceMapCache::new(source_map),
                // Bridge
                utils::FmtToIoBridge { destination },
            )
            .unwrap();

        Ok(())
    }
}

impl From<Level> for ariadne::ReportKind<'static>
{
    fn from(value: Level) -> Self
    {
        match value {
            Level::Error => ariadne::ReportKind::Error,
            Level::Warn => ariadne::ReportKind::Warning,
            Level::Advice => ariadne::ReportKind::Advice,
        }
    }
}
