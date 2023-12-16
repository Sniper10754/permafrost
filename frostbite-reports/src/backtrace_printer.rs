use core::fmt;

use alloc::{format, string::ToString};

use crate::{sourcemap::SourceMap, utils::get_line_from_location, Backtrace};

#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct BacktracePrinter;

impl BacktracePrinter {
    pub fn new() -> Self {
        BacktracePrinter
    }

    pub fn print<W: fmt::Write>(
        self,
        output: &mut W,
        backtrace: &Backtrace<'_>,
        source_map: &SourceMap<'_, '_>,
    ) -> fmt::Result {
        writeln!(output, "{}: {}", backtrace.reason, backtrace.message)?;

        for frame in backtrace.frames.iter() {
            let line = match frame.position.as_ref() {
                Some(crate::Position::Span(span)) => Some(get_line_from_location(
                    &source_map[&frame.source_id],
                    span.start,
                )),
                Some(crate::Position::Line(line)) => Some(*line),
                None => None,
            };

            let position = format!(
                "at {}:{}",
                frame.source_id,
                line.map(|line| line.to_string())
                    .unwrap_or("unknown".to_string())
            );

            writeln!(output, "\t{:>}", position)?;
        }

        Ok(())
    }
}
