use core::fmt;

use alloc::{format, string::ToString};

use crate::{sourcemap::SourceMap, utils::get_line_from_location, Backtrace};

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct BacktracePrinter<'a, W: fmt::Write> {
    use_colors: bool,

    output: &'a mut W,
}

impl<'a, W: fmt::Write> BacktracePrinter<'a, W> {
    pub fn new(output: &'a mut W) -> Self {
        BacktracePrinter {
            use_colors: true,

            output,
        }
    }

    pub fn print(&mut self, backtrace: &Backtrace, source_map: &SourceMap) -> fmt::Result {
        writeln!(self.output, "{}: {}", backtrace.reason, backtrace.message)?;

        for frame in &backtrace.frames {
            let line = match frame.position.as_ref() {
                Some(crate::Position::Span(span)) => Some(get_line_from_location(
                    source_map[frame.source_id].source_code.as_str(),
                    span.start,
                )),
                Some(crate::Position::Line(line)) => Some(*line),
                None => None,
            };

            let position = format!(
                "at {}:{}",
                source_map[frame.source_id].url,
                line.map_or("unknown".to_string(), |line| line.to_string())
            );

            writeln!(self.output, "\t{position:>}")?;
        }

        Ok(())
    }
}
