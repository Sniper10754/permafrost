use core::fmt;

use crate::utils::get_line_from_location;

use super::PrintBackend;

pub struct CustomBackend;

fn write_line_from_line_number(
    destination: &mut dyn fmt::Write,
    line: usize,
    source: &str,
) -> Result<(), fmt::Error> {
    let line = get_line_from_location(source, line);
    let source = source.lines().nth(line - 1).unwrap();

    writeln!(destination, "{line:<4} | {source}")?;

    Ok(())
}

impl PrintBackend for CustomBackend {
    type Error = fmt::Error;

    fn write_report_to(
        destination: &mut dyn fmt::Write,
        source_id: Option<&str>,
        source: &str,
        report: &crate::Report,
    ) -> Result<(), Self::Error> {
        writeln!(destination, "{}: {}", report.level, report.title)?;

        if let Some(location) = &report.location {
            writeln!(
                destination,
                "\t---> {}:{}:{}",
                source_id.unwrap_or("Unknown"),
                get_line_from_location(source, location.start()),
                location.start()
            )?;

            write_line_from_line_number(destination, location.start(), source)?;
        }

        writeln!(destination)?;

        for info in report.infos.iter() {
            writeln!(destination, "Info: {}", info.info)?;

            if let Some(location) = &info.location {
                write_line_from_line_number(destination, location.start(), source)?;
            }
        }

        for help in report.helps.iter() {
            writeln!(destination, "Help: {}", help.info)?;
   
            if let Some(location) = &help.location {
                write_line_from_line_number(destination, location.start(), source)?;
            }
        }

        Ok(())
    }
}
