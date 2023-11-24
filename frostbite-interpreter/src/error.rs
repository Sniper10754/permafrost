use derive_more::*;
use frostbite_report_interface::{Level, Location, Report};

#[derive(Debug, From)]
pub enum InterpreterError {
    Panic(Report),
}

impl From<InterpreterError> for Report {
    fn from(value: InterpreterError) -> Self {
        match value {
            InterpreterError::Panic(report) => Report::new(
                Level::Error,
                report.location,
                "Application panicked",
                Some(report.title),
                report.infos,
                report.helps,
            ),
        }
    }
}
