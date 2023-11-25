use derive_more::*;
use frostbite_report_interface::{Level, Report};

#[derive(Debug, From)]
pub enum InterpreterError {
    Panic(Report),
}

impl From<InterpreterError> for Report {
    fn from(value: InterpreterError) -> Self {
        match value {
            InterpreterError::Panic(report) => report,
        }
    }
}
