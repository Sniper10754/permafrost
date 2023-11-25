use derive_more::*;
use frostbite_report_interface::Report;

#[derive(Debug, From)]
pub enum InterpreterError {
    Panic(Box<Report>),
}

impl From<InterpreterError> for Report {
    fn from(value: InterpreterError) -> Self {
        match value {
            InterpreterError::Panic(report) => *report,
        }
    }
}
