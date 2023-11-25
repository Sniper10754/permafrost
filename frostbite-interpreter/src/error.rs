use std::io;

use derive_more::*;
use frostbite_report_interface::Report;

pub type FileId = usize;

#[derive(Debug, From)]
pub enum InterpreterError {
    Io(io::Error),

    Runtime { report: Box<Report> },
}

impl From<InterpreterError> for Report {
    fn from(value: InterpreterError) -> Self {
        match value {
            InterpreterError::Io(_) => todo!(),
            InterpreterError::Runtime { report } => *report,
        }
    }
}
