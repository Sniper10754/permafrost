use frostbite_report_interface::{Level, Location, Report};

#[derive(Debug)]
pub enum InterpreterError {
    Panic,
}

impl From<InterpreterError> for Report {
    fn from(value: InterpreterError) -> Self {
        match value {
            InterpreterError::Panic => Report::new(
                Level::Error,
                None::<Location>,
                "Application panicked",
                Some("An unrecoverable error has happened"),
                [],
                [],
            ),
        }
    }
}
