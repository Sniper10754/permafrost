use alloc::{borrow::Cow, format};

use frostbite_parser::ast::Span;
use frostbite_reports::{sourcemap::SourceId, IntoReport, Level};

pub enum InterpretationError<'ast> {
    SymbolNotFound { at: Span, symbol: &'ast str },
    CannotAssignTo { at: Span },
    InvalidOperands { at: Span },
    DivisionByZero { at: Span },
    CannotCallNonFunctionIdent { at: Span },
    CannotCallNonFunctionValue { at: Span },
}

impl<'id, 'ast> IntoReport<'id> for InterpretationError<'ast> {
    type Arguments = SourceId<'id>;

    fn into_report(self, id: Self::Arguments) -> frostbite_reports::Report<'id> {
        let location;
        let title;
        let description: Option<Cow<'_, _>>;

        match self {
            InterpretationError::SymbolNotFound { at, symbol } => {
                location = at;

                title = "No symbol found";
                description = Some(format!("Symbol {symbol} was not found").into());
            }
            InterpretationError::CannotAssignTo { at } => {
                location = at;

                title = "Assignee not supported";
                description = Some("you can assign only to identifiers".into());
            }
            InterpretationError::InvalidOperands { at } => {
                location = at;

                title = "Invalid operands";
                description = None;
            }
            InterpretationError::DivisionByZero { at } => {
                location = at;

                title = "Division by zero";
                description = Some("The right part of the expression evaluated to 0".into())
            }
            InterpretationError::CannotCallNonFunctionIdent { at } => {
                location = at;

                title = "Cannot call expression";
                description = Some("You can only call identifiers".into())
            }
            InterpretationError::CannotCallNonFunctionValue { at } => {
                location = at;

                title = "Cannot call values which arent functions";
                description = Some("You can only call values that resolve to functions".into())
            }
        }

        frostbite_reports::Report::new_diagnostic(
            Level::Error,
            location,
            id,
            title,
            description,
            [],
            [],
        )
    }
}
