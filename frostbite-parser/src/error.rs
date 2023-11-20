use crate::ast::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    IntTooBig { span: Span },
}

impl<L, T> From<lalrpop_util::ParseError<L, T, Self>> for Error {
    fn from(value: lalrpop_util::ParseError<L, T, Self>) -> Self {
        let lalrpop_util::ParseError::User { error } = value else {
            unreachable!()
        };

        error
    }
}
