use alloc::{
    boxed::Box,
    string::{String, ToString},
    vec::Vec,
};
use frostbite_parser::ast::Expr;

#[derive(Debug, Clone, derive_more::Display, derive_more::From)]
pub enum Value<'tokens> {
    Int(i32),
    Float(f32),
    String(String),

    #[display(fmt = "function")]
    Function {
        function_arguments: Vec<&'tokens str>,

        body: Box<Expr<'tokens>>,
    },

    Nothing,
}

impl<'str, 'src> From<&'str str> for Value<'src> {
    fn from(value: &'str str) -> Self {
        Self::String(value.to_string())
    }
}
