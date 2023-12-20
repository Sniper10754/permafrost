use alloc::{
    boxed::Box,
    string::{String, ToString},
    vec::Vec,
};
use frostbite_parser::ast::Expr;
use frostbite_reports::sourcemap::SourceId;

#[derive(Debug, Clone, derive_more::Display, derive_more::From)]
pub enum Value<'id, 'tokens> {
    Int(i32),
    Float(f32),
    String(String),

    #[display(fmt = "function")]
    Function {
        function_arguments: Vec<&'tokens str>,

        body: Box<Expr<'tokens>>,
        source_id: SourceId<'id>,
    },

    Nothing,
}

impl<'id, 'str, 'src> From<&'str str> for Value<'id, 'src> {
    fn from(value: &'str str) -> Self {
        Self::String(value.to_string())
    }
}
