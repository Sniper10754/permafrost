use derive_more::*;

#[derive(Debug, Display, Unwrap, From, Clone)]
pub enum RuntimeValue<'input> {
    #[display(fmt = "{_0}")]
    Int(i32),
    #[display(fmt = "{_0}")]
    Float(f32),
    #[display(fmt = "{_0}")]
    Ident(&'input str),
    #[display(fmt = "{_0}")]
    String(String),

    #[display(fmt = "()")]
    Unit,
}
