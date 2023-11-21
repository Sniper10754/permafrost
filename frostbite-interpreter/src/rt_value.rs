#[derive(Debug, Clone)]
pub enum RuntimeValue<'input> {
    Int(i32),
    Float(f32),
    Ident(&'input str),
    String(String),

    Unit,
}
