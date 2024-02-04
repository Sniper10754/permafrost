use alloc::string::String;

use derive_more::*;
use permafrost_bytecode::ConstantValue;

#[derive(Debug, Clone, PartialEq, Display, From)]
pub enum Value
{
    Int(i32),
    Float(f32),
    String(String),
    Bool(bool),
    #[display(fmt = "fn")]
    Function(permafrost_bytecode::Function),
    #[display(fmt = "unit")]
    Nil,
}

impl From<ConstantValue> for Value
{
    fn from(value: ConstantValue) -> Self
    {
        match value {
            ConstantValue::Int(value) => Self::from(value),
            ConstantValue::Float(value) => Self::from(value),
            ConstantValue::String(value) => Self::from(value),
            ConstantValue::Bool(value) => Self::from(value),
            ConstantValue::Function(value) => Self::from(value),
            ConstantValue::Unit => Self::Nil,
        }
    }
}
