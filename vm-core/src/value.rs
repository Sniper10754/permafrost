use alloc::string::String;

use derive_more::*;
use frostbite_bytecode::ConstantValue;

#[derive(Debug, Clone, PartialEq, From)]
pub enum Value
{
    Int(i32),
    Float(f32),
    String(String),
    Bool(bool),
    Function(frostbite_bytecode::Function),
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
