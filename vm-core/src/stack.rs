use alloc::{collections::VecDeque, string::String};
use dbg_pls::DebugPls;
use frostbite_bytecode::{ConstantValue, Function};

#[derive(
    Debug,
    DebugPls,
    derive_more::Index,
    derive_more::IndexMut,
    derive_more::Deref,
    derive_more::DerefMut,
)]
pub struct Stack(VecDeque<Plate>);

#[derive(
    Debug,
    DebugPls,
    derive_more::Index,
    derive_more::IndexMut,
    derive_more::Deref,
    derive_more::DerefMut,
)]
pub struct Plate(VecDeque<StackValue>);

#[derive(Debug)]
pub enum StackValue
{
    Int(i32),
    Float(f32),
    Bool(bool),
    String(String),
    Function(Function),
    Unit,
}

impl DebugPls for StackValue
{
    fn fmt(
        &self,
        f: dbg_pls::Formatter<'_>,
    )
    {
        match self {
            StackValue::Int(value) => DebugPls::fmt(value, f),
            StackValue::Float(value) => DebugPls::fmt(value, f),
            StackValue::Bool(value) => DebugPls::fmt(value, f),
            StackValue::String(value) => DebugPls::fmt(value, f),
            StackValue::Function(..) => DebugPls::fmt("first-class function", f),
            StackValue::Unit => DebugPls::fmt("Unit", f),
        }
    }
}

impl From<ConstantValue> for StackValue
{
    fn from(value: ConstantValue) -> Self
    {
        match value {
            ConstantValue::Int(value) => Self::Int(value),
            ConstantValue::Float(value) => Self::Float(value),
            ConstantValue::Bool(value) => Self::Bool(value),
            ConstantValue::String(value) => Self::String(value),
            ConstantValue::Function(function) => Self::Function(function),
            ConstantValue::Unit => Self::Unit,
        }
    }
}
