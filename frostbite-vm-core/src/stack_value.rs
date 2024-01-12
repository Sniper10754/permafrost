use alloc::{string::String, vec::Vec};
use frostbite_bytecode::{ConstantValue, Instruction};

#[derive(Debug, Clone, PartialEq, derive_more::From)]
pub enum StackValue {
    Int(i32),
    Float(f32),
    String(String),
    Bool(bool),

    Function { instructions: Vec<Instruction> },

    Unit,
}

impl core::ops::Add for StackValue {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (StackValue::Int(lhs), StackValue::Int(rhs)) => (lhs + rhs).into(),
            (StackValue::Float(lhs), StackValue::Float(rhs)) => (lhs + rhs).into(),
            (StackValue::Int(lhs), StackValue::Float(rhs)) => (lhs as f32 + rhs).into(),
            (StackValue::Float(lhs), StackValue::Int(rhs)) => (lhs + rhs as f32).into(),

            _ => unreachable!(),
        }
    }
}

impl core::ops::Sub for StackValue {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (StackValue::Int(lhs), StackValue::Int(rhs)) => (lhs - rhs).into(),
            (StackValue::Float(lhs), StackValue::Float(rhs)) => (lhs - rhs).into(),
            (StackValue::Int(lhs), StackValue::Float(rhs)) => (lhs as f32 - rhs).into(),
            (StackValue::Float(lhs), StackValue::Int(rhs)) => (lhs - rhs as f32).into(),

            _ => unreachable!(),
        }
    }
}

impl core::ops::Mul for StackValue {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (StackValue::Int(lhs), StackValue::Int(rhs)) => (lhs * rhs).into(),
            (StackValue::Float(lhs), StackValue::Float(rhs)) => (lhs * rhs).into(),
            (StackValue::Int(lhs), StackValue::Float(rhs)) => (lhs as f32 * rhs).into(),
            (StackValue::Float(lhs), StackValue::Int(rhs)) => (lhs * rhs as f32).into(),

            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ZeroDivisionError;

impl core::ops::Div for StackValue {
    type Output = Result<Self, ZeroDivisionError>;

    fn div(self, rhs: Self) -> Self::Output {
        match &rhs {
            StackValue::Int(0) => return Err(ZeroDivisionError),
            StackValue::Float(value) if *value == 0.0 => return Err(ZeroDivisionError),
            _ => (),
        }

        Ok(match (self, rhs) {
            (StackValue::Int(lhs), StackValue::Int(rhs)) => (lhs / rhs).into(),
            (StackValue::Float(lhs), StackValue::Float(rhs)) => (lhs / rhs).into(),
            (StackValue::Int(lhs), StackValue::Float(rhs)) => (lhs as f32 / rhs).into(),
            (StackValue::Float(lhs), StackValue::Int(rhs)) => (lhs / rhs as f32).into(),

            _ => unreachable!(),
        })
    }
}

impl From<ConstantValue> for StackValue {
    fn from(constant_value: ConstantValue) -> Self {
        match constant_value {
            ConstantValue::Int(int) => Self::from(int),
            ConstantValue::Float(float) => Self::from(float),
            ConstantValue::String(string) => Self::from(string),
            ConstantValue::Bool(bool) => Self::from(bool),
            ConstantValue::Function(function) => Self::Function { instructions: function.body },
            ConstantValue::Unit => Self::Unit,
        }
    }
}
