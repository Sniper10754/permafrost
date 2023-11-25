use derive_more::*;
use frostbite_parser::ast::tokens::OperatorKind;

use crate::rt_value::RuntimeValue;

pub fn checked_binary_operation<'a>(
    lhs: RuntimeValue<'a>,
    rhs: RuntimeValue<'a>,
    operator_kind: OperatorKind,
) -> Result<RuntimeValue<'a>, OverflowError> {
    match (lhs, rhs, operator_kind) {
        #[rustfmt::skip]
        (RuntimeValue::Int(lhs), RuntimeValue::Int(rhs), op) => match op {
            OperatorKind::Add => i32::checked_add(lhs, rhs).map(RuntimeValue::Int).ok_or(OverflowError),
            OperatorKind::Sub => i32::checked_sub(lhs, rhs).map(RuntimeValue::Int).ok_or(OverflowError),
            OperatorKind::Mul => i32::checked_mul(lhs, rhs).map(RuntimeValue::Int).ok_or(OverflowError),
            OperatorKind::Div => i32::checked_div(lhs, rhs).map(RuntimeValue::Int).ok_or(OverflowError),
        },

        #[rustfmt::skip]
        (RuntimeValue::Int(lhs), RuntimeValue::Float(rhs), op) => match op {
            OperatorKind::Add => Ok(RuntimeValue::Float((lhs as f32) + rhs)),
            OperatorKind::Sub => Ok(RuntimeValue::Float((lhs as f32) - rhs)),
            OperatorKind::Mul => Ok(RuntimeValue::Float((lhs as f32) * rhs)),
            OperatorKind::Div => Ok(RuntimeValue::Float((lhs as f32) / rhs)),
        },

        #[rustfmt::skip]
        (RuntimeValue::Float(lhs), RuntimeValue::Int(rhs), op) => match op {
            OperatorKind::Add => Ok(RuntimeValue::Float(lhs + (rhs as f32))),
            OperatorKind::Sub => Ok(RuntimeValue::Float(lhs - (rhs as f32))),
            OperatorKind::Mul => Ok(RuntimeValue::Float(lhs * (rhs as f32))),
            OperatorKind::Div => Ok(RuntimeValue::Float(lhs / (rhs as f32))),
        },

        #[rustfmt::skip]
        (RuntimeValue::Float(lhs), RuntimeValue::Float(rhs), op) => match op {
            OperatorKind::Add => Ok(RuntimeValue::Float(lhs + rhs)),
            OperatorKind::Sub => Ok(RuntimeValue::Float(lhs - rhs)),
            OperatorKind::Mul => Ok(RuntimeValue::Float(lhs * rhs)),
            OperatorKind::Div => Ok(RuntimeValue::Float(lhs / rhs)),
        },

        _ => unreachable!(),
    }
}

#[derive(Debug, Clone, Copy, Display)]
pub struct OverflowError;
