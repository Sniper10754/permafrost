use derive_more::*;
use frostbite_parser::ast::tokens::OperatorKind;

use crate::rt_value::RuntimeValue;

pub fn checked_binary_operation<'a>(
    lhs: RuntimeValue<'a>,
    rhs: RuntimeValue<'a>,
    operator_kind: OperatorKind,
) -> Result<RuntimeValue<'a>, OverflowError> {
    match (lhs, rhs) {
        (RuntimeValue::Int(lhs), RuntimeValue::Int(rhs)) => i32::checked_add(lhs, rhs)
            .map(RuntimeValue::Int)
            .ok_or(OverflowError),

        (RuntimeValue::Int(lhs), RuntimeValue::Float(rhs)) => {
            Ok(RuntimeValue::Float((lhs as f32) + rhs))
        }

        (RuntimeValue::Float(lhs), RuntimeValue::Int(rhs)) => {
            Ok(RuntimeValue::Float(lhs + (rhs as f32)))
        }

        (RuntimeValue::Float(lhs), RuntimeValue::Float(rhs)) => Ok(RuntimeValue::Float(lhs + rhs)),

        _ => unreachable!(),
    }
}

#[derive(Debug, Clone, Copy, Display)]
pub struct OverflowError;
