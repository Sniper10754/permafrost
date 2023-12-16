use crate::value::{Value, Value::*};

use frostbite_parser::ast::{tokens::OperatorKind, Span};

use crate::{error::InterpretationError, Shared};

pub fn evaluate_binary_operation<'id, 'ast>(
    at: Span,
    lhs: Shared<Value<'id, 'ast>>,
    operator: OperatorKind,
    rhs: Shared<Value<'id, 'ast>>,
) -> Result<Value<'id, 'ast>, InterpretationError<'static>> {
    match (&*lhs, operator, &*rhs) {
        (Int(..), OperatorKind::Div, Int(0)) => Err(InterpretationError::DivisionByZero { at }),
        (Float(..), OperatorKind::Div, Float(right)) if *right == 0.0 => {
            Err(InterpretationError::DivisionByZero { at })
        }
        (Int(left), op, Int(right)) => Ok(match op {
            OperatorKind::Add => Int(left + right),
            OperatorKind::Sub => Int(left - right),
            OperatorKind::Mul => Int(left * right),
            OperatorKind::Div => Float(*left as f32 / *right as f32),
        }),
        (Float(left), op, Float(right)) => Ok(match op {
            OperatorKind::Add => Float(left + right),
            OperatorKind::Sub => Float(left - right),
            OperatorKind::Mul => Float(left * right),
            OperatorKind::Div => Float(left / right),
        }),
        (lhs @ Int(_) | lhs @ Float(_), op, rhs @ Int(_) | rhs @ Float(_)) => {
            let left_float = match lhs {
                Int(val) => *val as f32,
                Float(val) => *val,
                _ => unreachable!(),
            };
            let right_float = match rhs {
                Int(val) => *val as f32,
                Float(val) => *val,
                _ => unreachable!(),
            };
            Ok(match op {
                OperatorKind::Add => Float(left_float + right_float),
                OperatorKind::Sub => Float(left_float - right_float),
                OperatorKind::Mul => Float(left_float * right_float),
                OperatorKind::Div => Float(left_float / right_float),
            })
        }
        _ => Err(InterpretationError::InvalidOperands { at }),
    }
}
