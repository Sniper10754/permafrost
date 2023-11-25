use std::{borrow::Cow, collections::BTreeMap};

use const_format::formatcp;
use frostbite_parser::ast::{Expr, Program, Spannable};
use frostbite_report_interface::{Level, Report};

use crate::{
    arithmetic::checked_binary_operation, error::InterpreterError, rt_value::RuntimeValue,
};

#[derive(Debug, Clone, Default)]
pub struct Interpreter<'input> {
    frames: Vec<StackFrame<'input>>,
}

impl<'input> Interpreter<'input> {
    pub fn run(mut self, program: &Program<'input>) -> Result<(), InterpreterError> {
        for expr in &program.exprs {
            self.eval_expr(expr)?;
        }

        Ok(())
    }

    fn eval_expr<'a>(
        &mut self,
        expr: &'a Expr<'input>,
    ) -> Result<RuntimeValue<'input>, InterpreterError> {
        match expr {
            Expr::Int(_, int) => Ok(RuntimeValue::Int(*int)),
            Expr::Float(_, float) => Ok(RuntimeValue::Float(*float)),
            Expr::Ident(_, ident) => Ok(RuntimeValue::Ident(ident)),
            Expr::String(_, string_slice) => Ok(RuntimeValue::String(string_slice.to_string())),
            Expr::BinaryOperation { lhs, operator, rhs } => {
                let lhs = self.eval_expr(lhs)?;
                let rhs = self.eval_expr(rhs)?;

                match (lhs, rhs) {
                    (
                        lhs @ (RuntimeValue::Float(..) | RuntimeValue::Int(..)),
                        rhs @ (RuntimeValue::Float(..) | RuntimeValue::Int(..)),
                    ) => checked_binary_operation(lhs, rhs, operator.1).map_err(|_| {
                        InterpreterError::Panic(Report::new(
                            Level::Error,
                            Some(expr.span()),
                            "Operation overflowed",
                            Some(formatcp!("The limit for an integer is {}", i32::MAX)),
                            [],
                            [],
                        ))
                    }),

                    (lhs, rhs) => Err(InterpreterError::Panic(Report::new(
                        Level::Error,
                        Some(expr.span()),
                        "Incompatible operands",
                        Some(format!(
                            "`{:?}` and `{:?}` cannot be used with operand `{operator}`",
                            lhs, rhs
                        )),
                        [],
                        [],
                    ))),
                }
            }
            Expr::Assign {
                lhs,
                eq_token,
                value,
            } => {
                if !matches!(&**lhs, Expr::Ident(..)) {
                    return Err(InterpreterError::Panic(Report::new(
                        Level::Error,
                        Some(eq_token.span()),
                        "Cannot assign to static/immutable",
                        Some(format!("Cannot assign {value:?} to {lhs:?}")),
                        [],
                        [],
                    )));
                }
                match &**lhs {
                    Expr::Ident(_, ident) => {
                        let value = self.eval_expr(value)?;

                        let stack_frame_maybe_contaning_var = self
                            .frames
                            .iter_mut()
                            .find(|frame| frame.stack.contains_key(ident));

                        if let Some(frame) = stack_frame_maybe_contaning_var {
                            frame.stack.insert(ident, value);
                        } else {
                            self.frames.first_mut().expect("Theres always at least a stack frame in the stack (the global one)").stack.insert(ident, value);
                        }
                    }

                    _ => unreachable!(),
                }

                Ok(RuntimeValue::Unit)
            }
            Expr::Poisoned => unreachable!("How have we gotten here?"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StackFrame<'input> {
    name: Cow<'input, str>,
    stack: BTreeMap<&'input str, RuntimeValue<'input>>,
}
