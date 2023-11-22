use std::{borrow::Cow, collections::BTreeMap};

use frostbite_parser::ast::{tokens::OperatorKind, Expr, Program, Spannable};
use frostbite_report_interface::{Level, Report};

use crate::rt_value::RuntimeValue;

#[derive(Debug, Clone, Default)]
pub struct Interpreter<'input> {
    frames: Vec<StackFrame<'input>>,
}

#[derive(Debug, Clone)]
struct StackFrame<'input> {
    name: Cow<'input, str>,
    stack: BTreeMap<&'input str, RuntimeValue<'input>>,
}

impl<'input> Interpreter<'input> {
    pub fn run(mut self, program: &Program<'input>) {
        for expr in &program.exprs {
            match self.eval_expr(expr) {
                Ok(value) => {
                    println!(" -> {value}");
                }
                Err(err) => {
                    println!("{err:?}");

                    break;
                }
            }
        }
    }

    fn eval_expr<'a>(
        &mut self,
        expr: &'a Expr<'input>,
    ) -> Result<RuntimeValue<'input>, Box<Report>> {
        match expr {
            Expr::Int(_, int) => Ok(RuntimeValue::Int(*int)),
            Expr::Float(_, float) => Ok(RuntimeValue::Float(*float)),
            Expr::Ident(_, ident) => Ok(RuntimeValue::Ident(ident)),
            Expr::String(_, string_slice) => Ok(RuntimeValue::String(string_slice.to_string())),
            Expr::BinaryOperation { lhs, operator, rhs } => {
                let lhs = self.eval_expr(lhs)?;
                let rhs = self.eval_expr(rhs)?;

                match (lhs, rhs) {
                    (RuntimeValue::Int(lhs), RuntimeValue::Int(rhs)) => match operator.1 {
                        OperatorKind::Add => Ok(RuntimeValue::Int(lhs + rhs)),
                        OperatorKind::Sub => Ok(RuntimeValue::Int(lhs - rhs)),
                        OperatorKind::Mul => Ok(RuntimeValue::Int(lhs * rhs)),
                        OperatorKind::Div => Ok(RuntimeValue::Int(lhs / rhs)),
                    },
                    (RuntimeValue::Float(lhs), RuntimeValue::Float(rhs)) => match operator.1 {
                        OperatorKind::Add => Ok(RuntimeValue::Float(lhs + rhs)),
                        OperatorKind::Sub => Ok(RuntimeValue::Float(lhs - rhs)),
                        OperatorKind::Mul => Ok(RuntimeValue::Float(lhs * rhs)),
                        OperatorKind::Div => Ok(RuntimeValue::Float(lhs / rhs)),
                    },

                    (lhs, rhs) => Err(Report {
                        level: Level::Error,
                        location: Some(expr.span().into()),
                        title: "Incompatible operands".into(),
                        description: Some(
                            format!(
                                "`{:?}` and `{:?}` cannot be used with operand `{operator}`",
                                lhs, rhs
                            )
                            .into(),
                        ),
                        infos: vec![],
                        helps: vec![],
                    }
                    .into()),
                }
            }
            Expr::Assign {
                lhs,
                eq_token,
                value,
            } => {
                if !matches!(&**lhs, Expr::Ident(..)) {
                    return Err(Report {
                        level: Level::Error,
                        location: Some(eq_token.span().into()),
                        title: "Cannot assign to static/immutable".into(),
                        description: Some(format!("Cannot assign {value:?} to {lhs:?}").into()),
                        infos: vec![],
                        helps: vec![],
                    }
                    .into());
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
                            self.frames.first_mut().unwrap().stack.insert(ident, value);
                        }
                    }

                    _ => todo!(),
                }

                Ok(RuntimeValue::Unit)
            }
        }
    }
}
