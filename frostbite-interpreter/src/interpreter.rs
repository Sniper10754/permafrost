use std::{borrow::Cow, collections::BTreeMap};

use derive_more::*;

use frostbite_parser::ast::{Expr, Program, Spannable};
use frostbite_report_interface::{Level, Report};

use crate::rt_value::RuntimeValue;

#[derive(Debug, Clone, Default)]
pub struct Interpreter<'input> {
    frames: Vec<StackFrame<'input>>,
}

#[derive(Debug, Display, Clone)]
#[display(fmt = "{}: {:?}", name, stack)]
struct StackFrame<'input> {
    name: Cow<'input, str>,
    stack: BTreeMap<&'input str, RuntimeValue<'input>>,
}

impl<'input> Interpreter<'input> {
    pub fn run(mut self, program: &Program<'input>) {
        for expr in &program.exprs {
            match self.eval_expr(&expr) {
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

    fn eval_expr<'a>(&mut self, expr: &'a Expr<'input>) -> Result<RuntimeValue<'input>, Report> {
        match expr {
            Expr::Int(_, int) => Ok(RuntimeValue::Int(*int)),
            Expr::Float(_, float) => Ok(RuntimeValue::Float(*float)),
            Expr::Ident(_, ident) => Ok(RuntimeValue::Ident(ident)),
            Expr::String(_, string_slice) => Ok(RuntimeValue::String(string_slice.to_string())),
            Expr::BinaryOperation { lhs, operator, rhs } => {
                let lhs = self.eval_expr(lhs)?;
                let rhs = self.eval_expr(rhs)?;

                match (lhs, rhs) {
                    (RuntimeValue::Int(lhs), RuntimeValue::Int(rhs)) => match operator {
                        Add => Ok(RuntimeValue::Int(lhs + rhs)),
                        Sub => Ok(RuntimeValue::Int(lhs - rhs)),
                        Mul => Ok(RuntimeValue::Int(lhs * rhs)),
                        Div => Ok(RuntimeValue::Int(lhs / rhs)),
                    },
                    (RuntimeValue::Float(lhs), RuntimeValue::Float(rhs)) => match operator {
                        Add => Ok(RuntimeValue::Float(lhs + rhs)),
                        Sub => Ok(RuntimeValue::Float(lhs - rhs)),
                        Mul => Ok(RuntimeValue::Float(lhs * rhs)),
                        Div => Ok(RuntimeValue::Float(lhs / rhs)),
                    },

                    (lhs, rhs) => Err(Report {
                        level: Level::Error,
                        span: Some(expr.span()),
                        title: format!("Incompatible operands").into(),
                        description: Some(
                            format!(
                                "`{:?}` and `{:?}` cannot be used with operand `{operator}`",
                                lhs, rhs
                            )
                            .into(),
                        ),
                        infos: vec![],
                        helps: vec![],
                    }),
                }
            }
            Expr::Assign { lhs, value } => {
                if !matches!(&**lhs, Expr::Ident(..)) {
                    return Err(Report {
                        level: Level::Error,
                        span: Some(lhs.span()),
                        title: "Cannot assign to static/immutable".into(),
                        description: Some(format!("Cannot assign {value:?} to {lhs:?}").into()),
                        infos: vec![],
                        helps: vec![],
                    });
                }
                match &**lhs {
                    Expr::Ident(_, ident) => {
                        let value = self.eval_expr(value)?;

                        let stack_frame_maybe_contaning_var = self
                            .frames
                            .iter_mut()
                            .filter(|frame| frame.stack.contains_key(ident))
                            .next();

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
