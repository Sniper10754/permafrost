use std::{
    borrow::Cow,
    collections::{BTreeMap, HashMap},
};

use const_format::formatcp;
use derive_more::*;

use frostbite_parser::{
    ast::{Expr, Program, Spannable},
    lexer, Parser,
};
use frostbite_report_interface::{Level, Report};

use crate::{
    arithmetic::{checked_binary_operation, OperationError},
    error::InterpreterError,
    rt_value::RuntimeValue,
};

#[derive(Debug, Clone, Default)]
pub struct Threads<'input>(Vec<Thread<'input>>);

#[derive(Debug, Clone)]
pub struct Thread<'input>(HashMap<&'static str, StackFrame<'input>>);

#[derive(Debug, Clone, Default)]
pub struct Interpreter<'input> {
    threads: Threads<'input>,
}

impl<'input> Interpreter<'input> {
    pub fn run_source_in_thread(
        &self,
        thread: Thread<'input>,
        input: impl AsRef<str>,
    ) -> Result<(), InterpreterError> {
        let token_stream = lexer::tokenize(input.as_ref()).expect("Prototyping");
        let ast = Parser::with_tokenstream(token_stream).parse();

        let ast = match ast {
            Some(ast) => ast,
            None => {
                todo!()
            }
        };

        Interpreter::default().run_ast_in_thread(&ast)?;

        Ok(())
    }

    pub fn run_ast_in_thread(mut self, program: &Program<'input>) -> Result<(), InterpreterError> {
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
                    ) => checked_binary_operation(lhs, rhs, operator.1).map_err(|error| {
                        InterpreterError::Runtime {
                            report: Report::new(
                                Level::Error,
                                Some(expr.span()),
                                match error {
                                    OperationError::Overflow => "Operation overflowed",
                                    OperationError::DivisionByZero => "Tried to divide by zero",
                                },
                                Some(match error {
                                    OperationError::Overflow => {
                                        formatcp!("Maximum limit for numbers is {}", i32::MAX)
                                    }
                                    OperationError::DivisionByZero => {
                                        "The right hand side of the expression was 0"
                                    }
                                }),
                                [],
                                [],
                            )
                            .into(),
                        }
                    }),

                    (lhs, rhs) => Err(InterpreterError::Runtime {
                        report: Report::new(
                            Level::Error,
                            Some(expr.span()),
                            "Incompatible operands",
                            Some(format!(
                                "`{:?}` and `{:?}` cannot be used with operand `{operator}`",
                                lhs, rhs
                            )),
                            [],
                            [],
                        )
                        .into(),
                    }),
                }
            }
            Expr::Assign {
                lhs,
                eq_token,
                value,
            } => {
                if !matches!(&**lhs, Expr::Ident(..)) {
                    return Err(InterpreterError::Runtime {
                        report: Report::new(
                            Level::Error,
                            Some(eq_token.span()),
                            "Cannot assign to static/immutable",
                            Some(format!("Cannot assign {value:?} to {lhs:?}")),
                            [],
                            [],
                        )
                        .into(),
                    });
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

#[derive(Debug, Display, Clone)]
#[display(fmt = "{}: {:?}", name, stack)]
pub struct StackFrame<'input> {
    name: Cow<'input, str>,
    stack: BTreeMap<&'input str, RuntimeValue<'input>>,
}
