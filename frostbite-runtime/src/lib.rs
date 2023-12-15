#![no_std]

extern crate alloc;

use alloc::{boxed::Box, collections::BTreeMap, rc::Rc, vec, vec::Vec};

use frostbite_parser::ast::{Expr, Program, Spannable, Spanned};

use error::InterpretationError;
use math::evaluate_binary_operation;
use value::Value;

pub mod error;
pub mod value;

mod math;

pub type ExternalFunction<'ast> = dyn Fn(&[Rc<Value<'ast>>]) -> Value<'ast>;

pub(crate) type Shared<T> = Rc<T>;

pub struct Runtime<'ast> {
    stack_frames: Vec<StackFrame<'ast>>,
    intrinsic_functions: BTreeMap<&'static str, Box<ExternalFunction<'ast>>>,
}

#[derive(Default)]
pub struct StackFrame<'ast> {
    symbols: BTreeMap<&'ast str, Shared<Value<'ast>>>,
}

impl<'ast> Default for Runtime<'ast> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'ast> Runtime<'ast> {
    pub fn new() -> Self {
        Self {
            stack_frames: [StackFrame::default()].into(),
            intrinsic_functions: BTreeMap::default(),
        }
    }

    fn enter_stack_frame(&mut self) {
        self.stack_frames.push(StackFrame::default());
    }

    fn leave_stack_frame(&mut self) {
        self.stack_frames.pop();
    }

    pub fn eval_program(
        &mut self,
        program: &Program<'ast>,
    ) -> Result<(), InterpretationError<'ast>> {
        for expr in program.exprs.iter() {
            self.eval(expr)?;
        }

        Ok(())
    }

    fn eval(
        &mut self,
        expr: &Expr<'ast>,
    ) -> Result<Shared<Value<'ast>>, InterpretationError<'ast>> {
        match expr {
            Expr::Int(_, int) => Ok(Value::from(*int).into()),

            Expr::Float(_, float) => Ok(Value::from(*float).into()),

            Expr::Ident(span, symbol) => {
                find_symbol_from_stack_frames(symbol, &mut self.stack_frames)
                    .map(|(_, value)| value.clone())
                    .ok_or_else(|| InterpretationError::SymbolNotFound {
                        at: span.clone(),
                        symbol,
                    })
            }

            Expr::String(_, string) => Ok(Value::from(*string).into()),

            Expr::BinaryOperation { lhs, operator, rhs } => {
                let (lhs, rhs) = (self.eval(lhs)?, self.eval(rhs)?);

                evaluate_binary_operation(expr.span(), lhs, operator.1, rhs).map(Into::into)
            }

            Expr::Assign {
                lhs,
                eq_token: _,
                value,
            } => match (&**lhs, self.eval(value)?) {
                (Expr::Ident(_, ident), value) => {
                    let (stack_frame, _) =
                        find_symbol_from_stack_frames(ident, &mut self.stack_frames).ok_or_else(
                            || InterpretationError::SymbolNotFound {
                                at: lhs.span(),
                                symbol: ident,
                            },
                        )?;

                    stack_frame.symbols.insert(ident, value);

                    Ok(Value::Nothing.into())
                }

                (expr, _) => Err(InterpretationError::CannotAssignTo { at: expr.span() }),
            },

            Expr::Function {
                fn_token: _,
                name: Spanned(_, name),
                lpt: _,
                arguments,
                rpt: _,
                return_type_token: _,
                return_type_annotation: _,
                equals: _,
                body,
            } => {
                self.stack_frames.first_mut().unwrap().symbols.insert(
                    name,
                    Value::Function {
                        function_arguments: arguments
                            .iter()
                            .map(|argument| argument.name.1)
                            .collect(),
                        body: body.clone(),
                    }
                    .into(),
                );

                Ok(Value::Nothing.into())
            }

            Expr::Call {
                callee,
                lpt: _,
                arguments: call_arguments,
                rpt: _,
            } => {
                if let Expr::Ident(_, callee) = &**callee {
                    match find_symbol_from_stack_frames(callee, &mut self.stack_frames) {
                        Some((_, callee_value)) => {
                            if let Value::Function {
                                function_arguments,
                                body,
                            } = &*callee_value
                            {
                                let evaluated_call_arguments = call_arguments
                                    .iter()
                                    .map(|expression| self.eval(expression))
                                    .try_fold(
                                        vec![],
                                        |mut acc, evaluated_arg| match evaluated_arg {
                                            Ok(evaluated_arg) => {
                                                acc.push(evaluated_arg);

                                                Ok(acc)
                                            }
                                            Err(err) => Err(err),
                                        },
                                    )?;

                                self.enter_stack_frame();

                                let stack_frame = self.stack_frames.first_mut().unwrap();

                                function_arguments
                                    .iter()
                                    .zip(evaluated_call_arguments)
                                    .for_each(|(function_arg_name, call_arg_value)| {
                                        stack_frame
                                            .symbols
                                            .insert(function_arg_name, call_arg_value);
                                    });

                                let return_value = match body {
                                    expr => Some(self.eval(expr)?),

                                    #[allow(unreachable_patterns)]
                                    _ => todo!("Blocks not implemented yet"),
                                };

                                self.leave_stack_frame();

                                match return_value {
                                    Some(value) => Ok(value),
                                    None => Ok(Value::Nothing.into()),
                                }
                            } else {
                                Err(InterpretationError::CannotCallNonFunctionIdent {
                                    at: expr.span(),
                                })
                            }
                        }
                        None => {
                            if self.intrinsic_functions.contains_key(callee) {
                                let evaluated_call_arguments = call_arguments
                                    .iter()
                                    .map(|expression| self.eval(expression))
                                    .try_fold(
                                        vec![],
                                        |mut acc, evaluated_arg| match evaluated_arg {
                                            Ok(evaluated_arg) => {
                                                acc.push(evaluated_arg);

                                                Ok(acc)
                                            }
                                            Err(err) => Err(err),
                                        },
                                    )?;

                                let function = &self.intrinsic_functions[*callee];

                                Ok(function(&evaluated_call_arguments).into())
                            } else {
                                Err(InterpretationError::SymbolNotFound {
                                    at: expr.span(),
                                    symbol: callee,
                                })
                            }
                        }
                    }
                } else {
                    Err(InterpretationError::CannotCallNonFunctionValue { at: expr.span() })
                }
            }

            Expr::Poisoned => unreachable!(),
        }
    }

    pub fn intrinsic_functions_mut<'hashmap>(
        &'hashmap mut self,
    ) -> &'hashmap mut BTreeMap<&'static str, Box<ExternalFunction<'ast>>> {
        &mut self.intrinsic_functions
    }
}

fn find_symbol_from_stack_frames<'ast, 'stack_frame>(
    symbol: &'ast str,
    stack_frames: impl IntoIterator<Item = &'stack_frame mut StackFrame<'ast>>,
) -> Option<(&'stack_frame mut StackFrame<'ast>, Shared<Value<'ast>>)> {
    let stack_frame = stack_frames
        .into_iter()
        .find(|stack_frame| stack_frame.symbols.contains_key(symbol))?;

    let symbol = stack_frame.symbols[symbol].clone();

    Some((stack_frame, symbol))
}
