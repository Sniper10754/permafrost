use std::collections::HashMap;

use frostbite_parser::ast::{tokens::OperatorKind, Expr, Program, Span, Spannable};

use derive_more::*;

pub type ExternalFunction<'ast> = dyn FnMut(Vec<Value<'ast>>) -> Value<'ast>;

pub struct Interpreter<'ast> {
    stack_frames: Vec<StackFrame<'ast>>,
    intrinsic_functions: HashMap<&'static str, Box<ExternalFunction<'ast>>>,
}

impl<'ast> Interpreter<'ast> {
    pub fn new() -> Self {
        Self {
            stack_frames: vec![],
            intrinsic_functions: HashMap::new(),
        }
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

    fn eval(&mut self, expr: &Expr<'ast>) -> Result<Value<'ast>, InterpretationError<'ast>> {
        match expr {
            Expr::Int(_, int) => Ok(Value::from(*int)),
            Expr::Float(_, float) => Ok(Value::from(*float)),
            Expr::Ident(span, symbol) => {
                find_symbol_from_stack_frames(symbol, &mut self.stack_frames)
                    .map(|value| value.clone())
                    .ok_or_else(|| InterpretationError::SymbolNotFound {
                        at: span.clone(),
                        symbol,
                    })
            }
            Expr::String(_, string) => Ok(Value::from(string.to_string())),
            Expr::BinaryOperation { lhs, operator, rhs } => {
                let (lhs, rhs) = (self.eval(lhs)?, self.eval(rhs)?);

                evaluate_binary_operation(expr.span(), lhs, operator.1, rhs)
            }
            Expr::Assign {
                lhs,
                eq_token: _,
                value,
            } => match (&**lhs, self.eval(value)?) {
                (Expr::Ident(_, ident), value) => {
                    *find_symbol_from_stack_frames(ident, &mut self.stack_frames).ok_or_else(
                        || InterpretationError::SymbolNotFound {
                            at: expr.span(),
                            symbol: ident,
                        },
                    )? = value;

                    Ok(Value::Nothing)
                }

                (expr, _) => Err(InterpretationError::CannotAssignTo { at: expr.span() }),
            },
            Expr::Function {
                fn_token: _,
                name: _,
                lpt: _,
                arguments: _,
                rpt: _,
                return_type_token: _,
                return_type_annotation: _,
                equals: _,
                body: _,
            } => todo!(),
            Expr::Call {
                callee,
                lpt: _,
                arguments,
                rpt: _,
            } => {
                if let Expr::Ident(_, callee) = &**callee {
                    let callee_value =
                        match find_symbol_from_stack_frames(callee, &mut self.stack_frames) {
                            Some(value) => value,
                            None => {
                                return Err(InterpretationError::SymbolNotFound {
                                    at: expr.span(),
                                    symbol: callee,
                                })
                            }
                        };

                    if let Value::Function { name } = callee_value {
                        self.stack_frames.push(StackFrame::default());

                        
                    } else {
                        Err(InterpretationError::CannotCallNonFunction { at: expr.span() })
                    }
                } else {
                    Err(InterpretationError::CannotCallNonFunction { at: expr.span() })
                }
            }
            Expr::Poisoned => unreachable!(),
        }
    }

    pub fn intrinsic_functions_mut<'hashmap>(
        &'hashmap mut self,
    ) -> &'hashmap mut HashMap<&'static str, Box<ExternalFunction<'ast>>> {
        &mut self.intrinsic_functions
    }
}

fn find_symbol_from_stack_frames<'ast: 'stack_frame, 'stack_frame>(
    symbol: &'ast str,
    stack_frames: impl IntoIterator<Item = &'stack_frame mut StackFrame<'ast>>,
) -> Option<&'stack_frame mut Value<'ast>> {
    stack_frames
        .into_iter()
        .find(|stack_frame| stack_frame.variables.contains_key(symbol))?
        .variables
        .get_mut(symbol)
}

use Value::*;

use crate::error::InterpretationError;

fn evaluate_binary_operation<'ast>(
    at: Span,
    lhs: Value<'ast>,
    operator: OperatorKind,
    rhs: Value<'ast>,
) -> Result<Value<'ast>, InterpretationError<'static>> {
    match (lhs, operator, rhs) {
        (Int(..), OperatorKind::Div, Int(0)) => Err(InterpretationError::DivisionByZero { at }),
        (Float(..), OperatorKind::Div, Float(right)) if right == 0.0 => {
            Err(InterpretationError::DivisionByZero { at })
        }
        (Int(left), op, Int(right)) => Ok(match op {
            OperatorKind::Add => Int(left + right),
            OperatorKind::Sub => Int(left - right),
            OperatorKind::Mul => Int(left * right),
            OperatorKind::Div => Int(left / right),
        }),
        (Float(left), op, Float(right)) => Ok(match op {
            OperatorKind::Add => Float(left + right),
            OperatorKind::Sub => Float(left - right),
            OperatorKind::Mul => Float(left * right),
            OperatorKind::Div => Float(left / right),
        }),
        (lhs @ Int(_) | lhs @ Float(_), op, rhs @ Int(_) | rhs @ Float(_)) => {
            let left_float = match lhs {
                Int(val) => val as f32,
                Float(val) => val,
                _ => unreachable!(),
            };
            let right_float = match rhs {
                Int(val) => val as f32,
                Float(val) => val,
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

#[derive(Default)]
pub struct StackFrame<'ast> {
    variables: HashMap<&'ast str, Value<'ast>>,
}

#[derive(Debug, Clone, From)]
pub enum Value<'ast> {
    Int(i32),
    Float(f32),
    String(std::string::String),

    Function { name: &'ast str },

    Nothing,
}
