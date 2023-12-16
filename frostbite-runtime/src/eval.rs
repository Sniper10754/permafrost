use alloc::{boxed::Box, collections::BTreeMap, vec};
use frostbite_parser::ast::{Expr, Spannable, Spanned};
use frostbite_reports::sourcemap::SourceId;

use crate::{
    error::InterpretationError,
    helper::find_symbol_from_stack_frames,
    internals::Shared,
    intrinsic::IntrinsicContext,
    math::evaluate_binary_operation,
    stack::{Stack, StackFrame},
    value::Value,
    ExternalFunction,
};

pub struct Sandbox<'intrinsic_ctx, 'id, 'ast> {
    main_file_source_id: SourceId<'id>,
    stack: Stack<'id, 'ast>,
    intrinsic_ctx: &'intrinsic_ctx IntrinsicContext<'id, 'ast>,
}

impl<'intrinsic_ctx, 'id, 'ast> Sandbox<'intrinsic_ctx, 'id, 'ast> {
    pub fn new(
        main_file_source_id: SourceId<'id>,
        intrinsic_ctx: &'intrinsic_ctx IntrinsicContext<'id, 'ast>,
    ) -> Self {
        Self {
            main_file_source_id,
            stack: Stack::default(),
            intrinsic_ctx,
        }
    }

    pub fn eval(
        &mut self,
        expr: &Expr<'ast>,
    ) -> Result<Shared<Value<'id, 'ast>>, InterpretationError<'ast>> {
        match expr {
            Expr::Int(_, int) => Ok(Value::from(*int).into()),

            Expr::Float(_, float) => Ok(Value::from(*float).into()),

            Expr::Ident(span, symbol) => find_symbol_from_stack_frames(symbol, &mut self.stack)
                .map(|(_, value)| value.clone())
                .ok_or_else(|| InterpretationError::SymbolNotFound {
                    at: span.clone(),
                    symbol,
                }),

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
                    if let Some((stack_frame, _)) =
                        find_symbol_from_stack_frames(ident, &mut self.stack)
                    {
                        stack_frame.symbols.insert(ident, value);
                    } else {
                        self.stack.first_mut().unwrap().symbols.insert(ident, value);
                    }

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
                self.stack.first_mut().unwrap().symbols.insert(
                    name,
                    Value::Function {
                        function_arguments: arguments
                            .iter()
                            .map(|argument| argument.name.1)
                            .collect(),
                        body: body.clone(),
                        source_id: self.main_file_source_id,
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
                    match find_symbol_from_stack_frames(callee, &mut self.stack) {
                        Some((_, callee_value)) => {
                            if let Value::Function {
                                function_arguments,
                                body,
                                source_id,
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

                                self.stack
                                    .push(StackFrame::new(*source_id, Some(expr.span().into())));

                                let stack_frame = self.stack.first_mut().unwrap();

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

                                self.stack.pop();

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
                            if self.intrinsic_ctx.intrinsic_functions.contains_key(callee) {
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

                                let function = &self.intrinsic_ctx.intrinsic_functions[*callee];

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
}
