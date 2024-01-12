#![allow(clippy::single_match)]

extern crate std;

use std::dbg;

use core::{
    cmp::Ordering::{Equal, Greater, Less},
    ops::Range,
};

use alloc::{
    borrow::Cow, boxed::Box, collections::BTreeMap, format, string::String, vec, vec::Vec,
};

use frostbite_parser::ast::{
    tokens::{BinaryOperatorKind, TypeAnnotation},
    Argument, Expr, Spannable, Spanned,
};
use frostbite_reports::{sourcemap::SourceId, IntoReport, Label, Level, Report};

use crate::{
    context::CompilerContext,
    tir::{
        display::display_type, Assignable, Callable, FunctionType, RefersTo, Type, TypeIndex,
        TypedAst, TypedExpression, TypedFunctionExpr,
    },
    utils::Scopes,
};

#[derive(Debug)]
pub enum TypecheckError {
    TypeMismatch {
        source_id: SourceId,
        span: Range<usize>,

        expected: Cow<'static, str>,
        found: Cow<'static, str>,
    },
    SymbolNotFound(SourceId, Spanned<String>),
    IncompatibleOperands {
        source_id: SourceId,
        span: Range<usize>,

        left: Cow<'static, str>,
        right: Cow<'static, str>,
    },
    CannotAssignTo(SourceId, Range<usize>),
    CannotCallNonIdent(SourceId, Range<usize>),
    CannotCallNonFunction(SourceId, Range<usize>),
    TooManyArguments {
        source_id: SourceId,
        span: Range<usize>,

        call_arguments_len: usize,
        function_arguments_len: usize,
    },
    NotEnoughArguments {
        source_id: SourceId,
        span: Range<usize>,

        call_arguments_len: usize,
        function_arguments_len: usize,
    },
    FunctionDoesntReturn {
        source_id: SourceId,
        faulty_branch_position: Range<usize>,
    },
}

impl IntoReport for TypecheckError {
    fn into_report(self) -> Report {
        match self {
            TypecheckError::TypeMismatch {
                source_id,
                span,
                expected,
                found,
            } => Report::new_diagnostic(
                Level::Error,
                span,
                source_id,
                "Type mismatch",
                Some(format!("Expected type {expected:?}, found type {found:?}")),
                [],
                [],
            ),
            TypecheckError::SymbolNotFound(source_id, Spanned(span, ident)) => {
                Report::new_diagnostic(
                    Level::Error,
                    span,
                    source_id,
                    "Symbol not found",
                    Some(format!("Symbol {ident} not found")),
                    [],
                    [],
                )
            }
            TypecheckError::IncompatibleOperands { source_id, span, left, right } => Report::new_diagnostic(
                Level::Error,
                span.clone(),
                source_id,
                "Incompatible operands",
                None::<&str>,
                [
                    Label::new(format!("Left type is {left}"), span.clone(), source_id),
                    Label::new(format!("Right type is {right}"), span.clone(), source_id),
                ],
                [],
            ),
            TypecheckError::CannotAssignTo(source_id, span) => Report::new_diagnostic(
                Level::Error,
                span,
                source_id,
                "Cannot assign to",
                Some("Only identifiers may be assigned to"),
                [],
                [],
            ),
            TypecheckError::CannotCallNonIdent(source_id, span) => Report::new_diagnostic(
                Level::Error,
                span,
                source_id,
                "Cannot call expression",
                Some("Only identifiers may be called"),
                [],
                [],
            ),
            TypecheckError::CannotCallNonFunction(source_id, span) => Report::new_diagnostic(
                Level::Error,
                span,
                source_id,
                "Cannot call non function",
                Some("Only functions may be called"),
                [],
                [],
            ),
            TypecheckError::TooManyArguments {
                source_id,
                span,
                call_arguments_len: call_arguments,
                function_arguments_len: function_arguments,
            } => Report::new_diagnostic(
                Level::Error,
                span,
                source_id,
                "Too many arguments",
                Some(format!(
                    "Function expected {function_arguments} arguments, but was called with {call_arguments} arguments"
                )),
                [],
                [],
            ),
            TypecheckError::NotEnoughArguments {
                source_id,
                span,
                call_arguments_len,
                function_arguments_len,
            } => Report::new_diagnostic(
                Level::Error,
                span,
                source_id,
                "Not enough arguments",
                Some(format!(
                    "Function expected {function_arguments_len} arguments, but was called with {call_arguments_len} arguments"
                )),
                [],
                [],
            ),
            TypecheckError::FunctionDoesntReturn { source_id, faulty_branch_position } => Report::new_diagnostic(Level::Error, faulty_branch_position, source_id, "One branch of this function doesnt return", Some("This branch of this function doesnt return."), [], []),
        }
    }
}

pub fn check_types(compiler_ctx: &mut CompilerContext, source_id: SourceId) {
    let mut rts = RecursiveTypechecker::new();
    let mut typed_ast = TypedAst::default();

    let ast = &compiler_ctx.asts[source_id];

    for expr in &ast.exprs {
        match rts.visit_expr(source_id, expr, &mut typed_ast) {
            Ok(t_expr) => typed_ast.nodes.push(t_expr),
            Err(report) => compiler_ctx.report_ctx.push(report.into_report()),
        }
    }

    compiler_ctx.t_asts.insert(source_id, typed_ast);
}

struct RecursiveTypechecker {
    scopes: Scopes<RefersTo>,
}

impl RecursiveTypechecker {
    fn new() -> Self {
        Self {
            scopes: Scopes::new(),
        }
    }

    fn unify(t_ast: &mut TypedAst, a: TypeIndex, b: TypeIndex) -> Result<(), ()> {
        match (t_ast.types_arena[a].clone(), t_ast.types_arena[b].clone()) {
            (Type::Unit, Type::Unit) => Ok(()),
            (Type::Object(left), Type::Object(right)) if left == right => Ok(()),
            (Type::Int, Type::Int) => Ok(()),
            (Type::Function(left_fn_type), Type::Function(right_fn_type)) => {
                Iterator::zip(
                    left_fn_type.arguments.values(),
                    right_fn_type.arguments.values(),
                )
                .try_for_each(|(left, right)| Self::unify(t_ast, *left, *right))?;

                Ok(())
            }
            (Type::Float, Type::Float) => Ok(()),
            (Type::Bool, Type::Bool) => Ok(()),
            (_, Type::Any) => Ok(()),
            (Type::Any, _) => Ok(()),

            _ => Err(()),
        }
    }

    fn infer_type(
        &mut self,
        source_id: SourceId,
        expr: &Expr,
        t_ast: &mut TypedAst,
    ) -> Result<TypeIndex, TypecheckError> {
        match expr {
            Expr::Int(_) => Ok(t_ast.types_arena.insert(Type::Int)),
            Expr::Float(_) => Ok(t_ast.types_arena.insert(Type::Float)),
            Expr::String(_) => Ok(t_ast.types_arena.insert(Type::String)),
            Expr::Bool(_) => Ok(t_ast.types_arena.insert(Type::Bool)),
            Expr::Ident(spanned_str) => {
                let Some(referred_to) = self.scopes.local(spanned_str.value()).copied() else {
                    return Err(TypecheckError::SymbolNotFound(
                        source_id,
                        spanned_str.clone(),
                    ));
                };

                match referred_to {
                    RefersTo::Local(local_index) => Ok(t_ast.locals[local_index]),
                    RefersTo::Type(type_index) => Ok(type_index),
                }
            }
            Expr::BinaryOperation { lhs, operator, rhs } => {
                let (lhs_type_idx, rhs_type_idx) = (
                    self.infer_type(source_id, lhs, t_ast)?,
                    self.infer_type(source_id, rhs, t_ast)?,
                );

                match (
                    operator.kind,
                    (
                        &t_ast.types_arena[lhs_type_idx],
                        &t_ast.types_arena[rhs_type_idx],
                    ),
                ) {
                    (
                        BinaryOperatorKind::Add | BinaryOperatorKind::Sub | BinaryOperatorKind::Mul,
                        (Type::Int, Type::Int),
                    ) => Ok(t_ast.types_arena.insert(Type::Int)),

                    (
                        BinaryOperatorKind::Add
                        | BinaryOperatorKind::Sub
                        | BinaryOperatorKind::Mul
                        | BinaryOperatorKind::Div,
                        (Type::Int, Type::Int)
                        | (Type::Float, Type::Float)
                        | (Type::Float, Type::Int)
                        | (Type::Int, Type::Float),
                    ) => Ok(t_ast.types_arena.insert(Type::Float)),

                    (BinaryOperatorKind::Equal, (_, _)) => {
                        if Self::unify(t_ast, lhs_type_idx, rhs_type_idx).is_ok() {
                            Ok(t_ast.types_arena.insert(Type::Bool))
                        } else {
                            Err(TypecheckError::TypeMismatch {
                                source_id,
                                span: expr.span(),
                                expected: display_type(lhs_type_idx, t_ast),
                                found: display_type(rhs_type_idx, t_ast),
                            })
                        }
                    }

                    _ => Err(TypecheckError::IncompatibleOperands {
                        source_id,
                        span: expr.span(),
                        left: display_type(lhs_type_idx, t_ast),
                        right: display_type(rhs_type_idx, t_ast),
                    }),
                }
            }
            Expr::Assign {
                lhs: _,
                eq_token: _,
                value: _,
            } => Ok(t_ast.types_arena.insert(Type::Unit)),
            Expr::Function {
                fn_token: _,
                name: _,
                lpt: _,
                arguments,
                rpt: _,
                return_type_token: _,
                return_type_annotation,
                equals: _,
                body: _,
            } => {
                let fn_type = Type::Function(FunctionType {
                    arguments: arguments
                        .iter()
                        .map(|argument| {
                            (
                                argument.name.value().clone(),
                                t_ast
                                    .types_arena
                                    .insert(argument.type_annotation.value().clone().into()),
                            )
                        })
                        .collect(),
                    return_type: t_ast.types_arena.insert(
                        return_type_annotation
                            .as_ref()
                            .cloned()
                            .map(|Spanned(_, ty)| ty)
                            .unwrap_or(TypeAnnotation::Unit)
                            .into(),
                    ),
                });

                Ok(t_ast.types_arena.insert(fn_type))
            }
            Expr::Call {
                callee,
                left_paren: _,
                arguments: _,
                right_paren: _,
            } => match &**callee {
                Expr::Ident(spanned_str) => {
                    let Some(referred_to) = self.scopes.local(spanned_str.value()).copied() else {
                        return Err(TypecheckError::SymbolNotFound(
                            source_id,
                            spanned_str.clone(),
                        ));
                    };

                    let type_index = referred_to.into_type(t_ast);

                    if let Type::Function(FunctionType {
                        arguments: _,
                        return_type,
                    }) = &t_ast.types_arena[type_index]
                    {
                        Ok(*return_type)
                    } else {
                        Err(TypecheckError::CannotCallNonFunction(
                            source_id,
                            callee.span(),
                        ))
                    }
                }

                _ => Err(TypecheckError::CannotCallNonIdent(source_id, callee.span())),
            },

            Expr::Block { .. } => Ok(t_ast.types_arena.insert(Type::Unit)),
            Expr::Return(..) => Ok(t_ast.types_arena.insert(Type::Unit)),

            Expr::Poisoned => unreachable!(),
        }
    }

    fn visit_expr(
        &mut self,
        source_id: SourceId,
        expr: &Expr,
        t_ast: &mut TypedAst,
    ) -> Result<TypedExpression, TypecheckError> {
        let old_len = self.scopes.len();

        let t_expr = match expr {
            Expr::Int(value) => TypedExpression::Int(value.as_ref().map(|value| *value)),
            Expr::Float(value) => TypedExpression::Float(value.as_ref().map(|value| *value)),
            Expr::Bool(value) => TypedExpression::Bool(value.as_ref().map(|value| *value)),
            Expr::String(value) => TypedExpression::String(value.as_ref().map(Into::into)),
            Expr::Ident(spanned_ident) => {
                let Some(refers_to) = self.scopes.local(spanned_ident.value()).copied() else {
                    return Err(TypecheckError::SymbolNotFound(
                        source_id,
                        spanned_ident.clone(),
                    ));
                };

                let type_index = refers_to.into_type(t_ast);

                TypedExpression::Ident {
                    type_index,
                    refers_to,
                    str_value: spanned_ident.clone(),
                }
            }
            Expr::BinaryOperation { lhs, operator, rhs } => {
                self.infer_type(source_id, expr, t_ast)?;

                let (t_ast_lhs, t_ast_rhs) = (
                    self.visit_expr(source_id, lhs, t_ast)?,
                    self.visit_expr(source_id, rhs, t_ast)?,
                );

                TypedExpression::BinaryOperation {
                    lhs: Box::new(t_ast_lhs),
                    operator: operator.clone(),
                    rhs: Box::new(t_ast_rhs),
                }
            }
            Expr::Assign {
                lhs,
                eq_token: _,
                value,
            } => {
                let local_index = match &**lhs {
                    Expr::Ident(spanned_str) => match self.scopes.local(&spanned_str.1).copied() {
                        Some(RefersTo::Local(local_index)) => local_index,
                        Some(RefersTo::Type(_)) => {
                            return Err(TypecheckError::CannotAssignTo(source_id, lhs.span()))
                        }
                        None => {
                            let inferred_type = dbg!(self.infer_type(source_id, value, t_ast)?);

                            let local_index = dbg!(t_ast.locals.insert(inferred_type));

                            self.scopes
                                .insert_local(spanned_str.value(), RefersTo::Local(local_index));

                            local_index
                        }
                    },

                    _ => return Err(TypecheckError::CannotAssignTo(source_id, lhs.span())),
                };

                let (t_ast_lhs, t_ast_value) = (
                    self.visit_expr(source_id, lhs, t_ast)?,
                    self.visit_expr(source_id, value, t_ast)?,
                );

                let assignable = Assignable::try_from(t_ast_lhs).unwrap();

                TypedExpression::Assign {
                    local_index,
                    lhs: assignable,
                    value: Box::new(t_ast_value),
                }
            }
            Expr::Function {
                fn_token,
                name: spanned_name,
                lpt: _,
                arguments,
                rpt: _,
                return_type_token: _,
                return_type_annotation,
                equals: _,
                body,
            } => {
                let fn_type_index = self.infer_type(source_id, expr, t_ast)?;

                self.scopes
                    .insert_local(spanned_name.value(), RefersTo::Type(fn_type_index));

                let arguments = arguments
                    .iter()
                    .map(
                        |Argument {
                             name: Spanned(_, arg_name),
                             type_annotation,
                         }| {
                            (arg_name.into(), Type::from(type_annotation.value().clone()))
                        },
                    )
                    .map(|(name, ty)| (name, t_ast.types_arena.insert(ty)))
                    .collect::<BTreeMap<_, _>>();

                let return_type = Type::from(
                    return_type_annotation
                        .as_ref()
                        .cloned()
                        .map(|Spanned(_, ret)| ret)
                        .unwrap_or(TypeAnnotation::Unit),
                );

                let return_type = t_ast.types_arena.insert(return_type);

                self.scopes.enter_scope();

                for (k, v) in arguments.iter() {
                    let local_index = t_ast.locals.insert(*v);

                    self.scopes.insert_local(k, RefersTo::Local(local_index));
                }

                let t_ast_body = self.visit_expr(source_id, body, t_ast)?;

                self.scopes.leave_scope();

                let function = TypedFunctionExpr {
                    function_index: fn_type_index,
                    fn_token: fn_token.clone(),
                    name: spanned_name.as_ref().map(Into::into),
                    arguments,
                    return_type,
                    body: Box::new(t_ast_body),
                };

                if !matches!(&t_ast.types_arena[function.return_type], Type::Unit)
                    && matches!(&*function.body, TypedExpression::Block { .. })
                {
                    self.typecheck_function_body_returns(source_id, t_ast, &function)?;
                }

                TypedExpression::Function(function)
            }
            Expr::Call {
                callee,
                left_paren,
                arguments: call_arguments,
                right_paren,
            } => match &**callee {
                Expr::Ident(spanned_str) => {
                    let Some(refers_to) = self.scopes.local(spanned_str.value()) else {
                        return Err(TypecheckError::SymbolNotFound(
                            source_id,
                            spanned_str.clone(),
                        ));
                    };

                    let type_idx = dbg!(refers_to.into_type(t_ast));

                    let Type::Function(function) = t_ast.types_arena[type_idx].clone() else {
                        return Err(TypecheckError::CannotCallNonFunction(
                            source_id,
                            callee.span(),
                        ));
                    };

                    let (call_arguments_len, function_arguments_len) =
                        (call_arguments.len(), function.arguments.len());

                    match call_arguments_len.cmp(&function_arguments_len) {
                        Greater => {
                            return Err(TypecheckError::TooManyArguments {
                                source_id,
                                span: expr.span(),
                                call_arguments_len,
                                function_arguments_len,
                            })
                        }
                        Equal => {}
                        Less => {
                            return Err(TypecheckError::NotEnoughArguments {
                                source_id,
                                span: expr.span(),
                                call_arguments_len,
                                function_arguments_len,
                            })
                        }
                    }

                    for ((call_arg_span, call_arg), func_arg) in Iterator::zip(
                        call_arguments
                            .iter()
                            .map(|expr| {
                                self.infer_type(source_id, expr, t_ast)
                                    .map(|inferred| (expr.span(), inferred))
                            })
                            .collect::<Result<Vec<_>, _>>()?
                            .into_iter(),
                        function.arguments.values().copied(),
                    ) {
                        match Self::unify(t_ast, call_arg, func_arg) {
                            Ok(_) => {}
                            Err(_) => {
                                return Err(TypecheckError::TypeMismatch {
                                    source_id,
                                    span: call_arg_span,
                                    expected: display_type(func_arg, t_ast),
                                    found: display_type(call_arg, t_ast),
                                })
                            }
                        }
                    }

                    let mut arguments = vec![];

                    for call_arg in call_arguments.iter() {
                        let node = self.visit_expr(source_id, call_arg, t_ast)?;

                        arguments.push(node);
                    }

                    TypedExpression::Call {
                        callee: Callable::Function(type_idx, spanned_str.clone().map(Into::into)),
                        left_parent: left_paren.clone(),
                        arguments,
                        right_parent: right_paren.clone(),
                        return_type: function.return_type,
                    }
                }

                _ => return Err(TypecheckError::CannotCallNonIdent(source_id, callee.span())),
            },

            Expr::Return(return_token, ret_expr) => {
                let return_value_type = if let Some(ret_expr) = ret_expr {
                    self.infer_type(source_id, ret_expr, t_ast)?
                } else {
                    t_ast.types_arena.insert(Type::Unit)
                };

                let mut value = None;

                if let Some(ret_expr) = ret_expr {
                    let temp_val = self.visit_expr(source_id, ret_expr, t_ast)?;

                    value = Some(temp_val);
                }

                TypedExpression::Return(
                    return_value_type,
                    return_token.clone(),
                    value.map(Box::new),
                )
            }
            Expr::Block {
                left_brace,
                expressions,
                right_brace,
            } => TypedExpression::Block {
                left_brace: left_brace.clone(),
                expressions: expressions
                    .iter()
                    .map(|expr| self.visit_expr(source_id, expr, t_ast))
                    .collect::<Result<_, TypecheckError>>()?,
                right_brace: right_brace.clone(),
            },

            Expr::Poisoned => unreachable!(),
        };

        self.scopes.truncate(old_len);

        Ok(t_expr)
    }

    /// Rules for this algorithm:
    /// Detect if a function returns when the return type isnt `Type::Unit`
    /// assure that all branches of a function return
    fn typecheck_function_body_returns(
        &mut self,
        source_id: SourceId,
        t_ast: &mut TypedAst,
        function: &TypedFunctionExpr,
    ) -> Result<(), TypecheckError> {
        self.check_fn_body_branches(source_id, &function.body)?;
        Self::typecheck_fn_body_returns(source_id, t_ast, function.return_type, &function.body)?;

        Ok(())
    }

    fn typecheck_fn_body_returns(
        source_id: SourceId,
        t_ast: &mut TypedAst,
        expected_type: TypeIndex,
        expr: &TypedExpression,
    ) -> Result<(), TypecheckError> {
        use TypedExpression::*;

        match expr {
            Return(return_type_index, return_token, value) => {
                match Self::unify(t_ast, *return_type_index, expected_type) {
                    Ok(_) => Ok(()),
                    Err(_) => Err(TypecheckError::TypeMismatch {
                        source_id,
                        span: value
                            .as_ref()
                            .map(|expr| expr.span())
                            .unwrap_or(return_token.span()),
                        expected: display_type(expected_type, t_ast),
                        found: display_type(*return_type_index, t_ast),
                    }),
                }
            }

            Block { expressions, .. } => expressions.iter().try_for_each(|expr| {
                Self::typecheck_fn_body_returns(source_id, t_ast, expected_type, expr)
            }),

            _ => Ok(()),
        }
    }

    fn check_fn_body_branches(
        &mut self,
        source_id: SourceId,
        expr: &TypedExpression,
    ) -> Result<(), TypecheckError> {
        if let Err(span) = Self::check_branches_for_return(expr) {
            Err(TypecheckError::FunctionDoesntReturn {
                source_id,
                faulty_branch_position: span,
            })
        } else {
            Ok(())
        }
    }

    fn check_branches_for_return(expr: &TypedExpression) -> Result<(), Range<usize>> {
        use TypedExpression::*;

        match expr {
            Return(..) => Ok(()),

            Block { expressions, .. } => {
                let scope_has_return = expressions.iter().any(|expr| matches!(expr, Return(..)));

                if scope_has_return {
                    expressions
                        .iter()
                        .map(Self::check_branches_for_return)
                        .collect::<Result<Vec<_>, _>>()?;

                    Ok(())
                } else {
                    Err(expr.span())
                }
            }

            _ => Ok(()),
        }
    }
}
