#![allow(clippy::single_match)]

extern crate std;

use core::{
    cmp::Ordering::{Equal, Greater, Less},
    ops::Range,
};

use alloc::{
    boxed::Box,
    collections::BTreeMap,
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use frostbite_parser::ast::{tokens::TypeAnnotation, Argument, Expr, Program, Spannable, Spanned};
use frostbite_reports::{
    sourcemap::{SourceId, SourceMap},
    IntoReport, Level, Report, ReportContext,
};

use crate::tir::{Assignable, Callable, RefersTo, TirFunction, TirNode, TirTree, Type, TypeIndex};

#[derive(Debug)]
pub enum TypecheckError<'ast> {
    _TypeMismatch {
        source_id: SourceId,
        span: Range<usize>,

        expected: Type,
        found: Type,
    },
    SymbolNotFound(SourceId, Spanned<&'ast str>),
    IncompatibleOperands(SourceId, Range<usize>),
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
}

impl<'ast> IntoReport for TypecheckError<'ast> {
    fn into_report(self) -> Report {
        match self {
            TypecheckError::_TypeMismatch {
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
            TypecheckError::IncompatibleOperands(source_id, span) => Report::new_diagnostic(
                Level::Error,
                span,
                source_id,
                "Incompatible operands",
                None::<&str>,
                [],
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
        }
    }
}

pub fn check_types(
    report_ctx: &mut ReportContext,
    source_id: SourceId,
    _map: &SourceMap,
    ast: &Program<'_>,
    t_ir: &mut TirTree,
) {
    let mut rts = RecursiveTypechecker::new();

    for expr in &ast.exprs {
        let mut t_ir_node = TirNode::Uninitialized;

        if let Err(report) = rts.visit_expr(source_id, report_ctx, expr, &mut t_ir_node, t_ir) {
            report_ctx.push(report.into_report())
        }

        t_ir.nodes.push(match t_ir_node {
            TirNode::Uninitialized => TirNode::Poisoned,

            node => node,
        });
    }
}

struct RecursiveTypechecker {
    scopes: Vec<BTreeMap<String, RefersTo>>,
}

impl<'ast> RecursiveTypechecker {
    fn new() -> Self {
        Self {
            scopes: vec![BTreeMap::new()],
        }
    }

    fn infer_type(
        &mut self,
        source_id: SourceId,
        expr: &Expr<'ast>,
        t_ir_tree: &mut TirTree,
    ) -> Result<TypeIndex, TypecheckError<'ast>> {
        match expr {
            Expr::Int(_) => Ok(t_ir_tree.types_arena.insert(Type::Int)),
            Expr::Float(_) => Ok(t_ir_tree.types_arena.insert(Type::Float)),
            Expr::String(_) => Ok(t_ir_tree.types_arena.insert(Type::String)),
            Expr::Ident(spanned_str) => {
                let Some(referred_to) = self
                    .scopes
                    .iter()
                    .find(|scope| scope.contains_key(spanned_str.1))
                    .map(|scope| scope[spanned_str.1])
                else {
                    return Err(TypecheckError::SymbolNotFound(
                        source_id,
                        spanned_str.clone(),
                    ));
                };

                match referred_to {
                    RefersTo::Local(local_index) => Ok(t_ir_tree.locals[local_index]),
                    RefersTo::Type(type_index) => Ok(type_index),
                }
            }
            Expr::BinaryOperation {
                lhs,
                operator: _,
                rhs,
            } => {
                let (lhs_type_idx, rhs_type_idx) = (
                    self.infer_type(source_id, lhs, t_ir_tree)?,
                    self.infer_type(source_id, rhs, t_ir_tree)?,
                );

                match (
                    &t_ir_tree.types_arena[lhs_type_idx],
                    &t_ir_tree.types_arena[rhs_type_idx],
                ) {
                    (Type::Int, Type::Int) => Ok(t_ir_tree.types_arena.insert(Type::Int)),
                    (Type::Float, Type::Float)
                    | (Type::Float, Type::Int)
                    | (Type::Int, Type::Float) => Ok(t_ir_tree.types_arena.insert(Type::Float)),

                    _ => {
                        return Err(TypecheckError::IncompatibleOperands(source_id, expr.span()));
                    }
                }
            }
            Expr::Assign {
                lhs: _,
                eq_token: _,
                value: _,
            } => Ok(t_ir_tree.types_arena.insert(Type::Unit)),
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
                let fn_type = Type::Function(TirFunction {
                    arguments: arguments
                        .iter()
                        .map(|argument| {
                            (
                                argument.name.1.into(),
                                t_ir_tree
                                    .types_arena
                                    .insert(argument.type_annotation.1.into()),
                            )
                        })
                        .collect(),
                    return_type: t_ir_tree.types_arena.insert(
                        return_type_annotation
                            .as_ref()
                            .cloned()
                            .map(|Spanned(_, ty)| ty)
                            .unwrap_or(TypeAnnotation::Unit)
                            .into(),
                    ),
                });

                Ok(t_ir_tree.types_arena.insert(fn_type))
            }
            Expr::Call {
                callee,
                lpt: _,
                arguments: _,
                rpt: _,
            } => match &**callee {
                Expr::Ident(spanned_str) => {
                    let Some(referred_to) = self
                        .scopes
                        .iter()
                        .find_map(|scope| scope.get(spanned_str.1).copied())
                    else {
                        return Err(TypecheckError::SymbolNotFound(
                            source_id,
                            spanned_str.clone(),
                        ));
                    };

                    let type_index = referred_to.into_type(t_ir_tree);

                    if matches!(
                        &t_ir_tree.types_arena[type_index],
                        Type::Function(TirFunction {
                            arguments: _,
                            return_type: _,
                        }),
                    ) {
                        Ok(type_index)
                    } else {
                        Err(TypecheckError::CannotCallNonFunction(
                            source_id,
                            callee.span(),
                        ))
                    }
                }

                _ => Err(TypecheckError::CannotCallNonIdent(source_id, callee.span())),
            },

            Expr::Poisoned => unreachable!(),
        }
    }

    fn visit_expr(
        &mut self,
        source_id: SourceId,
        _report_ctx: &mut ReportContext,
        expr: &Expr<'ast>,
        t_ir_node_ptr: &mut TirNode,
        t_ir_tree: &mut TirTree,
    ) -> Result<(), TypecheckError<'ast>> {
        let t_ir_node = match expr {
            Expr::Int(value) => TirNode::Int(value.as_ref().map(|value| *value)),
            Expr::Float(value) => TirNode::Float(value.as_ref().map(|value| *value)),
            Expr::String(value) => TirNode::String(value.as_ref().map(|value| (*value).into())),
            Expr::Ident(spanned_ident) => {
                let Some(refers_to) = self
                    .scopes
                    .iter()
                    .find_map(|scope| scope.get(spanned_ident.1).copied())
                else {
                    return Err(TypecheckError::SymbolNotFound(
                        source_id,
                        spanned_ident.clone(),
                    ));
                };

                let type_idx = refers_to.into_type(t_ir_tree);

                TirNode::Ident {
                    ty: type_idx,
                    refers_to,
                    str_value: spanned_ident.clone().map(|name| name.into()),
                }
            }
            Expr::BinaryOperation { lhs, operator, rhs } => {
                let (inferred_lhs, inferred_rhs) = (
                    self.infer_type(source_id, lhs, t_ir_tree)?,
                    self.infer_type(source_id, rhs, t_ir_tree)?,
                );

                match (
                    &t_ir_tree.types_arena[inferred_lhs],
                    &t_ir_tree.types_arena[inferred_rhs],
                ) {
                    (Type::Int, Type::Int) => (),
                    (Type::Float, Type::Float) => (),
                    (Type::Float, Type::Int) => (),
                    (Type::Int, Type::Float) => (),

                    _ => {
                        return Err(TypecheckError::IncompatibleOperands(source_id, expr.span()));
                    }
                };

                let (t_ir_lhs, t_ir_rhs) = (TirNode::Uninitialized, TirNode::Uninitialized);

                self.visit_expr(source_id, _report_ctx, lhs, t_ir_node_ptr, t_ir_tree)?;
                self.visit_expr(source_id, _report_ctx, rhs, t_ir_node_ptr, t_ir_tree)?;

                TirNode::BinaryOperation {
                    lhs: Box::new(t_ir_lhs),
                    operator: operator.clone(),
                    rhs: Box::new(t_ir_rhs),
                }
            }
            Expr::Assign {
                lhs,
                eq_token: _,
                value,
            } => {
                match &**lhs {
                    Expr::Ident(spanned_str) => {
                        match self
                            .scopes
                            .iter()
                            .find_map(|scope| scope.get(spanned_str.1).copied())
                        {
                            Some(_) => {}
                            None => {
                                let inferred_type = self.infer_type(source_id, value, t_ir_tree)?;

                                let local_index = t_ir_tree.locals.insert(inferred_type);

                                self.scopes
                                    .first_mut()
                                    .unwrap()
                                    .insert(spanned_str.1.into(), RefersTo::Local(local_index));
                            }
                        }
                    }

                    _ => return Err(TypecheckError::CannotAssignTo(source_id, lhs.span())),
                }

                let (mut t_ir_lhs, mut t_ir_value) =
                    (TirNode::Uninitialized, TirNode::Uninitialized);

                self.visit_expr(source_id, _report_ctx, lhs, &mut t_ir_lhs, t_ir_tree)?;
                self.visit_expr(source_id, _report_ctx, value, &mut t_ir_value, t_ir_tree)?;

                let assignable = Assignable::try_from(t_ir_lhs).unwrap();

                TirNode::Assign {
                    lhs: assignable,
                    value: Box::new(t_ir_value),
                }
            }
            Expr::Function {
                fn_token: _,
                name,
                lpt: _,
                arguments,
                rpt: _,
                return_type_token: _,
                return_type_annotation,
                equals: _,
                body,
            } => {
                let fn_type = self.infer_type(source_id, expr, t_ir_tree)?;

                match name {
                    Some(Spanned(_, name)) => {
                        self.scopes
                            .first_mut()
                            .unwrap()
                            .insert((*name).into(), fn_type.into());
                    }
                    None => {}
                }

                let arguments = arguments
                    .iter()
                    .map(
                        |Argument {
                             name: Spanned(_, arg_name),
                             type_annotation,
                         }| {
                            (arg_name.to_string(), Type::from(type_annotation.1))
                        },
                    )
                    .map(|(name, ty)| (name, t_ir_tree.types_arena.insert(ty)))
                    .collect::<BTreeMap<_, _>>();

                let return_type = Type::from(
                    return_type_annotation
                        .as_ref()
                        .cloned()
                        .map(|Spanned(_, ret)| ret)
                        .unwrap_or(TypeAnnotation::Unit),
                );

                let return_type = t_ir_tree.types_arena.insert(return_type);

                let mut t_ir_body = TirNode::Uninitialized;

                self.scopes.push(BTreeMap::default());

                for (k, v) in arguments.iter() {
                    self.scopes
                        .first_mut()
                        .unwrap()
                        .insert(k.clone(), (*v).into());
                }

                self.visit_expr(source_id, _report_ctx, body, &mut t_ir_body, t_ir_tree)?;

                self.scopes.pop();

                TirNode::Function {
                    name: name
                        .as_ref()
                        .map(|spanned_str| spanned_str.as_ref().map(ToString::to_string)),
                    arguments,
                    return_type,
                    body: Box::new(t_ir_body),
                }
            }
            Expr::Call {
                callee,
                lpt: _,
                arguments: call_arguments,
                rpt: _,
            } => match &**callee {
                Expr::Ident(spanned_str) => {
                    let Some(refers_to) = self
                        .scopes
                        .iter()
                        .find_map(|scope| scope.get(spanned_str.1).cloned())
                    else {
                        return Err(TypecheckError::SymbolNotFound(
                            source_id,
                            spanned_str.clone(),
                        ));
                    };

                    let type_idx = refers_to.into_type(t_ir_tree);

                    let Type::Function(function) = &t_ir_tree.types_arena[type_idx].clone() else {
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

                    let mut arguments = vec![];

                    for call_arg in call_arguments.iter() {
                        let mut node = TirNode::Uninitialized;

                        self.visit_expr(source_id, _report_ctx, call_arg, &mut node, t_ir_tree)?;

                        arguments.push(node);
                    }

                    TirNode::Call {
                        callee: Callable::Ident(type_idx, spanned_str.clone().map(Into::into)),
                        arguments,
                        return_type: function.return_type,
                    }
                }

                _ => return Err(TypecheckError::CannotCallNonIdent(source_id, callee.span())),
            },
            Expr::Poisoned => todo!(),
        };

        *t_ir_node_ptr = t_ir_node;

        Ok(())
    }
}
