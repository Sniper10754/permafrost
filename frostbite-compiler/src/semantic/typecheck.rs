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
    TypeMismatch {
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
            Expr::BinaryOperation { lhs, operator: _, rhs } => {
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
                name,
                lpt: _,
                arguments,
                rpt: _,
                return_type_token: _,
                return_type_annotation,
                equals: _,
                body: _,
            } => {
                if name.is_some() {
                    Ok(Type::Unit)
                } else {
                    Ok(Type::Function(TirFunction {
                        arguments: arguments
                            .into_iter()
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
                    }))
                }
            }
            .map(|ty| t_ir_tree.types_arena.insert(ty)),
            Expr::Call {
                callee,
                lpt: _,
                arguments: _,
                rpt: _,
            } => {
                match &**callee {
                    Expr::Ident(spanned_str) => {
                        let Some(referred_to) = self.scopes.iter().find_map(|scope| {
                            scope.get(spanned_str.1).copied()
                        }) else {
                            return Err(TypecheckError::SymbolNotFound(
                                source_id,
                                spanned_str.clone(),
                            ));
                        };

                        let type_index = match referred_to {
                            RefersTo::Local(local) => t_ir_tree.locals[local],
                            RefersTo::Type(type_idx) => type_idx,
                        };

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
                }
            }

            Expr::Poisoned => unreachable!(),
        }
    }

    fn visit_expr(
        &mut self,
        source_id: SourceId,
        report_ctx: &mut ReportContext,
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
                    .find_map(|scope| scope.get(spanned_ident.1).map(|value| *value))
                else {
                    return Err(TypecheckError::SymbolNotFound(
                        source_id,
                        spanned_ident.clone(),
                    ));
                };

                let r#type = match refers_to {
                    RefersTo::Local(local_idx) => t_ir_tree.locals[local_idx],
                    RefersTo::Type(type_idx) => type_idx,
                };

                TirNode::Ident {
                    r#type,
                    refers_to,
                    str_value: spanned_ident.clone().map(|str| str.into()),
                }
            }
            Expr::BinaryOperation { lhs, operator, rhs } => {
                match (&**lhs, &**rhs) {
                    (Expr::Int(Spanned(_, _)), Expr::Int(Spanned(_, _))) => (),
                    (Expr::Float(Spanned(_, _)), Expr::Float(Spanned(_, _))) => (),
                    (Expr::Float(Spanned(_, _)), Expr::Int(Spanned(_, _))) => (),
                    (Expr::Int(Spanned(_, _)), Expr::Float(Spanned(_, _))) => (),

                    _ => {
                        return Err(TypecheckError::IncompatibleOperands(source_id, expr.span()));
                    }
                };

                let (t_ir_lhs, t_ir_rhs) = (TirNode::Uninitialized, TirNode::Uninitialized);

                self.visit_expr(source_id, report_ctx, lhs, t_ir_node_ptr, t_ir_tree)?;
                self.visit_expr(source_id, report_ctx, rhs, t_ir_node_ptr, t_ir_tree)?;

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
                let (t_ir_lhs, t_ir_value) = (TirNode::Uninitialized, TirNode::Uninitialized);

                self.visit_expr(source_id, report_ctx, lhs, t_ir_node_ptr, t_ir_tree)?;
                self.visit_expr(source_id, report_ctx, value, t_ir_node_ptr, t_ir_tree)?;

                let Ok(assignable) = Assignable::try_from(t_ir_lhs) else {
                    return Err(TypecheckError::CannotAssignTo(source_id, lhs.span()));
                };

                match &assignable {
                    Assignable::Ident(r#type, str_value) => {
                        self.scopes
                            .first_mut()
                            .unwrap()
                            .insert(str_value.1.clone(), RefersTo::Type(*r#type));

                        TirNode::Assign {
                            lhs: assignable,
                            value: Box::new(t_ir_value),
                        }
                    }
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
                body: _,
            } => {
                let fn_type = self.infer_type(source_id, expr, t_ir_tree)?;

                match name {
                    Some(Spanned(_, name)) => {
                        self.scopes
                            .first_mut()
                            .unwrap()
                            .insert((*name).into(), RefersTo::Type(fn_type));
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
                    .collect();

                let return_type = Type::from(
                    return_type_annotation
                        .as_ref()
                        .cloned()
                        .map(|Spanned(_, ret)| ret)
                        .unwrap_or(TypeAnnotation::Unit),
                );

                let return_type = t_ir_tree.types_arena.insert(return_type);

                let mut body = TirNode::Uninitialized;

                self.visit_expr(source_id, report_ctx, expr, &mut body, t_ir_tree)?;

                TirNode::Function {
                    name: name
                        .as_ref()
                        .map(|spanned_str| spanned_str.as_ref().map(ToString::to_string)),
                    arguments,
                    return_type,
                    body: Box::new(body),
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

                    let type_idx = match refers_to {
                        RefersTo::Local(local_idx) => t_ir_tree.locals[local_idx],
                        RefersTo::Type(type_idx) => type_idx,
                    };

                    let Type::Function(function) = &t_ir_tree.types_arena[type_idx] else {
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

                    let arguments = vec![];

                    let return_type = function.return_type;

                    TirNode::Call {
                        callee: Callable::Ident(type_idx, spanned_str.clone().map(Into::into)),
                        arguments,
                        return_type,
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
