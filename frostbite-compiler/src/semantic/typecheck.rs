#![allow(clippy::single_match)]

extern crate std;

use core::{
    cmp::Ordering::{Equal, Greater, Less},
    ops::Range,
};

use alloc::{
    borrow::Cow,
    boxed::Box,
    collections::BTreeMap,
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use frostbite_parser::ast::{
    tokens::{BinaryOperatorKind, TypeAnnotation},
    Argument, Expr, Program, Spannable, Spanned,
};
use frostbite_reports::{
    sourcemap::{SourceId, SourceMap},
    IntoReport, Label, Level, Report, ReportContext,
};

use crate::tir::{
    display::display_type, Assignable, Callable, FunctionType, RefersTo, Type, TypeIndex, TypedAst,
    TypedExpression, TypedFunctionExpr,
};

#[derive(Debug)]
pub enum TypecheckError<'ast> {
    TypeMismatch {
        source_id: SourceId,
        span: Range<usize>,

        expected: Cow<'ast, str>,
        found: Cow<'ast, str>,
    },
    SymbolNotFound(SourceId, Spanned<&'ast str>),
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

pub fn check_types(
    report_ctx: &mut ReportContext,
    source_id: SourceId,
    _map: &SourceMap,
    ast: &Program<'_>,
) -> TypedAst {
    let mut rts = RecursiveTypechecker::new();
    let mut typed_ast = TypedAst::default();

    for expr in &ast.exprs {
        let mut t_ast_node = TypedExpression::Uninitialized;

        if let Err(report) = rts.visit_expr(source_id, expr, &mut t_ast_node, &mut typed_ast) {
            report_ctx.push(report.into_report())
        }

        typed_ast.nodes.push(t_ast_node);
    }

    typed_ast
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

    fn unify(&mut self, t_ast: &mut TypedAst, a: TypeIndex, b: TypeIndex) -> Result<(), ()> {
        match (&t_ast.types_arena[a], &t_ast.types_arena[b]) {
            (a, b) if a == b => Ok(()),

            _ => Err(()),
        }
    }

    fn infer_type(
        &mut self,
        source_id: SourceId,
        expr: &Expr<'ast>,
        t_ast: &mut TypedAst,
    ) -> Result<TypeIndex, TypecheckError<'ast>> {
        match expr {
            Expr::Int(_) => Ok(t_ast.types_arena.insert(Type::Int)),
            Expr::Float(_) => Ok(t_ast.types_arena.insert(Type::Float)),
            Expr::String(_) => Ok(t_ast.types_arena.insert(Type::String)),
            Expr::Bool(_) => Ok(t_ast.types_arena.insert(Type::Bool)),
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
                    ) => Ok(t_ast.types_arena.insert(Type::Int)),

                    (BinaryOperatorKind::Equal, (_, _)) => Ok(t_ast.types_arena.insert(Type::Bool)),

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
                                argument.name.1.into(),
                                t_ast.types_arena.insert(argument.type_annotation.1.into()),
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
                left_paren,
                arguments: _,
                right_paren,
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
        expr: &Expr<'ast>,
        t_ast_node_ptr: &mut TypedExpression,
        t_ast: &mut TypedAst,
    ) -> Result<(), TypecheckError<'ast>> {
        let t_ast_node = match expr {
            Expr::Int(value) => TypedExpression::Int(value.as_ref().map(|value| *value)),
            Expr::Float(value) => TypedExpression::Float(value.as_ref().map(|value| *value)),
            Expr::Bool(value) => TypedExpression::Bool(value.as_ref().map(|value| *value)),
            Expr::String(value) => {
                TypedExpression::String(value.as_ref().map(|value| (*value).into()))
            }
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

                let type_index = refers_to.into_type(t_ast);

                TypedExpression::Ident {
                    type_index,
                    refers_to,
                    str_value: spanned_ident.clone().map(|name| name.into()),
                }
            }
            Expr::BinaryOperation { lhs, operator, rhs } => {
                let (inferred_lhs, inferred_rhs) = (
                    self.infer_type(source_id, lhs, t_ast)?,
                    self.infer_type(source_id, rhs, t_ast)?,
                );

                match (
                    operator.kind,
                    (
                        &t_ast.types_arena[inferred_lhs],
                        &t_ast.types_arena[inferred_rhs],
                    ),
                ) {
                    (
                        BinaryOperatorKind::Add
                        | BinaryOperatorKind::Sub
                        | BinaryOperatorKind::Mul
                        | BinaryOperatorKind::Div,
                        (Type::Int, Type::Int)
                        | (Type::Float, Type::Float)
                        | (Type::Float, Type::Int)
                        | (Type::Int, Type::Float),
                    ) => (),

                    (BinaryOperatorKind::Equal, (_, _)) => (),

                    _ => {
                        return Err(TypecheckError::IncompatibleOperands {
                            source_id,
                            span: expr.span(),
                            left: display_type(inferred_lhs, t_ast),
                            right: display_type(inferred_rhs, t_ast),
                        });
                    }
                };

                let (mut t_ast_lhs, mut t_ast_rhs) = (
                    TypedExpression::Uninitialized,
                    TypedExpression::Uninitialized,
                );

                self.visit_expr(source_id, lhs, &mut t_ast_lhs, t_ast)?;
                self.visit_expr(source_id, rhs, &mut t_ast_rhs, t_ast)?;

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
                    Expr::Ident(spanned_str) => {
                        match self
                            .scopes
                            .iter()
                            .find_map(|scope| scope.get(spanned_str.1).copied())
                        {
                            Some(RefersTo::Local(local_index)) => local_index,
                            Some(RefersTo::Type(_)) => {
                                return Err(TypecheckError::CannotAssignTo(source_id, lhs.span()))
                            }
                            None => {
                                let inferred_type = self.infer_type(source_id, value, t_ast)?;

                                let local_index = t_ast.locals.insert(inferred_type);

                                self.scopes
                                    .first_mut()
                                    .unwrap()
                                    .insert(spanned_str.1.into(), RefersTo::Local(local_index));

                                local_index
                            }
                        }
                    }

                    _ => return Err(TypecheckError::CannotAssignTo(source_id, lhs.span())),
                };

                let (mut t_ast_lhs, mut t_ast_value) = (
                    TypedExpression::Uninitialized,
                    TypedExpression::Uninitialized,
                );

                self.visit_expr(source_id, lhs, &mut t_ast_lhs, t_ast)?;
                self.visit_expr(source_id, value, &mut t_ast_value, t_ast)?;

                let assignable = Assignable::try_from(t_ast_lhs).unwrap();

                TypedExpression::Assign {
                    local_index,
                    lhs: assignable,
                    value: Box::new(t_ast_value),
                }
            }
            Expr::Function {
                fn_token,
                name,
                lpt: _,
                arguments,
                rpt: _,
                return_type_token: _,
                return_type_annotation,
                equals: _,
                body,
            } => {
                let fn_type_index = self.infer_type(source_id, expr, t_ast)?;

                match name {
                    Some(Spanned(_, name)) => {
                        self.scopes
                            .first_mut()
                            .unwrap()
                            .insert((*name).into(), RefersTo::Type(fn_type_index));
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

                let mut t_ast_body = TypedExpression::Uninitialized;

                self.scopes.push(BTreeMap::default());

                for (k, v) in arguments.iter() {
                    let local_index = t_ast.locals.insert(*v);

                    self.scopes
                        .first_mut()
                        .unwrap()
                        .insert(k.clone(), RefersTo::Local(local_index));
                }

                self.visit_expr(source_id, body, &mut t_ast_body, t_ast)?;

                self.scopes.pop();

                let function = TypedFunctionExpr {
                    fn_token: fn_token.clone(),
                    type_index: fn_type_index,
                    name: name
                        .as_ref()
                        .map(|spanned_str| spanned_str.as_ref().map(ToString::to_string)),
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

                    let type_idx = refers_to.into_type(t_ast);

                    let Type::Function(function) = &t_ast.types_arena[type_idx].clone() else {
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
                        let mut node = TypedExpression::Uninitialized;

                        self.visit_expr(source_id, call_arg, &mut node, t_ast)?;

                        arguments.push(node);
                    }

                    TypedExpression::Call {
                        callee: Callable::Ident(type_idx, spanned_str.clone().map(Into::into)),
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
                    let mut temp_val = TypedExpression::Uninitialized;

                    self.visit_expr(source_id, ret_expr, &mut temp_val, t_ast)?;

                    value = Some(temp_val);
                }

                TypedExpression::Return(
                    return_value_type,
                    return_token.clone(),
                    value.map(|val| Box::new(val)),
                )
            }
            Expr::Block {
                span: _,
                expressions,
            } => TypedExpression::Block {
                expressions: expressions
                    .iter()
                    .map(|expr| (TypedExpression::Uninitialized, expr))
                    .map(|(mut t_ast_node, expr)| {
                        self.visit_expr(source_id, expr, &mut t_ast_node, t_ast)?;
                        Ok(t_ast_node)
                    })
                    .collect::<Result<_, TypecheckError<'ast>>>()?,
            },

            Expr::Poisoned => unreachable!(),
            Expr::Block {
                span: _,
                expressions,
            } => {
                self.scopes.push(BTreeMap::default());

                let block = TypedExpression::Block {
                    expressions: expressions
                        .iter()
                        .map(|expr| {
                            let mut temp = TypedExpression::Uninitialized;

                            self.visit_expr(source_id, expr, &mut temp, t_ast);

                            temp
                        })
                        .collect(),
                };

                self.scopes.pop();

                block
            }

            Expr::Return(_, _) => todo!(),
        };

        assert!(
            (!t_ast_node.is_uninitialized()) && (!t_ast_node.is_poisoned()),
            "{source_id}, {t_ast_node:#?}",
        );

        *t_ast_node_ptr = t_ast_node;

        Ok(())
    }

    /// Rules for this algorithm:
    /// Detect if a function returns when the return type isnt `Type::Unit`
    /// assure that all branches of a function return
    fn typecheck_function_body_returns(
        &mut self,
        source_id: SourceId,
        t_ast: &mut TypedAst,
        function: &TypedFunctionExpr,
    ) -> Result<(), TypecheckError<'ast>> {
        self.__typecheck_function_body_returns(source_id, t_ast, &function.body, function)
    }

    fn __typecheck_function_body_returns(
        &mut self,
        source_id: SourceId,
        t_ast: &mut TypedAst,
        expr: &TypedExpression,
        function: &TypedFunctionExpr,
    ) -> Result<(), TypecheckError<'ast>> {
        if let Err(span) = Self::__check_branches_for_return(expr) {
            Err(TypecheckError::TypeMismatch {
                source_id,
                span,
                expected: display_type(function.return_type, t_ast),
                found: display_type(t_ast.types_arena.insert(Type::Unit), t_ast),
            })
        } else {
            Ok(())
        }
    }

    fn __check_branches_for_return(expr: &TypedExpression) -> Result<(), Range<usize>> {
        use TypedExpression::*;

        match expr {
            Return(..) => Ok(()),

            Block { expressions } => {
                let scope_has_return = expressions.iter().any(|expr| matches!(expr, Return(..)));

                if scope_has_return {
                    expressions
                        .iter()
                        .map(|expr| Self::__check_branches_for_return(expr))
                        .collect::<Result<Vec<_>, _>>()?;

                    Ok(())
                } else {
                    Err(expr.span())
                }
            }

            _ => Err(expr.span()),
        }
    }
}
