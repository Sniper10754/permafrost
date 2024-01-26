#![allow(clippy::too_many_arguments)]

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
    tokens::{
        BinaryOperatorKind, FunctionToken, LeftBraceToken, LeftParenthesisToken, Operator,
        ReturnToken, RightBraceToken, RightParenthesisToken, TypeAnnotation,
    },
    Spannable, Spanned,
};

use frostbite_reports::{sourcemap::SourceKey, IntoReport, Label, Level, Report};
use slotmap::SecondaryMap;

use crate::{
    ir::{
        named::{Argument, Assignable as NamedAssignable, LocalKey, NamedExpr},
        typed::{
            display::display_type, Assignable, Callable, FunctionType, Type, TypeKey, TypedAst,
            TypedExpression, TypedExpressionKind, TypedFunction,
        },
    },
    Compiler,
};

#[derive(Debug)]
enum TypecheckError
{
    TypeMismatch
    {
        source_key: SourceKey,
        span: Range<usize>,

        expected: Cow<'static, str>,
        found: Cow<'static, str>,
    },
    IncompatibleOperands
    {
        source_key: SourceKey,
        span: Range<usize>,

        left: Cow<'static, str>,
        right: Cow<'static, str>,
    },
    CannotCallNonIdent(SourceKey, Range<usize>),
    CannotCallNonFunction(SourceKey, Range<usize>),
    TooManyArguments
    {
        source_key: SourceKey,
        span: Range<usize>,

        call_arguments_len: usize,
        function_arguments_len: usize,
    },
    NotEnoughArguments
    {
        source_key: SourceKey,
        span: Range<usize>,

        call_arguments_len: usize,
        function_arguments_len: usize,
    },
    FunctionDoesntReturn
    {
        source_key: SourceKey,
        faulty_branch_position: Range<usize>,
    },
}

impl IntoReport for TypecheckError
{
    fn into_report(self) -> Report
    {
        match self {
            TypecheckError::TypeMismatch {
                source_key,
                span,
                expected,
                found,
            } => Report::new(
                Level::Error,
                span,
                source_key,
                "Type mismatch",
                Some(format!("Expected type {expected:?}, found type {found:?}")),
                [],
                [],
            ),
            TypecheckError::IncompatibleOperands { source_key, span, left, right } => Report::new(
                Level::Error,
                span.clone(),
                source_key,
                "Incompatible operands",
                None::<&str>,
                [
                    Label::new(format!("Left type is {left}"), span.clone(), source_key),
                    Label::new(format!("Right type is {right}"), span.clone(), source_key),
                ],
                [],
            ),
            TypecheckError::CannotCallNonIdent(source_key, span) => Report::new(
                Level::Error,
                span,
                source_key,
                "Cannot call expression",
                Some("Only identifiers may be called"),
                [],
                [],
            ),
            TypecheckError::CannotCallNonFunction(source_key, span) => Report::new(
                Level::Error,
                span,
                source_key,
                "Cannot call non function",
                Some("Only functions may be called"),
                [],
                [],
            ),
            TypecheckError::TooManyArguments {
                source_key,
                span,
                call_arguments_len: call_arguments,
                function_arguments_len: function_arguments,
            } => Report::new(
                Level::Error,
                span,
                source_key,
                "Too many arguments",
                Some(format!(
                    "Function expected {function_arguments} arguments, but was called with {call_arguments} arguments"
                )),
                [],
                [],
            ),
            TypecheckError::NotEnoughArguments {
                source_key,
                span,
                call_arguments_len,
                function_arguments_len,
            } => Report::new(
                Level::Error,
                span,
                source_key,
                "Not enough arguments",
                Some(format!(
                    "Function expected {function_arguments_len} arguments, but was called with {call_arguments_len} arguments"
                )),
                [],
                [],
            ),
            TypecheckError::FunctionDoesntReturn { source_key, faulty_branch_position } => Report::new(Level::Error, faulty_branch_position, source_key, "One branch of this function doesnt return", Some("This branch of this function doesnt return."), [], []),
        }
    }
}

pub fn check_types(
    compiler: &mut Compiler,
    source_key: SourceKey,
)
{
    compiler
        .ctx
        .type_ctx
        .t_asts
        .insert(source_key, TypedAst::default());

    // compiler.ctx
    //     .intrinsic_ctx
    //     .symbols
    //     .iter()
    //     .map(|(name, ty)| (name, *ty))
    //     .for_each(|(name, type_key)| rts.scopes.insert_local(name, RefersTo::Type(type_key)));

    let mut rts = RecursiveTypechecker {
        locals_to_types: SecondaryMap::new(),
        compiler,
    };

    let ast = rts.compiler.ctx.named_ctx.named_asts[source_key]
        .exprs
        .clone()
        .into_iter();

    for ref expr in ast {
        let result = rts.visit_expr(source_key, expr);

        match result {
            Ok(t_expr) => rts.compiler.ctx.type_ctx.t_asts[source_key]
                .nodes
                .push(t_expr),
            Err(report) => rts.compiler.ctx.report_ctx.push(report.into_report()),
        }
    }
}

struct RecursiveTypechecker<'compiler>
{
    pub locals_to_types: SecondaryMap<LocalKey, TypeKey>,
    pub compiler: &'compiler mut Compiler,
}

impl<'a> RecursiveTypechecker<'a>
{
    fn unify(
        &mut self,
        a: TypeKey,
        b: TypeKey,
    ) -> Result<(), ()>
    {
        match (
            self.compiler.ctx.type_ctx.types_arena[a].clone(),
            self.compiler.ctx.type_ctx.types_arena[b].clone(),
        ) {
            (Type::Object(left), Type::Object(right)) if left == right => Ok(()),

            (Type::Function(left_fn_type), Type::Function(right_fn_type)) => {
                Iterator::zip(
                    left_fn_type.arguments.values().copied(),
                    right_fn_type.arguments.values().copied(),
                )
                .try_for_each(|(left, right)| self.unify(left, right))?;

                Ok(())
            }

            (Type::Unit, Type::Unit) => Ok(()),
            (Type::Int, Type::Int) => Ok(()),
            (Type::Float, Type::Float) => Ok(()),
            (Type::Bool, Type::Bool) => Ok(()),

            (Type::Any, _) => Ok(()),
            (_, Type::Any) => Ok(()),

            _ => Err(()),
        }
    }

    fn infer_type(
        &mut self,
        source_key: SourceKey,
        expr: &NamedExpr,
    ) -> Result<TypeKey, TypecheckError>
    {
        match expr {
            NamedExpr::Int(_) => Ok(self.compiler.ctx.type_ctx.types_arena.insert(Type::Int)),
            NamedExpr::Float(_) => Ok(self.compiler.ctx.type_ctx.types_arena.insert(Type::Float)),
            NamedExpr::String(_) => Ok(self.compiler.ctx.type_ctx.types_arena.insert(Type::String)),
            NamedExpr::Bool(_) => Ok(self.compiler.ctx.type_ctx.types_arena.insert(Type::Bool)),

            NamedExpr::Ident {
                local_key,
                identifier: _,
            } => self.infer_ident(*local_key),
            NamedExpr::BinaryOperation { lhs, operator, rhs } => {
                self.infer_binary_op(source_key, lhs, operator, rhs, expr.span())
            }
            NamedExpr::Assign { lhs: _, value: _ } => {
                Ok(self.compiler.ctx.type_ctx.types_arena.insert(Type::Unit))
            }
            NamedExpr::Function {
                local_key: _,
                fn_token: _,
                name,
                arguments,
                return_type_token: _,
                return_type_annotation,
                body: _,
            } => self.infer_function(
                source_key,
                name.as_ref().map(|spanned| spanned.as_deref()),
                arguments,
                return_type_annotation
                    .as_ref()
                    .map(|spanned| spanned.as_ref()),
            ),
            NamedExpr::Call {
                callee,
                left_paren: _,
                arguments: _,
                right_paren: _,
            } => self.infer_call(source_key, callee),

            NamedExpr::Block { .. } => {
                Ok(self.compiler.ctx.type_ctx.types_arena.insert(Type::Unit))
            }
            NamedExpr::Return(..) => Ok(self.compiler.ctx.type_ctx.types_arena.insert(Type::Unit)),

            NamedExpr::Poisoned => unreachable!(),
        }
    }

    fn infer_ident(
        &mut self,
        local_key: LocalKey,
    ) -> Result<TypeKey, TypecheckError>
    {
        Ok(self.locals_to_types[local_key])
    }

    fn infer_binary_op(
        &mut self,
        source_key: SourceKey,
        lhs: &NamedExpr,
        operator: &Operator,
        rhs: &NamedExpr,
        span: Range<usize>,
    ) -> Result<TypeKey, TypecheckError>
    {
        let (lhs_type_key, rhs_type_key) = (
            self.infer_type(source_key, lhs)?,
            self.infer_type(source_key, rhs)?,
        );

        match (
            operator.kind,
            (
                &self.compiler.ctx.type_ctx.types_arena[lhs_type_key],
                &self.compiler.ctx.type_ctx.types_arena[rhs_type_key],
            ),
        ) {
            (
                BinaryOperatorKind::Add | BinaryOperatorKind::Sub | BinaryOperatorKind::Mul,
                (Type::Int, Type::Int),
            ) => Ok(self.compiler.ctx.type_ctx.types_arena.insert(Type::Int)),

            (
                BinaryOperatorKind::Add
                | BinaryOperatorKind::Sub
                | BinaryOperatorKind::Mul
                | BinaryOperatorKind::Div,
                (Type::Float, Type::Float) | (Type::Float, Type::Int) | (Type::Int, Type::Float),
            ) => Ok(self.compiler.ctx.type_ctx.types_arena.insert(Type::Float)),

            (BinaryOperatorKind::Equal, (..)) => {
                if self.unify(lhs_type_key, rhs_type_key).is_ok() {
                    Ok(self.compiler.ctx.type_ctx.types_arena.insert(Type::Bool))
                } else {
                    Err(TypecheckError::TypeMismatch {
                        source_key,
                        span,
                        expected: display_type(
                            lhs_type_key,
                            &self.compiler.ctx.type_ctx.types_arena,
                        ),
                        found: display_type(rhs_type_key, &self.compiler.ctx.type_ctx.types_arena),
                    })
                }
            }

            _ => Err(TypecheckError::IncompatibleOperands {
                source_key,
                span,
                left: display_type(lhs_type_key, &self.compiler.ctx.type_ctx.types_arena),
                right: display_type(rhs_type_key, &self.compiler.ctx.type_ctx.types_arena),
            }),
        }
    }

    fn infer_function(
        &mut self,
        _source_key: SourceKey,
        _name: Option<Spanned<&str>>,
        arguments: &[Argument],
        return_type_annotation: Option<Spanned<&TypeAnnotation>>,
    ) -> Result<TypeKey, TypecheckError>
    {
        let fn_type = {
            let return_type = self.compiler.ctx.type_ctx.types_arena.insert(
                return_type_annotation
                    .map(|Spanned(_, ty)| ty)
                    .cloned()
                    .unwrap_or(TypeAnnotation::Unit)
                    .into(),
            );

            let arguments = arguments
                .iter()
                .map(
                    |Argument {
                         local_key: _,

                         name: Spanned(_, name),
                         type_annotation: Spanned(_, type_annotation),
                     }| {
                        (
                            name.clone(),
                            self.compiler
                                .ctx
                                .type_ctx
                                .types_arena
                                .insert(type_annotation.clone().into()),
                        )
                    },
                )
                .collect();

            Type::Function(FunctionType {
                arguments,
                return_type,
            })
        };

        Ok(self.compiler.ctx.type_ctx.types_arena.insert(fn_type))
    }

    fn infer_call(
        &mut self,
        source_key: SourceKey,
        callee: &NamedExpr,
    ) -> Result<TypeKey, TypecheckError>
    {
        match callee {
            NamedExpr::Ident {
                local_key,
                identifier: _,
            } => {
                let type_key = dbg!(&self.locals_to_types)[*dbg!(local_key)];

                if let Type::Function(FunctionType {
                    arguments: _,
                    return_type,
                }) = &self.compiler.ctx.type_ctx.types_arena[type_key]
                {
                    Ok(*return_type)
                } else {
                    Err(TypecheckError::CannotCallNonFunction(
                        source_key,
                        callee.span(),
                    ))
                }
            }

            _ => Err(TypecheckError::CannotCallNonIdent(
                source_key,
                callee.span(),
            )),
        }
    }

    fn visit_expr(
        &mut self,
        source_key: SourceKey,
        expr: &NamedExpr,
    ) -> Result<TypedExpression, TypecheckError>
    {
        let typed_expression = match expr {
            NamedExpr::Int(value) => Ok(TypedExpression {
                type_key: self.infer_type(source_key, expr)?,
                kind: TypedExpressionKind::Int(value.clone()),
            }),
            NamedExpr::Float(value) => Ok(TypedExpression {
                type_key: self.infer_type(source_key, expr)?,
                kind: TypedExpressionKind::Float(value.clone()),
            }),
            NamedExpr::Bool(value) => Ok(TypedExpression {
                type_key: self.infer_type(source_key, expr)?,
                kind: TypedExpressionKind::Bool(value.clone()),
            }),
            NamedExpr::String(value) => Ok(TypedExpression {
                type_key: self.infer_type(source_key, expr)?,
                kind: TypedExpressionKind::String(value.as_ref().map(Clone::clone)),
            }),
            NamedExpr::Ident {
                local_key,
                identifier: Spanned(span, ident),
            } => self.visit_ident(*local_key, span, ident),
            NamedExpr::BinaryOperation { lhs, operator, rhs } => {
                self.visit_binary_op(source_key, expr, lhs, operator.clone(), rhs)
            }
            NamedExpr::Assign { lhs, value } => self.visit_assign(source_key, lhs, value),
            NamedExpr::Function {
                local_key,
                fn_token,
                name,
                arguments,
                return_type_token: _,
                return_type_annotation,
                body,
            } => self.visit_function(
                source_key,
                local_key.as_ref().copied(),
                expr,
                fn_token,
                name.clone(),
                arguments,
                return_type_annotation.clone(),
                body,
            ),
            NamedExpr::Call {
                callee,
                left_paren,
                arguments,
                right_paren,
            } => self.visit_call(source_key, expr, callee, left_paren, arguments, right_paren),
            NamedExpr::Return(return_token, ret_expr) => {
                self.visit_return(source_key, return_token.clone(), ret_expr.as_deref())
            }
            NamedExpr::Block {
                left_brace,
                expressions,
                right_brace,
            } => self.visit_block(source_key, left_brace, expressions, right_brace),

            NamedExpr::Poisoned => unreachable!(),
        };

        let typed_expression = typed_expression?;

        Ok(typed_expression)
    }

    fn visit_ident(
        &mut self,
        local_key: LocalKey,
        span: &Range<usize>,
        ident: &str,
    ) -> Result<TypedExpression, TypecheckError>
    {
        let type_key = self.locals_to_types[local_key];

        Ok(TypedExpression {
            type_key,
            kind: TypedExpressionKind::Ident {
                str_value: Spanned(span.clone(), ident.into()),
            },
        })
    }

    fn visit_binary_op(
        &mut self,
        source_key: SourceKey,
        expr: &NamedExpr,
        lhs: &NamedExpr,
        operator: Operator,
        rhs: &NamedExpr,
    ) -> Result<TypedExpression, TypecheckError>
    {
        let type_key = self.infer_type(source_key, expr)?;

        let (t_ast_lhs, t_ast_rhs) = (
            self.visit_expr(source_key, lhs)?,
            self.visit_expr(source_key, rhs)?,
        );

        Ok(TypedExpression {
            type_key,
            kind: TypedExpressionKind::BinaryOperation {
                lhs: Box::new(t_ast_lhs),
                operator: operator.clone(),
                rhs: Box::new(t_ast_rhs),
            },
        })
    }

    fn visit_assign(
        &mut self,
        source_key: SourceKey,
        lhs: &NamedAssignable,
        value: &NamedExpr,
    ) -> Result<TypedExpression, TypecheckError>
    {
        let value = self.visit_expr(source_key, value)?;

        // FIXME(Sniper10754): im pretty sure this clone can be avoided
        match lhs {
            NamedAssignable::Ident(local_key, ..) => {
                self.locals_to_types.insert(*local_key, value.type_key);
            }
        }

        let typed_lhs = self.visit_expr(source_key, &(lhs.clone().into()))?;
        let typed_lhs = Assignable::try_from(typed_lhs).unwrap();

        let type_key = self.compiler.ctx.type_ctx.types_arena.insert(Type::Unit);

        Ok(TypedExpression {
            type_key,
            kind: TypedExpressionKind::Assign {
                lhs: typed_lhs,
                value: Box::new(value),
            },
        })
    }

    fn visit_function(
        &mut self,
        source_key: SourceKey,
        local_key: Option<LocalKey>,
        expr: &NamedExpr,
        fn_token: &FunctionToken,
        name: Option<Spanned<String>>,
        arguments: &[Argument],
        return_type_annotation: Option<Spanned<TypeAnnotation>>,
        body: &NamedExpr,
    ) -> Result<TypedExpression, TypecheckError>
    {
        let type_key = self.infer_type(source_key, expr)?;

        if let Some(local_key) = local_key {
            self.locals_to_types.insert(local_key, type_key);
        }

        let typed_arguments = arguments
            .iter()
            .map(
                |Argument {
                     local_key: _,
                     name: Spanned(_, arg_name),
                     type_annotation,
                 }| {
                    (arg_name.into(), Type::from(type_annotation.value().clone()))
                },
            )
            .map(|(name, ty)| (name, self.compiler.ctx.type_ctx.types_arena.insert(ty)))
            .collect::<BTreeMap<_, _>>();

        let return_type = self.compiler.ctx.type_ctx.types_arena.insert(
            return_type_annotation
                .as_ref()
                .cloned()
                .map(|Spanned(_, ret)| ret)
                .unwrap_or(TypeAnnotation::Unit)
                .into(),
        );

        arguments.iter().zip(typed_arguments.values()).for_each(
            |(
                Argument {
                    local_key,
                    name: _,
                    type_annotation: _,
                },
                type_key,
            )| {
                self.locals_to_types.insert(*local_key, *type_key);
            },
        );

        let t_ast_body = self.visit_expr(source_key, body)?;

        let function = TypedFunction {
            fn_token: fn_token.clone(),
            name,
            arguments: typed_arguments,
            return_type,
            body: Box::new(t_ast_body),
        };

        if matches!(function.body.kind, TypedExpressionKind::Block { .. }) {
            self.typecheck_function_body_returns(source_key, &function)?;
        }

        Ok(TypedExpression {
            type_key,
            kind: TypedExpressionKind::Function(function),
        })
    }

    /// Rules for this algorithm:
    /// Detect if a function returns when the return type isnt `Type::Unit`
    /// assure that all branches of a function return
    fn typecheck_function_body_returns(
        &mut self,
        source_key: SourceKey,
        function: &TypedFunction,
    ) -> Result<(), TypecheckError>
    {
        self.check_fn_body_branches(source_key, &function.body)?;
        self.typecheck_fn_body_returns(source_key, function.return_type, &function.body)?;

        Ok(())
    }

    fn typecheck_fn_body_returns(
        &mut self,
        source_key: SourceKey,
        expected_type: TypeKey,
        expr: &TypedExpression,
    ) -> Result<(), TypecheckError>
    {
        use TypedExpressionKind::*;

        match &expr.kind {
            Return(return_type_index, return_token, value) => {
                match self.unify(*return_type_index, expected_type) {
                    Ok(_) => Ok(()),
                    Err(_) => Err(TypecheckError::TypeMismatch {
                        source_key,
                        span: value
                            .as_ref()
                            .map(|expr| expr.kind.span())
                            .unwrap_or(return_token.span()),
                        expected: display_type(
                            expected_type,
                            &self.compiler.ctx.type_ctx.types_arena,
                        ),
                        found: display_type(
                            *return_type_index,
                            &self.compiler.ctx.type_ctx.types_arena,
                        ),
                    }),
                }
            }

            Block { expressions, .. } => expressions.iter().try_for_each(|expr| {
                self.typecheck_fn_body_returns(source_key, expected_type, expr)
            }),

            _ => Ok(()),
        }
    }

    fn check_fn_body_branches(
        &mut self,
        source_key: SourceKey,
        expr: &TypedExpression,
    ) -> Result<(), TypecheckError>
    {
        if let Err(faulty_branch_position) = Self::check_branches_for_return(&expr.kind) {
            Err(TypecheckError::FunctionDoesntReturn {
                source_key,
                faulty_branch_position,
            })
        } else {
            Ok(())
        }
    }

    fn check_branches_for_return(expr: &TypedExpressionKind) -> Result<(), Range<usize>>
    {
        use TypedExpressionKind::*;

        match expr {
            Return(..) => Ok(()),

            Block { expressions, .. } => {
                let scope_has_return = expressions
                    .iter()
                    .any(|expr| matches!(expr.kind, Return(..)));

                if scope_has_return {
                    expressions
                        .iter()
                        .map(|t_ast| Self::check_branches_for_return(&t_ast.kind))
                        .collect::<Result<Vec<_>, _>>()?;

                    Ok(())
                } else {
                    Err(expr.span())
                }
            }

            _ => Ok(()),
        }
    }

    fn visit_call(
        &mut self,
        source_key: SourceKey,
        expr: &NamedExpr,
        callee: &NamedExpr,
        left_paren: &LeftParenthesisToken,
        arguments: &[NamedExpr],
        right_paren: &RightParenthesisToken,
    ) -> Result<TypedExpression, TypecheckError>
    {
        let call_arguments = arguments;

        match callee {
            NamedExpr::Ident {
                local_key,
                identifier: Spanned(span, name),
            } => {
                let type_key = self.locals_to_types[*local_key];

                let Type::Function(function) =
                    self.compiler.ctx.type_ctx.types_arena[type_key].clone()
                else {
                    return Err(TypecheckError::CannotCallNonFunction(
                        source_key,
                        callee.span(),
                    ));
                };

                let (call_arguments_len, function_arguments_len) =
                    (call_arguments.len(), function.arguments.len());

                match call_arguments_len.cmp(&function_arguments_len) {
                    Greater => {
                        return Err(TypecheckError::TooManyArguments {
                            source_key,
                            span: expr.span(),
                            call_arguments_len,
                            function_arguments_len,
                        })
                    }
                    Equal => {}
                    Less => {
                        return Err(TypecheckError::NotEnoughArguments {
                            source_key,
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
                            self.infer_type(source_key, expr)
                                .map(|inferred| (expr.span(), inferred))
                        })
                        .collect::<Result<Vec<_>, _>>()?
                        .into_iter(),
                    function.arguments.values().copied(),
                ) {
                    match self.unify(call_arg, func_arg) {
                        Ok(_) => {}
                        Err(_) => {
                            return Err(TypecheckError::TypeMismatch {
                                source_key,
                                span: call_arg_span,
                                expected: display_type(
                                    func_arg,
                                    &self.compiler.ctx.type_ctx.types_arena,
                                ),
                                found: display_type(
                                    call_arg,
                                    &self.compiler.ctx.type_ctx.types_arena,
                                ),
                            })
                        }
                    }
                }

                let mut arguments = vec![];

                for call_arg in call_arguments.iter() {
                    let node = self.visit_expr(source_key, call_arg)?;

                    arguments.push(node);
                }

                Ok(TypedExpression {
                    type_key,
                    kind: TypedExpressionKind::Call {
                        callee: Callable::Function(type_key, Spanned(span.clone(), name.into())),
                        left_parent: left_paren.clone(),
                        arguments,
                        right_parent: right_paren.clone(),
                        return_type: function.return_type,
                    },
                })
            }

            _ => Err(TypecheckError::CannotCallNonIdent(
                source_key,
                callee.span(),
            )),
        }
    }

    fn visit_return(
        &mut self,
        source_key: SourceKey,
        return_token: ReturnToken,
        ret_expr: Option<&NamedExpr>,
    ) -> Result<TypedExpression, TypecheckError>
    {
        let return_value_type = if let Some(ret_expr) = ret_expr {
            self.infer_type(source_key, ret_expr)?
        } else {
            self.compiler.ctx.type_ctx.types_arena.insert(Type::Unit)
        };

        let mut value = None;

        if let Some(ret_expr) = ret_expr {
            let temp_val = self.visit_expr(source_key, ret_expr)?;

            value = Some(temp_val);
        }

        let type_key = self.compiler.ctx.type_ctx.types_arena.insert(Type::Unit);

        Ok(TypedExpression {
            type_key,
            kind: TypedExpressionKind::Return(
                return_value_type,
                return_token.clone(),
                value.map(Box::new),
            ),
        })
    }

    fn visit_block(
        &mut self,
        source_key: SourceKey,
        left_brace: &LeftBraceToken,
        expressions: &[NamedExpr],
        right_brace: &RightBraceToken,
    ) -> Result<TypedExpression, TypecheckError>
    {
        let type_key = self.compiler.ctx.type_ctx.types_arena.insert(Type::Unit);

        Ok(TypedExpression {
            type_key,
            kind: TypedExpressionKind::Block {
                left_brace: left_brace.clone(),
                expressions: expressions
                    .iter()
                    .map(|expr| self.visit_expr(source_key, expr))
                    .collect::<Result<_, TypecheckError>>()?,
                right_brace: right_brace.clone(),
            },
        })
    }
}
