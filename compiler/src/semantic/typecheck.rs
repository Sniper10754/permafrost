#![allow(clippy::too_many_arguments)]

use core::cmp::Ordering::{Equal, Greater, Less};

use alloc::{
    borrow::Cow, boxed::Box, collections::BTreeMap, format, string::String, vec, vec::Vec,
};

use frostbite_ast::{
    tokens::{
        BinaryOperatorKind, FunctionToken, LeftBraceToken, LeftParenthesisToken, Operator,
        ReturnToken, RightBraceToken, RightParenthesisToken, TypeAnnotation,
    },
    Span, Spannable, Spanned,
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
        span: Span,

        expected: Cow<'static, str>,
        found: Cow<'static, str>,
    },
    IncompatibleOperands
    {
        source_key: SourceKey,
        span: Span,

        left: Cow<'static, str>,
        right: Cow<'static, str>,
    },
    CannotCallNonIdent(SourceKey, Span),
    CannotCallNonFunction(SourceKey, Span),
    TooManyArguments
    {
        source_key: SourceKey,
        span: Span,

        call_arguments_len: usize,
        function_arguments_len: usize,
    },
    NotEnoughArguments
    {
        source_key: SourceKey,
        span: Span,

        call_arguments_len: usize,
        function_arguments_len: usize,
    },
    FunctionDoesntReturn
    {
        source_key: SourceKey,
        faulty_branch_position: Span,
    },
    Other(Report),
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
            ),
            TypecheckError::IncompatibleOperands { source_key, span, left, right } => {
                Report::new(
                    Level::Error,
                    span.clone(),
                    source_key,
                    "Incompatible operands",
                    None::<&str>,
                )
                .with_label(Label::new(format!("Left type is {left}"), span.clone(), source_key))
                .with_label(Label::new(format!("Right type is {right}"), span.clone(), source_key))
        },
            TypecheckError::CannotCallNonIdent(source_key, span) => Report::new(
                Level::Error,
                span,
                source_key,
                "Cannot call expression",
                Some("Only identifiers may be called"),
            ),
            TypecheckError::CannotCallNonFunction(source_key, span) => Report::new(
                Level::Error,
                span,
                source_key,
                "Cannot call non function",
                Some("Only functions may be called"),
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
            ),
            TypecheckError::FunctionDoesntReturn { source_key, faulty_branch_position } => Report::new(Level::Error, faulty_branch_position, source_key, "One branch of this function doesnt return", Some("This branch of this function doesnt return.")),
            TypecheckError::Other(report) => report,
        }
    }
}

impl From<Report> for TypecheckError
{
    fn from(value: Report) -> Self
    {
        Self::Other(value)
    }
}

pub fn check_types(
    compiler: &mut Compiler,
    source_key: SourceKey,
)
{
    compiler.ctx.insert_ast(source_key, TypedAst::default());

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

    for ref expr in rts.compiler.ctx.named_ctx.named_asts[source_key]
        .exprs
        .clone()
        .into_iter()
    {
        let result = rts.visit_expr(source_key, expr);

        match result {
            Ok(t_expr) => rts.compiler.ctx.get_ast_mut(source_key).nodes.push(t_expr),
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
    fn infer_type_annotation(
        &mut self,
        source_key: SourceKey,
        type_annotation: Spanned<&TypeAnnotation>,
    ) -> Result<TypeKey, Box<TypecheckError>>
    {
        match type_annotation.value() {
            TypeAnnotation::Int => Ok(self.insert_type(Type::Int)),
            TypeAnnotation::Float => Ok(self.insert_type(Type::Float)),
            TypeAnnotation::String => Ok(self.insert_type(Type::String)),
            TypeAnnotation::Bool => Ok(self.insert_type(Type::Bool)),
            TypeAnnotation::Any => Ok(self.insert_type(Type::Any)),
            TypeAnnotation::Unit => Ok(self.insert_type(Type::Unit)),
            TypeAnnotation::Object(..) => Err(TypecheckError::Other(Report::new(
                Level::Error,
                type_annotation.span(),
                source_key,
                "Objects not implemented yet",
                None::<&str>,
            ))
            .into()),
        }
    }

    fn unify(
        &mut self,
        a: TypeKey,
        b: TypeKey,
    ) -> Result<(), ()>
    {
        match (
            self.compiler.ctx.get_type(a).clone(),
            self.compiler.ctx.get_type(b).clone(),
        ) {
            (Type::Function(a), Type::Function(b)) => {
                Iterator::zip(a.arguments.values().copied(), b.arguments.values().copied())
                    .try_for_each(|(a, b)| self.unify(a, b))?;

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
    ) -> Result<TypeKey, Box<TypecheckError>>
    {
        match expr {
            NamedExpr::Int(_) => Ok(self.insert_type(Type::Int)),
            NamedExpr::Float(_) => Ok(self.insert_type(Type::Float)),
            NamedExpr::String(_) => Ok(self.insert_type(Type::String)),
            NamedExpr::Bool(_) => Ok(self.insert_type(Type::Bool)),

            NamedExpr::ModuleStatement(..) => Ok(self.insert_type(Type::Unit)),

            NamedExpr::Ident {
                local_key,
                identifier: _,
            } => self.infer_ident(*local_key),
            NamedExpr::BinaryOperation { lhs, operator, rhs } => {
                self.infer_binary_op(source_key, lhs, operator, rhs, expr.span())
            }
            NamedExpr::Assign { lhs: _, body: _ } => Ok(self.insert_type(Type::Unit)),
            NamedExpr::Function {
                local_key: _,
                fn_token: _,
                name: _,
                arguments,
                return_type_token: _,
                return_type_annotation,
                body: _,
            } => self
                .infer_function(
                    source_key,
                    arguments,
                    return_type_annotation
                        .as_ref()
                        .map(|spanned| spanned.as_ref()),
                )
                .map_err(|into_report| TypecheckError::Other(into_report.into_report()))
                .map_err(Into::into),

            NamedExpr::Call {
                callee,
                left_paren: _,
                arguments: _,
                right_paren: _,
            } => self.infer_call(source_key, callee),

            NamedExpr::Block { .. } => Ok(self.insert_type(Type::Unit)),
            NamedExpr::Return(..) => Ok(self.insert_type(Type::Unit)),

            NamedExpr::Poisoned => unreachable!(),
        }
    }

    fn infer_ident(
        &mut self,
        local_key: LocalKey,
    ) -> Result<TypeKey, Box<TypecheckError>>
    {
        Ok(self.locals_to_types[local_key])
    }

    fn infer_binary_op(
        &mut self,
        source_key: SourceKey,
        lhs: &NamedExpr,
        operator: &Operator,
        rhs: &NamedExpr,
        span: Span,
    ) -> Result<TypeKey, Box<TypecheckError>>
    {
        let (lhs_type_key, rhs_type_key) = (
            self.infer_type(source_key, lhs)?,
            self.infer_type(source_key, rhs)?,
        );

        match (
            operator.kind,
            (
                &self.compiler.ctx.get_type(lhs_type_key),
                &self.compiler.ctx.get_type(rhs_type_key),
            ),
        ) {
            (
                BinaryOperatorKind::Add | BinaryOperatorKind::Sub | BinaryOperatorKind::Mul,
                (Type::Int, Type::Int),
            ) => Ok(self.insert_type(Type::Int)),

            (
                BinaryOperatorKind::Add
                | BinaryOperatorKind::Sub
                | BinaryOperatorKind::Mul
                | BinaryOperatorKind::Div,
                (Type::Float, Type::Float) | (Type::Float, Type::Int) | (Type::Int, Type::Float),
            ) => Ok(self.insert_type(Type::Float)),

            (BinaryOperatorKind::Equal, (..)) => {
                if self.unify(lhs_type_key, rhs_type_key).is_ok() {
                    Ok(self.insert_type(Type::Bool))
                } else {
                    Err(TypecheckError::TypeMismatch {
                        source_key,
                        span,
                        expected: display_type(lhs_type_key, &self.compiler.ctx.type_ctx),
                        found: display_type(rhs_type_key, &self.compiler.ctx.type_ctx),
                    }
                    .into())
                }
            }

            _ => Err(TypecheckError::IncompatibleOperands {
                source_key,
                span,
                left: display_type(lhs_type_key, &self.compiler.ctx.type_ctx),
                right: display_type(rhs_type_key, &self.compiler.ctx.type_ctx),
            }
            .into()),
        }
    }

    fn infer_function(
        &mut self,
        source_key: SourceKey,
        arguments: &[Argument],
        return_type_annotation: Option<Spanned<&TypeAnnotation>>,
    ) -> Result<TypeKey, Box<TypecheckError>>
    {
        let fn_type = {
            let return_type = match return_type_annotation {
                Some(type_annotation) => self.infer_type_annotation(source_key, type_annotation)?,
                None => self.insert_type(Type::Unit),
            };

            let arguments = arguments
                .iter()
                .map(
                    |Argument {
                         local_key: _,
                         name: spanned_name,
                         type_annotation: spanned_type_annotation,
                     }| {
                        let type_key = self
                            .infer_type_annotation(source_key, spanned_type_annotation.as_ref())
                            .map_err(|into_report| {
                                TypecheckError::Other(into_report.into_report())
                            })?;

                        Ok((spanned_name.value().clone(), type_key))
                    },
                )
                .collect::<Result<BTreeMap<_, _>, TypecheckError>>()?;

            Type::Function(FunctionType {
                arguments,
                return_type,
            })
        };

        Ok(self.insert_type(fn_type))
    }

    fn infer_call(
        &mut self,
        source_key: SourceKey,
        callee: &NamedExpr,
    ) -> Result<TypeKey, Box<TypecheckError>>
    {
        match callee {
            NamedExpr::Ident {
                local_key,
                identifier: _,
            } => {
                let type_key = self.locals_to_types[*local_key];

                if let Type::Function(FunctionType {
                    arguments: _,
                    return_type,
                }) = self.compiler.ctx.get_type(type_key)
                {
                    Ok(*return_type)
                } else {
                    Err(TypecheckError::CannotCallNonFunction(source_key, callee.span()).into())
                }
            }

            _ => Err(TypecheckError::CannotCallNonIdent(source_key, callee.span()).into()),
        }
    }

    fn visit_expr(
        &mut self,
        source_key: SourceKey,
        expr: &NamedExpr,
    ) -> Result<TypedExpression, Box<TypecheckError>>
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
            NamedExpr::ModuleStatement(span) => Ok(TypedExpression {
                type_key: self.infer_type(source_key, expr)?,
                kind: TypedExpressionKind::ModuleStatement(span.clone()),
            }),
            NamedExpr::Ident {
                local_key,
                identifier: Spanned(span, ident),
            } => self.visit_ident(*local_key, span, ident),
            NamedExpr::BinaryOperation { lhs, operator, rhs } => {
                self.visit_binary_op(source_key, expr, lhs, operator.clone(), rhs)
            }
            NamedExpr::Assign { lhs, body: value } => self.visit_assign(source_key, lhs, value),
            NamedExpr::Function {
                local_key,
                fn_token,
                name,
                arguments,
                return_type_token: _,
                return_type_annotation: _,
                body,
            } => self.visit_function(
                source_key,
                local_key.as_ref().copied(),
                expr,
                fn_token,
                name.clone(),
                arguments,
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
        span: &Span,
        ident: &str,
    ) -> Result<TypedExpression, Box<TypecheckError>>
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
    ) -> Result<TypedExpression, Box<TypecheckError>>
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
    ) -> Result<TypedExpression, Box<TypecheckError>>
    {
        let value = self.visit_expr(source_key, value)?;

        match lhs {
            NamedAssignable::Ident(local_key, ..) => {
                self.locals_to_types.insert(*local_key, value.type_key);
            }
        }

        // FIXME(Sniper10754): im pretty sure this clone can be avoided
        let typed_lhs = self.visit_expr(source_key, &(lhs.clone().into()))?;
        let typed_lhs = Assignable::try_from(typed_lhs).unwrap();

        let type_key = self.insert_type(Type::Unit);

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
        body: &NamedExpr,
    ) -> Result<TypedExpression, Box<TypecheckError>>
    {
        let type_key = self.infer_type(source_key, expr)?;

        if let Some(local_key) = local_key {
            self.locals_to_types.insert(local_key, type_key);
        }

        let Type::Function(FunctionType {
            arguments: typed_arguments,
            return_type,
        }) = self.compiler.ctx.get_type(type_key).clone()
        else {
            unreachable!()
        };

        arguments
            .iter()
            .zip(typed_arguments.values().copied())
            .for_each(
                |(
                    Argument {
                        local_key,
                        name: _,
                        type_annotation: _,
                    },
                    type_key,
                )| {
                    self.locals_to_types.insert(*local_key, type_key);
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
        } else {
            self.unify(function.body.type_key, function.return_type)
                .map_err(|_| TypecheckError::TypeMismatch {
                    source_key,
                    span: body.span(),
                    expected: display_type(function.return_type, &self.compiler.ctx.type_ctx),
                    found: display_type(function.body.type_key, &self.compiler.ctx.type_ctx),
                })?;
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
    ) -> Result<(), Box<TypecheckError>>
    {
        if !matches!(self.compiler.ctx.get_type(function.return_type), Type::Unit) {
            self.check_fn_body_branches(source_key, &function.body)?;
        }

        self.typecheck_fn_body_returns(source_key, function.return_type, &function.body)?;

        Ok(())
    }

    fn typecheck_fn_body_returns(
        &mut self,
        source_key: SourceKey,
        expected_type: TypeKey,
        expr: &TypedExpression,
    ) -> Result<(), Box<TypecheckError>>
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
                        expected: display_type(expected_type, &self.compiler.ctx.type_ctx),
                        found: display_type(*return_type_index, &self.compiler.ctx.type_ctx),
                    }
                    .into()),
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
    ) -> Result<(), Box<TypecheckError>>
    {
        if let Err(faulty_branch_position) = Self::check_branches_for_return(&expr.kind) {
            Err(TypecheckError::FunctionDoesntReturn {
                source_key,
                faulty_branch_position,
            }
            .into())
        } else {
            Ok(())
        }
    }

    fn check_branches_for_return(expr: &TypedExpressionKind) -> Result<(), Span>
    {
        use TypedExpressionKind::*;

        match expr {
            Return(..) => Ok(()),

            Block { expressions, .. } => {
                let scope_has_return = expressions.iter().any(|expr| expr.kind.is_return());

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
    ) -> Result<TypedExpression, Box<TypecheckError>>
    {
        let call_arguments = arguments;

        match callee {
            NamedExpr::Ident {
                local_key,
                identifier: Spanned(span, name),
            } => {
                let type_key = self.locals_to_types[*local_key];

                let Type::Function(function) = self.compiler.ctx.get_type(type_key).clone() else {
                    return Err(
                        TypecheckError::CannotCallNonFunction(source_key, callee.span()).into(),
                    );
                };

                let (call_arguments_len, function_arguments_len) =
                    (call_arguments.len(), function.arguments.len());

                match Ord::cmp(&call_arguments_len, &function_arguments_len) {
                    Greater => {
                        return Err(TypecheckError::TooManyArguments {
                            source_key,
                            span: expr.span(),
                            call_arguments_len,
                            function_arguments_len,
                        }
                        .into())
                    }
                    Less => {
                        return Err(TypecheckError::NotEnoughArguments {
                            source_key,
                            span: expr.span(),
                            call_arguments_len,
                            function_arguments_len,
                        }
                        .into())
                    }
                    Equal => (), // Bro won the lottery!
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
                                expected: display_type(func_arg, &self.compiler.ctx.type_ctx),
                                found: display_type(call_arg, &self.compiler.ctx.type_ctx),
                            }
                            .into())
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

            _ => Err(TypecheckError::CannotCallNonIdent(source_key, callee.span()).into()),
        }
    }

    fn visit_return(
        &mut self,
        source_key: SourceKey,
        return_token: ReturnToken,
        ret_expr: Option<&NamedExpr>,
    ) -> Result<TypedExpression, Box<TypecheckError>>
    {
        let return_value_type = if let Some(ret_expr) = ret_expr {
            self.infer_type(source_key, ret_expr)?
        } else {
            self.insert_type(Type::Unit)
        };

        let mut value = None;

        if let Some(ret_expr) = ret_expr {
            let temp_val = self.visit_expr(source_key, ret_expr)?;

            value = Some(temp_val);
        }

        let type_key = self.insert_type(Type::Unit);

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
    ) -> Result<TypedExpression, Box<TypecheckError>>
    {
        let type_key = self.insert_type(Type::Unit);

        Ok(TypedExpression {
            type_key,
            kind: TypedExpressionKind::Block {
                left_brace: left_brace.clone(),
                expressions: expressions
                    .iter()
                    .map(|expr| self.visit_expr(source_key, expr))
                    .collect::<Result<_, _>>()?,
                right_brace: right_brace.clone(),
            },
        })
    }

    fn insert_type(
        &mut self,
        ty: Type,
    ) -> TypeKey
    {
        self.compiler.ctx.insert_type(ty)
    }
}
