#![allow(clippy::too_many_arguments)]

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
    Argument, Expr, Spannable, Spanned,
};
use frostbite_reports::{sourcemap::SourceId, IntoReport, Label, Level, Report};

use crate::{
    context::CompilerContext,
    tir::{
        display::display_type, Assignable, Callable, FunctionType, ImportDirectiveKind, RefersTo,
        Type, TypeKey, TypedAst, TypedExpression, TypedExpressionKind, TypedFunction, TypesArena,
    },
    utils::Scopes,
};

#[derive(Debug)]
pub enum TypecheckError
{
    TypeMismatch
    {
        source_id: SourceId,
        span: Range<usize>,

        expected: Cow<'static, str>,
        found: Cow<'static, str>,
    },
    SymbolNotFound(SourceId, Spanned<String>),
    IncompatibleOperands
    {
        source_id: SourceId,
        span: Range<usize>,

        left: Cow<'static, str>,
        right: Cow<'static, str>,
    },
    CannotAssignTo(SourceId, Range<usize>),
    CannotCallNonIdent(SourceId, Range<usize>),
    CannotCallNonFunction(SourceId, Range<usize>),
    TooManyArguments
    {
        source_id: SourceId,
        span: Range<usize>,

        call_arguments_len: usize,
        function_arguments_len: usize,
    },
    NotEnoughArguments
    {
        source_id: SourceId,
        span: Range<usize>,

        call_arguments_len: usize,
        function_arguments_len: usize,
    },
    FunctionDoesntReturn
    {
        source_id: SourceId,
        faulty_branch_position: Range<usize>,
    },
}

impl IntoReport for TypecheckError
{
    fn into_report(self) -> Report
    {
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
    compiler_ctx: &mut CompilerContext,
    source_id: SourceId,
)
{
    let t_ast = &mut compiler_ctx.t_asts[source_id];
    let types_arena = &mut compiler_ctx.types_arena;

    let mut rts = RecursiveTypechecker {
        scopes: Scopes::new(),
        t_ast,
        types_arena,
    };
    let mut typed_ast = TypedAst::default();

    compiler_ctx
        .intrinsic_ctx
        .symbols
        .iter()
        .map(|(name, ty)| (name, *ty))
        .for_each(|(name, type_key)| rts.scopes.insert_local(name, RefersTo::Type(type_key)));

    let ast = &compiler_ctx.asts[source_id];

    for expr in &ast.exprs {
        match rts.visit_expr(source_id, expr) {
            Ok(t_expr) => typed_ast.nodes.push(t_expr),
            Err(report) => compiler_ctx.report_ctx.push(report.into_report()),
        }
    }

    compiler_ctx.t_asts.insert(source_id, typed_ast);
}

struct RecursiveTypechecker<'infos>
{
    pub scopes: Scopes<RefersTo>,
    pub t_ast: &'infos mut TypedAst,
    pub types_arena: &'infos mut TypesArena,
}

impl<'a> RecursiveTypechecker<'a>
{
    fn unify(
        &mut self,
        a: TypeKey,
        b: TypeKey,
    ) -> Result<(), ()>
    {
        match (self.types_arena[a].clone(), self.types_arena[b].clone()) {
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
        source_id: SourceId,
        expr: &Expr,
    ) -> Result<TypeKey, TypecheckError>
    {
        match expr {
            Expr::Int(_) => Ok(self.types_arena.insert(Type::Int)),
            Expr::Float(_) => Ok(self.types_arena.insert(Type::Float)),
            Expr::String(_) => Ok(self.types_arena.insert(Type::String)),
            Expr::Bool(_) => Ok(self.types_arena.insert(Type::Bool)),
            Expr::ImportDirective(_) => Ok(self.types_arena.insert(Type::Unit)),
            Expr::Ident(Spanned(span, name)) => self.infer_ident(source_id, name, span.clone()),
            Expr::BinaryOperation { lhs, operator, rhs } => {
                self.infer_binary_op(source_id, lhs, operator, rhs, expr.span())
            }
            Expr::Assign {
                lhs: _,
                eq_token: _,
                value: _,
            } => Ok(self.types_arena.insert(Type::Unit)),
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
            } => self.infer_function(
                source_id,
                name.as_ref().map(|spanned| spanned.as_deref()),
                arguments,
                return_type_annotation
                    .as_ref()
                    .map(|spanned| spanned.as_ref()),
            ),
            Expr::Call {
                callee,
                left_paren: _,
                arguments: _,
                right_paren: _,
            } => match &**callee {
                Expr::Ident(Spanned(span, ident)) => {
                    let Some(referred_to) = self.scopes.local(ident).copied() else {
                        return Err(TypecheckError::SymbolNotFound(
                            source_id,
                            Spanned(span.clone(), ident.into()),
                        ));
                    };

                    let type_key = referred_to.into_type(self.t_ast);

                    if let Type::Function(FunctionType {
                        arguments: _,
                        return_type,
                    }) = &self.types_arena[type_key]
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

            Expr::Block { .. } => Ok(self.types_arena.insert(Type::Unit)),
            Expr::Return(..) => Ok(self.types_arena.insert(Type::Unit)),

            Expr::Poisoned => unreachable!(),
        }
    }

    fn infer_ident(
        &mut self,
        source_id: SourceId,
        name: &str,
        span: Range<usize>,
    ) -> Result<TypeKey, TypecheckError>
    {
        let Some(referred_to) = self.scopes.local(name).copied() else {
            return Err(TypecheckError::SymbolNotFound(
                source_id,
                Spanned(span, name.into()),
            ));
        };

        match referred_to {
            RefersTo::Local(local_key) => Ok(self.t_ast.locals[local_key]),
            RefersTo::Type(type_key) => Ok(type_key),
        }
    }

    fn infer_binary_op(
        &mut self,
        source_id: SourceId,
        lhs: &Expr,
        operator: &Operator,
        rhs: &Expr,
        span: Range<usize>,
    ) -> Result<TypeKey, TypecheckError>
    {
        let (lhs_type_key, rhs_type_key) = (
            self.infer_type(source_id, lhs)?,
            self.infer_type(source_id, rhs)?,
        );

        match (
            operator.kind,
            (
                &self.types_arena[lhs_type_key],
                &self.types_arena[rhs_type_key],
            ),
        ) {
            (
                BinaryOperatorKind::Add | BinaryOperatorKind::Sub | BinaryOperatorKind::Mul,
                (Type::Int, Type::Int),
            ) => Ok(self.types_arena.insert(Type::Int)),

            (
                BinaryOperatorKind::Add
                | BinaryOperatorKind::Sub
                | BinaryOperatorKind::Mul
                | BinaryOperatorKind::Div,
                (Type::Float, Type::Float) | (Type::Float, Type::Int) | (Type::Int, Type::Float),
            ) => Ok(self.types_arena.insert(Type::Float)),

            (BinaryOperatorKind::Equal, (..)) => {
                if self.unify(lhs_type_key, rhs_type_key).is_ok() {
                    Ok(self.types_arena.insert(Type::Bool))
                } else {
                    Err(TypecheckError::TypeMismatch {
                        source_id,
                        span,
                        expected: display_type(lhs_type_key, self.types_arena),
                        found: display_type(rhs_type_key, self.types_arena),
                    })
                }
            }

            _ => Err(TypecheckError::IncompatibleOperands {
                source_id,
                span,
                left: display_type(lhs_type_key, self.types_arena),
                right: display_type(rhs_type_key, self.types_arena),
            }),
        }
    }

    fn infer_function(
        &mut self,
        _source_id: SourceId,
        _name: Option<Spanned<&str>>,
        arguments: &[Argument],
        return_type_annotation: Option<Spanned<&TypeAnnotation>>,
    ) -> Result<TypeKey, TypecheckError>
    {
        let fn_type = Type::Function(FunctionType {
            arguments: arguments
                .iter()
                .map(
                    |Argument {
                         name: Spanned(_, name),
                         type_annotation: Spanned(_, type_annotation),
                     }| {
                        (
                            name.clone(),
                            self.types_arena.insert(type_annotation.clone().into()),
                        )
                    },
                )
                .collect(),
            return_type: self.types_arena.insert(
                return_type_annotation
                    .map(|Spanned(_, ty)| ty)
                    .cloned()
                    .unwrap_or(TypeAnnotation::Unit)
                    .into(),
            ),
        });

        Ok(self.types_arena.insert(fn_type))
    }

    fn visit_expr(
        &mut self,
        source_id: SourceId,
        expr: &Expr,
    ) -> Result<TypedExpression, TypecheckError>
    {
        let old_len = self.scopes.len();

        let typed_expression_kind = match expr {
            Expr::Int(value) => Ok(TypedExpressionKind::Int(value.clone())),
            Expr::Float(value) => Ok(TypedExpressionKind::Float(value.clone())),
            Expr::Bool(value) => Ok(TypedExpressionKind::Bool(value.clone())),
            Expr::String(value) => Ok(TypedExpressionKind::String(
                value.as_ref().map(Clone::clone),
            )),
            Expr::ImportDirective(import_directive) => {
                self.visit_import_directive(import_directive.as_ref())
            }
            Expr::Ident(Spanned(span, ident)) => self.visit_ident(source_id, span, ident),
            Expr::BinaryOperation { lhs, operator, rhs } => {
                self.visit_binary_op(source_id, expr, lhs, operator.clone(), rhs)
            }
            Expr::Assign {
                lhs,
                eq_token: _,
                value,
            } => self.visit_assign(source_id, lhs, value),
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
            } => self.visit_function(
                source_id,
                expr,
                fn_token,
                name.clone(),
                arguments,
                return_type_annotation.clone(),
                body,
            ),
            Expr::Call {
                callee,
                left_paren,
                arguments,
                right_paren,
            } => self.visit_call(source_id, expr, callee, left_paren, arguments, right_paren),
            Expr::Return(return_token, ret_expr) => {
                self.visit_return(source_id, return_token.clone(), ret_expr.as_deref())
            }
            Expr::Block {
                left_brace,
                expressions,
                right_brace,
            } => self.visit_block(source_id, left_brace, expressions, right_brace),

            Expr::Poisoned => unreachable!(),
        };

        self.scopes.truncate(old_len);

        let typed_expression_kind = typed_expression_kind?;

        Ok(TypedExpression {
            type_key: self.infer_type(source_id, expr)?,
            typed_expression_kind,
        })
    }

    /// Rules for this algorithm:
    /// Detect if a function returns when the return type isnt `Type::Unit`
    /// assure that all branches of a function return
    fn typecheck_function_body_returns(
        &mut self,
        source_id: SourceId,
        function: &TypedFunction,
    ) -> Result<(), TypecheckError>
    {
        self.check_fn_body_branches(source_id, &function.body)?;
        self.typecheck_fn_body_returns(source_id, function.return_type, &function.body)?;

        Ok(())
    }

    fn typecheck_fn_body_returns(
        &mut self,
        source_id: SourceId,
        expected_type: TypeKey,
        expr: &TypedExpression,
    ) -> Result<(), TypecheckError>
    {
        use TypedExpressionKind::*;

        match &expr.typed_expression_kind {
            Return(return_type_index, return_token, value) => {
                match self.unify(*return_type_index, expected_type) {
                    Ok(_) => Ok(()),
                    Err(_) => Err(TypecheckError::TypeMismatch {
                        source_id,
                        span: value
                            .as_ref()
                            .map(|expr| expr.typed_expression_kind.span())
                            .unwrap_or(return_token.span()),
                        expected: display_type(expected_type, self.types_arena),
                        found: display_type(*return_type_index, self.types_arena),
                    }),
                }
            }

            Block { expressions, .. } => expressions.iter().try_for_each(|expr| {
                self.typecheck_fn_body_returns(source_id, expected_type, expr)
            }),

            _ => Ok(()),
        }
    }

    fn check_fn_body_branches(
        &mut self,
        source_id: SourceId,
        expr: &TypedExpression,
    ) -> Result<(), TypecheckError>
    {
        if let Err(span) = Self::check_branches_for_return(&expr.typed_expression_kind) {
            Err(TypecheckError::FunctionDoesntReturn {
                source_id,
                faulty_branch_position: span,
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
                    .any(|expr| matches!(&expr.typed_expression_kind, Return(..)));

                if scope_has_return {
                    expressions
                        .iter()
                        .map(|t_ast| Self::check_branches_for_return(&t_ast.typed_expression_kind))
                        .collect::<Result<Vec<_>, _>>()?;

                    Ok(())
                } else {
                    Err(expr.span())
                }
            }

            _ => Ok(()),
        }
    }

    fn visit_import_directive(
        &mut self,
        import_directive: Spanned<&frostbite_parser::ast::ImportDirectiveKind>,
    ) -> Result<TypedExpressionKind, TypecheckError>
    {
        let Spanned(span, import_directive) = import_directive;

        let imp_dir = match import_directive.clone() {
            frostbite_parser::ast::ImportDirectiveKind::FromModuleImportSymbol {
                module,
                symbol,
            } => ImportDirectiveKind::FromModuleImportSymbol { module, symbol },
            frostbite_parser::ast::ImportDirectiveKind::ImportModule { module } => {
                ImportDirectiveKind::ImportModule { module }
            }
        };

        Ok(TypedExpressionKind::ImportDirective(Spanned(span, imp_dir)))
    }

    fn visit_ident(
        &mut self,
        source_id: SourceId,
        span: &Range<usize>,
        ident: &str,
    ) -> Result<TypedExpressionKind, TypecheckError>
    {
        let Some(refers_to) = self.scopes.local(ident).copied() else {
            return Err(TypecheckError::SymbolNotFound(
                source_id,
                Spanned(span.clone(), ident.into()),
            ));
        };

        let _type_key = refers_to.into_type(self.t_ast);

        Ok(TypedExpressionKind::Ident {
            refers_to,
            str_value: Spanned(span.clone(), ident.into()),
        })
    }

    fn visit_binary_op(
        &mut self,
        source_id: SourceId,
        expr: &Expr,
        lhs: &Expr,
        operator: Operator,
        rhs: &Expr,
    ) -> Result<TypedExpressionKind, TypecheckError>
    {
        self.infer_type(source_id, expr)?;

        let (t_ast_lhs, t_ast_rhs) = (
            self.visit_expr(source_id, lhs)?,
            self.visit_expr(source_id, rhs)?,
        );

        Ok(TypedExpressionKind::BinaryOperation {
            lhs: Box::new(t_ast_lhs),
            operator: operator.clone(),
            rhs: Box::new(t_ast_rhs),
        })
    }

    fn visit_assign(
        &mut self,
        source_id: SourceId,
        lhs: &Expr,
        value: &Expr,
    ) -> Result<TypedExpressionKind, TypecheckError>
    {
        let value = self.visit_expr(source_id, value)?;

        match lhs {
            Expr::Ident(spanned_str) => match self.scopes.local(spanned_str.value()).copied() {
                Some(RefersTo::Local(_)) => {}
                Some(RefersTo::Type(_)) => {
                    return Err(TypecheckError::CannotAssignTo(source_id, lhs.span()));
                }
                None => {
                    let local_index = self.t_ast.locals.insert(value.type_key);

                    self.scopes
                        .insert_local(spanned_str.value(), RefersTo::Local(local_index));
                }
            },

            _ => return Err(TypecheckError::CannotAssignTo(source_id, lhs.span())),
        }

        let lhs = self.visit_expr(source_id, lhs)?;

        let assignable = Assignable::try_from(lhs).unwrap();

        Ok(TypedExpressionKind::Assign {
            lhs: assignable,
            value: Box::new(value),
        })
    }

    fn visit_function(
        &mut self,
        source_id: SourceId,
        expr: &Expr,
        fn_token: &FunctionToken,
        name: Option<Spanned<String>>,
        arguments: &[Argument],
        return_type_annotation: Option<Spanned<TypeAnnotation>>,
        body: &Expr,
    ) -> Result<TypedExpressionKind, TypecheckError>
    {
        let type_key = self.infer_type(source_id, expr)?;

        if let Some(name) = name.as_ref() {
            self.scopes
                .insert_local(name.value(), RefersTo::Type(type_key));
        }

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
            .map(|(name, ty)| (name, self.types_arena.insert(ty)))
            .collect::<BTreeMap<_, _>>();

        let return_type = Type::from(
            return_type_annotation
                .as_ref()
                .cloned()
                .map(|Spanned(_, ret)| ret)
                .unwrap_or(TypeAnnotation::Unit),
        );

        let return_type = self.types_arena.insert(return_type);

        self.scopes.enter_scope();

        for (k, v) in arguments.iter() {
            let local_index = self.t_ast.locals.insert(*v);

            self.scopes.insert_local(k, RefersTo::Local(local_index));
        }

        let t_ast_body = self.visit_expr(source_id, body)?;

        self.scopes.leave_scope();

        let function = TypedFunction {
            fn_token: fn_token.clone(),
            name,
            arguments,
            return_type,
            body: Box::new(t_ast_body),
        };

        if !matches!(self.types_arena[function.return_type], Type::Unit)
            && matches!(
                &function.body.typed_expression_kind,
                TypedExpressionKind::Block { .. }
            )
        {
            self.typecheck_function_body_returns(source_id, &function)?;
        }

        Ok(TypedExpressionKind::Function(function))
    }

    fn visit_call(
        &mut self,
        source_id: SourceId,
        expr: &Expr,
        callee: &Expr,
        left_paren: &LeftParenthesisToken,
        arguments: &[Expr],
        right_paren: &RightParenthesisToken,
    ) -> Result<TypedExpressionKind, TypecheckError>
    {
        let call_arguments = arguments;

        match callee {
            Expr::Ident(Spanned(span, name)) => {
                let Some(refers_to) = self.scopes.local(name).copied() else {
                    return Err(TypecheckError::SymbolNotFound(
                        source_id,
                        Spanned(span.clone(), name.into()),
                    ));
                };

                let type_key = refers_to.into_type(self.t_ast);

                let Type::Function(function) = self.types_arena[type_key].clone() else {
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
                            self.infer_type(source_id, expr)
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
                                source_id,
                                span: call_arg_span,
                                expected: display_type(func_arg, self.types_arena),
                                found: display_type(call_arg, self.types_arena),
                            })
                        }
                    }
                }

                let mut arguments = vec![];

                for call_arg in call_arguments.iter() {
                    let node = self.visit_expr(source_id, call_arg)?;

                    arguments.push(node);
                }

                Ok(TypedExpressionKind::Call {
                    callee: Callable::Function(refers_to, Spanned(span.clone(), name.into())),
                    left_parent: left_paren.clone(),
                    arguments,
                    right_parent: right_paren.clone(),
                    return_type: function.return_type,
                })
            }

            _ => Err(TypecheckError::CannotCallNonIdent(source_id, callee.span())),
        }
    }

    fn visit_return(
        &mut self,
        source_id: SourceId,
        return_token: ReturnToken,
        ret_expr: Option<&Expr>,
    ) -> Result<TypedExpressionKind, TypecheckError>
    {
        let return_value_type = if let Some(ret_expr) = ret_expr {
            self.infer_type(source_id, ret_expr)?
        } else {
            self.types_arena.insert(Type::Unit)
        };

        let mut value = None;

        if let Some(ret_expr) = ret_expr {
            let temp_val = self.visit_expr(source_id, ret_expr)?;

            value = Some(temp_val);
        }

        Ok(TypedExpressionKind::Return(
            return_value_type,
            return_token.clone(),
            value.map(Box::new),
        ))
    }

    fn visit_block(
        &mut self,
        source_id: SourceId,
        left_brace: &LeftBraceToken,
        expressions: &[Expr],
        right_brace: &RightBraceToken,
    ) -> Result<TypedExpressionKind, TypecheckError>
    {
        Ok(TypedExpressionKind::Block {
            left_brace: left_brace.clone(),
            expressions: expressions
                .iter()
                .map(|expr| self.visit_expr(source_id, expr))
                .collect::<Result<_, TypecheckError>>()?,
            right_brace: right_brace.clone(),
        })
    }
}
