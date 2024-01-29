use core::ops::Range;

use alloc::{boxed::Box, format, string::String, vec::Vec};
use frostbite_ast::{
    tokens::{
        ArrowToken, FunctionToken, LeftBraceToken, LeftParenthesisToken, ReturnToken,
        RightBraceToken, RightParenthesisToken, TypeAnnotation,
    },
    Expr, Span, Spannable, Spanned,
};
use frostbite_reports::{sourcemap::SourceKey, IntoReport, Level, Report};

use crate::{
    ir::named::{Argument, Assignable, LocalKey, NamedAst, NamedExpr},
    utils::Scopes,
    Compiler,
};

enum NameResolutionError
{
    CannotAssignTo
    {
        source_key: SourceKey, span: Span
    },

    IdentifierNotFound
    {
        source_key: SourceKey,
        identifier: Spanned<String>,
    },
}

impl IntoReport for NameResolutionError
{
    fn into_report(self) -> Report
    {
        match self {
            NameResolutionError::CannotAssignTo { source_key, span } => Report::new(
                Level::Error,
                span,
                source_key,
                "Cannot assign to non-ident",
                Some("You may only assign to identifiers"),
                [],
                [],
            ),
            NameResolutionError::IdentifierNotFound {
                source_key,
                identifier: Spanned(span, identifier),
            } => Report::new(
                Level::Error,
                span,
                source_key,
                "Identifier not found",
                Some(format!("identifier `{identifier}` not found")),
                [],
                [],
            ),
        }
    }
}

pub fn check_names(
    compiler: &mut Compiler,
    source_key: SourceKey,
)
{
    compiler
        .ctx
        .named_ctx
        .named_asts
        .insert(source_key, NamedAst::default());

    let mut rnc = RecursiveNameChecker {
        compiler,
        scopes: Scopes::new(),
    };

    let ast = rnc.compiler.ctx.asts[source_key].exprs.clone().into_iter();

    for ref expr in ast {
        let result = rnc.visit_expr(source_key, expr);

        match result {
            Ok(named_expr) => rnc.compiler.ctx.named_ctx.named_asts[source_key]
                .exprs
                .push(named_expr),
            Err(error) => rnc.compiler.ctx.report_ctx.push(error.into_report()),
        }
    }
}

pub struct RecursiveNameChecker<'compiler>
{
    compiler: &'compiler mut Compiler,
    scopes: Scopes<LocalKey>,
}

impl<'compiler> RecursiveNameChecker<'compiler>
{
    fn visit_expr(
        &mut self,
        source_key: SourceKey,
        expr: &Expr,
    ) -> Result<NamedExpr, NameResolutionError>
    {
        let old_len = self.scopes.len();

        let expr = match expr {
            Expr::Int(value) => Ok(NamedExpr::Int(value.clone())),
            Expr::Float(value) => Ok(NamedExpr::Float(value.clone())),
            Expr::Bool(value) => Ok(NamedExpr::Bool(value.clone())),
            Expr::String(value) => Ok(NamedExpr::String(value.clone())),
            Expr::Ident(identifier) => {
                self.visit_ident(source_key, identifier.value(), identifier.span())
            }
            Expr::ImportDirective(_) => todo!(),
            Expr::ModuleDirective(..) => todo!(),
            Expr::BinaryOperation { lhs, operator, rhs } => Ok(NamedExpr::BinaryOperation {
                lhs: Box::new(self.visit_expr(source_key, lhs)?),
                operator: operator.clone(),
                rhs: Box::new(self.visit_expr(source_key, rhs)?),
            }),
            Expr::Assign {
                lhs,
                eq_token: _,
                value,
            } => self.visit_assign(source_key, lhs, value),
            Expr::Function {
                fn_token,
                name,
                lpt: _,
                arguments,
                rpt: _,
                return_type_token,
                return_type_annotation,
                equals: _,
                body,
            } => self.visit_function(
                source_key,
                fn_token.clone(),
                name.as_ref().map(|name| name.as_deref()),
                arguments,
                return_type_token.clone(),
                return_type_annotation.clone(),
                body,
            ),
            Expr::Call {
                callee,
                left_paren,
                arguments,
                right_paren,
            } => self.visit_call(
                source_key,
                callee,
                left_paren.clone(),
                arguments,
                right_paren.clone(),
            ),
            Expr::Block {
                left_brace,
                expressions,
                right_brace,
            } => self.visit_block(
                source_key,
                left_brace.clone(),
                expressions,
                right_brace.clone(),
            ),
            Expr::Return(return_token, return_value) => {
                self.visit_return(source_key, return_token.clone(), return_value.as_deref())
            }

            Expr::Poisoned => unreachable!(),
        };

        self.scopes.truncate(old_len);

        expr
    }

    fn visit_ident(
        &mut self,
        source_key: SourceKey,
        identifier: &str,
        span: Span,
    ) -> Result<NamedExpr, NameResolutionError>
    {
        let Some(local_key) = self.scopes.local(identifier).copied() else {
            return Err(NameResolutionError::IdentifierNotFound {
                source_key,
                identifier: Spanned(span, identifier.into()),
            });
        };

        Ok(NamedExpr::Ident {
            local_key,
            identifier: Spanned(span, identifier.into()),
        })
    }

    fn visit_assign(
        &mut self,
        source_key: SourceKey,
        lhs: &Expr,
        value: &Expr,
    ) -> Result<NamedExpr, NameResolutionError>
    {
        let body = Box::new(self.visit_expr(source_key, value)?);

        let lhs = match lhs {
            Expr::Poisoned => unreachable!(),

            Expr::Ident(spanned_name) => {
                let local_key = self.insert_forced(source_key, spanned_name.value());

                Assignable::Ident(local_key, spanned_name.clone())
            }

            _ => {
                return Err(NameResolutionError::CannotAssignTo {
                    source_key,
                    span: lhs.span(),
                })
            }
        };

        Ok(NamedExpr::Assign { lhs, body })
    }

    fn visit_function(
        &mut self,
        source_key: SourceKey,
        fn_token: FunctionToken,
        name: Option<Spanned<&str>>,
        arguments: &[frostbite_ast::Argument],
        return_type_token: Option<ArrowToken>,
        return_type_annotation: Option<Spanned<TypeAnnotation>>,
        body: &Expr,
    ) -> Result<NamedExpr, NameResolutionError>
    {
        let local_key = if let Some(ref spanned_name) = name {
            self.insert_clashing(source_key, spanned_name)
        } else {
            None
        };

        self.scopes.enter_scope();

        let arguments = arguments
            .iter()
            .cloned()
            .map(
                |frostbite_ast::Argument {
                     name,
                     type_annotation,
                 }| {
                    Argument {
                        local_key: self.insert_forced(source_key, name.value()),
                        name,
                        type_annotation,
                    }
                },
            )
            .collect::<Vec<_>>();

        arguments.iter().for_each(
            |Argument {
                 local_key,
                 name: spanned_name,
                 type_annotation: _,
             }| {
                self.scopes.insert_forced(spanned_name.value(), *local_key);
            },
        );

        let body = self.visit_expr(source_key, body)?;

        self.scopes.leave_scope();

        Ok(NamedExpr::Function {
            local_key,
            fn_token,
            name: name.map(|name| name.map(|name| name.into())),
            arguments,
            return_type_token,
            return_type_annotation,
            body: Box::new(body),
        })
    }

    fn visit_call(
        &mut self,
        source_key: SourceKey,
        callee: &Expr,
        left_paren: LeftParenthesisToken,
        arguments: &[Expr],
        right_paren: RightParenthesisToken,
    ) -> Result<NamedExpr, NameResolutionError>
    {
        Ok(NamedExpr::Call {
            callee: Box::new(self.visit_expr(source_key, callee)?),
            left_paren,
            arguments: arguments
                .iter()
                .map(|argument| self.visit_expr(source_key, argument))
                .collect::<Result<Vec<_>, _>>()?,
            right_paren,
        })
    }

    fn visit_block(
        &mut self,
        source_key: SourceKey,
        left_brace: LeftBraceToken,
        expressions: &[Expr],
        right_brace: RightBraceToken,
    ) -> Result<NamedExpr, NameResolutionError>
    {
        Ok(NamedExpr::Block {
            left_brace,
            expressions: expressions
                .iter()
                .map(|expr| self.visit_expr(source_key, expr))
                .collect::<Result<Vec<_>, _>>()?,
            right_brace,
        })
    }

    fn visit_return(
        &mut self,
        source_key: SourceKey,
        return_token: ReturnToken,
        return_value: Option<&Expr>,
    ) -> Result<NamedExpr, NameResolutionError>
    {
        Ok(NamedExpr::Return(
            return_token,
            return_value
                .map(|expr| self.visit_expr(source_key, expr))
                .transpose()?
                .map(Box::new),
        ))
    }

    fn insert_clashing(
        &mut self,
        source_key: SourceKey,
        name: &str,
    ) -> Option<LocalKey>
    {
        if self.scopes.local(name).is_none() {
            let local_key = self.compiler.ctx.named_ctx.named_asts[source_key]
                .locals
                .insert(());

            self.scopes.try_insert(name, local_key).unwrap();

            Some(local_key)
        } else {
            None
        }
    }

    fn insert_forced(
        &mut self,
        source_key: SourceKey,
        name: &str,
    ) -> LocalKey
    {
        let local_key = self.compiler.ctx.named_ctx.named_asts[source_key]
            .locals
            .insert(());

        self.scopes.insert_forced(name, local_key);

        local_key
    }
}
