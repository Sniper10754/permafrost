use core::ops::Range;

use frostbite_parser::ast::{Expr, Spannable, Spanned};
use frostbite_reports::{sourcemap::SourceKey, IntoReport};

use crate::{
    ir::named::{NamedAst, NamedExpr},
    Compiler,
};

enum NameResolutionError {}

impl IntoReport for NameResolutionError
{
    fn into_report(self) -> frostbite_reports::Report
    {
        match self {}
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

    let mut rnc = RecursiveNameChecker { compiler };

    let ast = rnc.compiler.ctx.asts[source_key].exprs.clone().into_iter();

    for ref expr in ast {
        let result = rnc.visit_expr(expr);

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
}

impl<'compiler> RecursiveNameChecker<'compiler>
{
    fn visit_expr(
        &mut self,
        expr: &Expr,
    ) -> Result<NamedExpr, NameResolutionError>
    {
        match expr {
            Expr::Int(value) => Ok(NamedExpr::Int(value.clone())),
            Expr::Float(value) => Ok(NamedExpr::Float(value.clone())),
            Expr::Bool(value) => Ok(NamedExpr::Bool(value.clone())),
            Expr::String(value) => Ok(NamedExpr::String(value.clone())),
            Expr::Ident(identifier) => self.visit_ident(identifier.value(), identifier.span()),
            Expr::ImportDirective(_) => todo!(),
            Expr::BinaryOperation { lhs, operator, rhs } => {}
            Expr::Assign {
                lhs,
                eq_token: _,
                value,
            } => todo!(),
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
            } => todo!(),
            Expr::Call {
                callee,
                left_paren: _,
                arguments,
                right_paren: _,
            } => todo!(),
            Expr::Block {
                left_brace: _,
                expressions,
                right_brace: _,
            } => todo!(),
            Expr::Return(_, expr) => todo!(),

            Expr::Poisoned => unreachable!(),
        }
    }

    fn visit_ident(
        &mut self,
        ident: &str,
        span: Range<usize>,
    ) -> Result<NamedExpr, NameResolutionError>
    {
        Ok(NamedExpr::Ident(Spanned(span, ident.into())))
    }
}
