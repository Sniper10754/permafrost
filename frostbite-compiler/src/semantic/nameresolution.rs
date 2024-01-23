use frostbite_parser::ast::Expr;
use frostbite_reports::sourcemap::SourceKey;

use crate::{
    ir::named::{NamedAst, NamedExpr},
    Compiler,
};

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
    ) -> Result<NamedExpr, ()>
    {
        match expr {
            Expr::Int(_) => todo!(),
            Expr::Float(_) => todo!(),
            Expr::Ident(_) => todo!(),
            Expr::Bool(_) => todo!(),
            Expr::String(_) => todo!(),
            Expr::ImportDirective(_) => todo!(),
            Expr::BinaryOperation { lhs, operator, rhs } => todo!(),
            Expr::Assign {
                lhs,
                eq_token,
                value,
            } => todo!(),
            Expr::Function {
                fn_token,
                name,
                lpt,
                arguments,
                rpt,
                return_type_token,
                return_type_annotation,
                equals,
                body,
            } => todo!(),
            Expr::Call {
                callee,
                left_paren,
                arguments,
                right_paren,
            } => todo!(),
            Expr::Block {
                left_brace,
                expressions,
                right_brace,
            } => todo!(),
            Expr::Return(..) => todo!(),
            Expr::Poisoned => todo!(),
        }
    }
}
