use frostbite_parser::ast::{tokens::TypeAnnotation, Expr, Program};
use frostbite_reports::{
    sourcemap::{SourceId, SourceMap},
    IntoReport,
};

use crate::SemanticCheck;

enum Type {
    Int,
    Float,
    String,
    NotSpecified,
    Other(String),
}

impl<'a> From<TypeAnnotation<'a>> for Type {
    fn from(value: TypeAnnotation<'a>) -> Self {
        match value {
            TypeAnnotation::Int => Self::Int,
            TypeAnnotation::Float => Self::Float,
            TypeAnnotation::String => Self::String,
            TypeAnnotation::NotSpecified => Self::NotSpecified,
            TypeAnnotation::Other(other) => Self::Other(other.to_string()),
        }
    }
}

pub enum TypecheckError {}

impl IntoReport for TypecheckError {
    type Arguments = ();

    fn into_report(self, _: Self::Arguments) -> frostbite_reports::Report {
        match self {}
    }
}

pub struct Typecheck;

impl SemanticCheck for Typecheck {
    type Output = ();
    type Error = TypecheckError;

    fn check(
        source_id: SourceId,
        map: &SourceMap,
        ast: &Program<'_>,
    ) -> Result<Self::Output, Vec<Self::Error>> {
        let rts = RecursiveTypechecker::new(source_id, map);

        let RecursiveTypechecker { errors, .. } = rts;

        match errors.is_empty() {
            true => Ok(()),
            false => Err(errors),
        }
    }
}

struct RecursiveTypechecker<'src_map> {
    current_file_id: SourceId,
    src_map: &'src_map SourceMap,
    errors: Vec<TypecheckError>,
}

impl<'src_map> RecursiveTypechecker<'src_map> {
    fn new(current_file_id: SourceId, map: &'src_map SourceMap) -> Self {
        Self {
            current_file_id,
            src_map: map,
            errors: vec![],
        }
    }

    fn check_expr(&mut self, expr: &Expr<'_>) {
        match expr {
            Expr::Ident(_, _) => todo!(),
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
                lpt,
                arguments,
                rpt,
            } => todo!(),

            _ => (),
        }
    }
}
