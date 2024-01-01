use frostbite_parser::ast::Program;
use frostbite_reports::{
    sourcemap::{SourceId, SourceMap},
    ReportContext,
};

use crate::tir::TypedAst;

use self::typecheck::check_types;

mod typecheck;

#[allow(clippy::let_unit_value)]
pub fn run_semantic_checks(
    report_ctx: &mut ReportContext,
    source_id: SourceId,
    source_map: &SourceMap,
    ast: &Program<'_>,
) -> (TypedAst,) {
    (check_types(report_ctx, source_id, source_map, ast),)
}
