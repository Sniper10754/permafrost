extern crate alloc;

use alloc::vec::Vec;

use frostbite_parser::ast::Program;
use frostbite_reports::{
    sourcemap::{SourceId, SourceMap},
    IntoReport, Report, ReportContext,
};

use crate::hir::HirTree;

use self::typecheck::check_types;

mod typecheck;

#[allow(clippy::let_unit_value)]
pub fn run_semantic_checks(
    report_ctx: &mut ReportContext,
    source_id: SourceId,
    source_map: &SourceMap,
    ast: &Program<'_>,
    hir: &mut HirTree,
) {
    check_types(report_ctx, source_id, source_map, ast, hir);
}
