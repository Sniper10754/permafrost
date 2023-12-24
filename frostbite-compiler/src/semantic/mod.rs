extern crate alloc;

use alloc::vec::Vec;

use frostbite_parser::ast::Program;
use frostbite_reports::{
    sourcemap::{SourceId, SourceMap},
    IntoReport, Report,
};

use crate::hir::Hir;

use self::typecheck::check_types;

mod typecheck;

#[allow(clippy::let_unit_value)]
pub fn run_semantic_checks(
    source_id: SourceId,
    source_map: &SourceMap,
    ast: &Program<'_>,
) -> Result<Hir, Vec<Report>> {
    let mut hir = Hir::default();

    check_types(source_id, source_map, ast, &mut hir).map_err(|errors| {
        errors
            .into_iter()
            .map(|error| error.into_report())
            .collect::<Vec<_>>()
    })?;

    Ok(hir)
}
