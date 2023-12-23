#![no_std]

extern crate alloc;

use alloc::vec::Vec;

use frostbite_parser::ast::Program;
use frostbite_reports::{
    sourcemap::{SourceId, SourceMap},
    IntoReport, Report,
};
use typecheck::Typecheck;

mod typecheck;

pub trait SemanticCheck {
    type Output;
    type Error<'a>: IntoReport + 'a;

    fn check<'ast>(
        source_id: SourceId,
        source_map: &SourceMap,
        ast: &Program<'ast>,
    ) -> Result<Self::Output, Vec<Self::Error<'ast>>>;
}

#[allow(clippy::let_unit_value)]
pub fn run_semantic_checks(
    source_id: SourceId,
    source_map: &SourceMap,
    ast: &Program<'_>,
) -> Result<<typecheck::Typecheck as SemanticCheck>::Output, Vec<Report>> {
    let typecheck_output = Typecheck::check(source_id, source_map, ast).map_err(|errors| {
        errors
            .into_iter()
            .map(|error| error.into_report())
            .collect::<Vec<_>>()
    })?;

    Ok(typecheck_output)
}
