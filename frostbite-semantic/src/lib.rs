use frostbite_parser::ast::Program;
use frostbite_reports::{
    sourcemap::{SourceId, SourceMap},
    IntoReport,
};

mod typecheck;

pub trait SemanticCheck {
    type Output;
    type Error: IntoReport;

    fn check(
        source_id: SourceId,
        source_map: &SourceMap,
        ast: &Program<'_>,
    ) -> Result<Self::Output, Vec<Self::Error>>;
}
