#![no_std]

extern crate alloc;

use alloc::vec::Vec;

use frostbite_parser::ast::Program;
use frostbite_reports::{
    sourcemap::{SourceId, SourceMap},
    IntoReport,
};

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
