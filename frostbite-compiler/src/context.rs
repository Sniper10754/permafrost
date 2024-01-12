use frostbite_parser::ast::Program;
use frostbite_reports::{
    sourcemap::{SourceId, SourceMap},
    ReportContext,
};
use slotmap::SecondaryMap;

use crate::{intrinsic::IntrinsicContext, tir::TypedAst};

#[derive(Debug, Default)]
pub struct CompilerContext
{
    pub src_map: SourceMap,
    pub report_ctx: ReportContext,
    pub intrinsic_ctx: IntrinsicContext,

    pub asts: SecondaryMap<SourceId, Program>,
    pub t_asts: SecondaryMap<SourceId, TypedAst>,
}

impl CompilerContext
{
    pub fn new(
        src_map: SourceMap,
        report_ctx: ReportContext,
        intrinsic_ctx: IntrinsicContext,
    ) -> Self
    {
        Self {
            src_map,
            report_ctx,
            intrinsic_ctx,
            asts: SecondaryMap::new(),
            t_asts: SecondaryMap::new(),
        }
    }
}
