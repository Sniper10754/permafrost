use frostbite_parser::ast::Program;
use frostbite_reports::{
    sourcemap::{SourceId, SourceMap},
    ReportContext,
};
use slotmap::SecondaryMap;

#[derive(Debug)]
pub struct CompilerContext<'src, 'report_ctx> {
    pub src_map: &'src mut SourceMap,
    pub report_ctx: &'report_ctx mut ReportContext,

    pub asts: SecondaryMap<SourceId, Program>,
}

impl<'src, 'report_ctx> CompilerContext<'src, 'report_ctx> {
    pub fn new(src_map: &'src mut SourceMap, report_ctx: &'report_ctx mut ReportContext) -> Self {
        Self {
            src_map,
            report_ctx,
            asts: SecondaryMap::new(),
        }
    }
}
