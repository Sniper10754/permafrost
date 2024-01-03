use frostbite_reports::sourcemap::SourceId;

use crate::context::CompilerContext;

use self::typecheck::check_types;

mod typecheck;

#[allow(clippy::let_unit_value)]
pub fn run_semantic_checks(compiler_ctx: &mut CompilerContext, source_id: SourceId) {
    check_types(compiler_ctx, source_id);
}
