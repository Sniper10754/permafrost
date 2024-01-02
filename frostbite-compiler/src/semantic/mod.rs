use frostbite_reports::sourcemap::SourceId;

use crate::{context::CompilerContext, tir::TypedAst};

use self::typecheck::check_types;

mod typecheck;

#[allow(clippy::let_unit_value)]
pub fn run_semantic_checks(
    compiler_ctx: &mut CompilerContext<'_, '_>,
    source_id: SourceId,
) -> (TypedAst,) {
    (check_types(compiler_ctx, source_id),)
}
