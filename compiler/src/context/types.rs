use frostbite_reports::sourcemap::SourceKey;
use slotmap::SecondaryMap;

use crate::ir::typed::{TypedAst, TypesArena};

#[derive(Debug, Default)]
pub struct TypeContext
{
    pub t_asts: SecondaryMap<SourceKey, TypedAst>,
    pub types_arena: TypesArena,
}
