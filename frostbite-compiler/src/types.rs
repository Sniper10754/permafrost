use frostbite_reports::sourcemap::SourceId;
use slotmap::SecondaryMap;

use crate::ir::typed::{TypeKey, TypedAst, TypesArena};

#[derive(Debug, Default)]
pub struct TypeContext
{
    pub t_asts: SecondaryMap<SourceId, TypedAst>,
    pub types_arena: TypesArena,
    pub srcs_ids_to_types: SecondaryMap<SourceId, TypeKey>,
}
