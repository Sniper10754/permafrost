use frostbite_reports::sourcemap::SourceKey;
use slotmap::SecondaryMap;

use crate::ir::typed::{Type, TypeKey, TypedAst, TypesArena};

#[derive(Debug, Default)]
pub struct TypeContext
{
    t_asts: SecondaryMap<SourceKey, TypedAst>,
    types_arena: TypesArena,
}

impl TypeContext
{
    pub fn get_ast(
        &self,
        source_key: SourceKey,
    ) -> &TypedAst
    {
        &self.t_asts[source_key]
    }

    pub fn get_ast_mut(
        &mut self,
        source_key: SourceKey,
    ) -> &mut TypedAst
    {
        &mut self.t_asts[source_key]
    }

    pub fn get_type(
        &self,
        type_key: TypeKey,
    ) -> &Type
    {
        &self.types_arena[type_key]
    }

    pub fn get_type_mut(
        &mut self,
        type_key: TypeKey,
    ) -> &mut Type
    {
        &mut self.types_arena[type_key]
    }

    pub fn insert_type(
        &mut self,
        ty: Type,
    ) -> TypeKey
    {
        self.types_arena.insert(ty)
    }

    pub fn insert_ast(
        &mut self,
        source_key: SourceKey,
        ast: TypedAst,
    )
    {
        self.t_asts.insert(source_key, ast);
    }
}
