use delegate::delegate;
use permafrost_reports::sourcemap::SourceKey;
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
    pub fn clear(&mut self)
    {
        self.t_asts.clear();
        self.types_arena.clear();
    }

    delegate! {
        to self.t_asts {
            #[call(get)]
            #[unwrap]
            pub fn get_typed_ast(
                &self,
                source_key: SourceKey,
            ) -> &TypedAst;

            #[call(get_mut)]
            #[unwrap]
            pub fn get_typed_ast_mut(
                &mut self,
                source_key: SourceKey,
            ) -> &mut TypedAst;

            #[call(insert)]
            pub fn insert_typed_ast(
                &mut self,
                source_key: SourceKey,
                ast: TypedAst,
            );
        }
        to self.types_arena {
            #[call(get)]
            #[unwrap]
            pub fn get_type(
                &self,
                type_key: TypeKey,
            ) -> &Type;

            #[call(get_mut)]
            #[unwrap]
            pub fn get_type_mut(
                &mut self,
                type_key: TypeKey,
            ) -> &mut Type;

            #[call(insert)]
            pub fn insert_type(
                &mut self,
                ty: Type,
            ) -> TypeKey;
        }
    }
}
