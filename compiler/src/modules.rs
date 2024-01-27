use alloc::string::String;
use dbg_pls::DebugPls;
use frostbite_reports::sourcemap::SourceKey;
use slotmap::{new_key_type, SecondaryMap, SlotMap};

use crate::ir::named::NamedAst;

new_key_type! {
    #[derive(derive_more::Display)]
    #[display(fmt = "{}", "self.0.as_ffi() as u32")]
    pub struct ModuleKey;
}

impl DebugPls for ModuleKey
{
    fn fmt(
        &self,
        f: dbg_pls::Formatter<'_>,
    )
    {
        f.debug_tuple_struct("ModuleKey")
            .field(&(self.0.as_ffi() as u32))
            .finish()
    }
}

#[derive(Debug, Clone, Default)]
pub struct ModuleImportError;

#[derive(Debug, Clone, Default)]
pub struct NamedContext
{
    pub modules: SlotMap<ModuleKey, Module>,
    pub modules_to_srcs: SecondaryMap<ModuleKey, SourceKey>,

    pub named_asts: SecondaryMap<SourceKey, NamedAst>,
}

#[derive(Debug, Clone, Default)]
pub struct Module
{
    pub name: String,
    pub src_id: SourceKey,
}