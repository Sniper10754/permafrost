use alloc::vec::Vec;
use dbg_pls::DebugPls;
use frostbite_reports::sourcemap::SourceKey;
use slotmap::{new_key_type, SecondaryMap, SlotMap};

use crate::ir::named::{LocalKey, NamedAst};

new_key_type! {
    #[derive(derive_more::Display)]
    #[display(fmt = "{}", "self.0.as_ffi() as u32")]
    pub struct NamedModuleKey;
}

impl DebugPls for NamedModuleKey
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

#[derive(Debug, Clone, Copy, Default)]
pub struct ModuleImportError;

#[derive(Debug, Clone, Default)]
pub struct NamedContext
{
    pub modules: SlotMap<NamedModuleKey, NamedModule>,
    pub modules_by_src_keys: SecondaryMap<SourceKey, NamedModuleKey>,

    pub named_asts: SecondaryMap<SourceKey, NamedAst>,
}

#[derive(Debug, Clone)]
pub struct NamedModule
{
    pub src_id: SourceKey,
    pub items: Vec<Export>,
}

#[derive(Debug, Clone, derive_more::IsVariant)]
pub enum Export
{
    Local(LocalKey),
    Module(NamedModuleKey),
}
