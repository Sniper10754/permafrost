use dbg_pls::DebugPls;
use frostbite_reports::sourcemap::SourceId;
use slotmap::{new_key_type, SlotMap};

use alloc::string::String;

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
pub struct ModuleContext
{
    pub modules: SlotMap<ModuleKey, Module>,
}

#[derive(Debug, Clone, Default)]
pub struct Module
{
    pub name: String,
    pub src_id: SourceId,
}
