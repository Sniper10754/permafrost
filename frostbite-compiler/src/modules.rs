use frostbite_reports::sourcemap::SourceId;
use slotmap::{new_key_type, SecondaryMap, SlotMap};

use alloc::string::String;

use crate::Compiler;

new_key_type! {
    pub struct ModuleKey;
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
