use frostbite_reports::sourcemap::{SourceId, SourceUrl};
use slotmap::{new_key_type, SecondaryMap, SlotMap};

use alloc::string::String;

use crate::{
    tir::{Type, TypeKey},
    Compiler,
};

new_key_type! {
    pub struct ModuleKey;
}

#[derive(Debug, Clone, Default)]
pub struct ModuleContext
{
    pub modules: SlotMap<ModuleKey, Module>,
    pub modules_to_srcs: SecondaryMap<ModuleKey, SourceId>,
}

#[derive(Debug, Clone, Default)]
pub struct Module
{
    pub symbols: SlotMap<TypeKey, Type>,
}

impl Compiler
{
    pub fn import_module(
        &mut self,
        module_name: &str,
        current_module_key: ModuleKey,
    )
    {
    }

    fn import_local_module(
        &mut self,
        module_name: &str,
        current_module_key: ModuleKey,
    )
    {
    }

    fn import_global_module(
        &mut self,
        module_name: &str,
        current_module_key: ModuleKey,
    )
    {
    }
}

pub trait Importer {}
