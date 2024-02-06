use alloc::vec::Vec;
use dbg_pls::DebugPls;
use delegate::delegate;
use slotmap::{new_key_type, SecondaryMap, SlotMap};

use permafrost_reports::sourcemap::SourceKey;

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

impl NamedContext
{
    delegate! {
        to self.modules {
            #[call(insert)]
            #[inline]
            pub fn insert_module(&mut self, module: NamedModule) -> NamedModuleKey;
            #[call(get)]
            #[unwrap]
            #[inline]
            pub fn get_module(&self, source_key: NamedModuleKey) -> &NamedModule;
            #[call(get_mut)]
            #[unwrap]
            #[inline]
            pub fn get_module_mut(&mut self, source_key: NamedModuleKey) -> &mut NamedModule;
        }
        to self.named_asts {
            #[call(insert)]
            #[inline]
            pub fn insert_ast(&mut self, source_key: SourceKey, ast: NamedAst);
            #[call(get)]
            #[unwrap]
            #[inline]
            pub fn get_ast(&self, source_key: SourceKey) -> &NamedAst;
            #[call(get_mut)]
            #[unwrap]
            #[inline]
            pub fn get_ast_mut(&mut self, source_key: SourceKey) -> &mut NamedAst;
        }
    }
}

#[derive(Debug, Clone)]
pub struct NamedModule
{
    pub src_key: SourceKey,
    pub items: Vec<Export>,
}

#[derive(Debug, Clone, derive_more::IsVariant)]
pub enum Export
{
    Local(LocalKey),
    Module(NamedModuleKey),
}
