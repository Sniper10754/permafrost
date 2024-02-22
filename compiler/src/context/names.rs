use alloc::{collections::BTreeMap, string::String};
use delegate::delegate;
use derive_more::*;
use slotmap::{new_key_type, SecondaryMap, SlotMap};

use permafrost_reports::sourcemap::SourceKey;

use crate::ir::named::{LocalKey, NamedAst};

new_key_type! {
    #[derive(derive_more::Display)]
    #[display(fmt = "{}", "self.0.as_ffi() as u32")]
    pub struct NamespaceKey;
}

#[derive(Debug, Clone, Copy, Default)]
pub struct NamespaceImportError;

#[derive(Debug, Clone, Default)]
pub struct NamedContext
{
    pub namespaces: SlotMap<NamespaceKey, Namespace>,
    pub namespaces_by_src_keys: SecondaryMap<SourceKey, NamespaceKey>,
    pub src_keys_by_namespaces: SecondaryMap<NamespaceKey, SourceKey>,

    pub named_asts: SecondaryMap<SourceKey, NamedAst>,
}

impl NamedContext
{
    pub fn clear(&mut self)
    {
        self.namespaces.clear();
        self.namespaces_by_src_keys.clear();
        self.named_asts.clear();
    }

    pub fn insert_namespace(
        &mut self,
        source_key: SourceKey,
        namespace: Namespace,
    ) -> NamespaceKey
    {
        let nk = self.namespaces.insert(namespace);

        self.namespaces_by_src_keys.insert(source_key, nk);

        nk
    }

    delegate! {
        to self.namespaces {
            #[call(get)]
            #[unwrap]
            #[inline]
            pub fn get_namespace(&self, namespace_key: NamespaceKey) -> &Namespace;
            #[call(get_mut)]
            #[unwrap]
            #[inline]
            pub fn get_namespace_mut(&mut self, namespace_key: NamespaceKey) -> &mut Namespace;
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
        to self.namespaces_by_src_keys {
            #[call(get)]
            #[unwrap]
            #[inline]
            pub fn get_namespace_key_by_source_key(&mut self, source_key: SourceKey) -> &NamespaceKey;
        }
    }
}

#[derive(Debug, Clone, Constructor)]
pub struct Namespace
{
    pub exports: BTreeMap<String, Export>,
}

#[derive(Debug, Clone, Copy, derive_more::IsVariant)]
pub enum Export
{
    Local(LocalKey),
    Namespace(NamespaceKey),
}
