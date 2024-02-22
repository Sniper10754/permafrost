use alloc::{collections::BTreeMap, string::String, vec::Vec};
use delegate::delegate;
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
    namespaces: SlotMap<NamespaceKey, Namespace>,
    named_asts: SecondaryMap<SourceKey, NamedAst>,
}

impl NamedContext
{
    pub fn clear(&mut self)
    {
        self.namespaces.clear();
        self.named_asts.clear();
    }

    pub fn new_ast(&mut self) -> NamedAst
    {
        NamedAst {
            exprs: Vec::new(),
            root_namespace: self.new_namespace(),
        }
    }

    pub fn new_namespace(&mut self) -> NamespaceKey
    {
        self.insert_namespace(Namespace::default())
    }

    pub fn insert_namespace(
        &mut self,
        namespace: Namespace,
    ) -> NamespaceKey
    {
        self.namespaces.insert(namespace)
    }

    pub fn root_namespace_of_ast(
        &self,
        ast_source_key: SourceKey,
    ) -> &Namespace
    {
        let namespace_key = self.get_ast(ast_source_key).root_namespace;

        self.get_namespace(namespace_key)
    }

    pub fn root_namespace_of_ast_mut(
        &mut self,
        ast_source_key: SourceKey,
    ) -> &mut Namespace
    {
        let namespace_key = self.get_ast(ast_source_key).root_namespace;

        self.get_namespace_mut(namespace_key)
    }

    pub fn source_key_by_ast_root_namespace_key(
        &self,
        namespace_key: NamespaceKey,
    ) -> Option<SourceKey>
    {
        self.named_asts
            .iter()
            .find(|(_, ir)| ir.root_namespace == namespace_key)
            .map(|(source_key, _)| source_key)
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
    }
}

#[derive(Debug, Clone, Default)]
pub struct Namespace
{
    pub locals: SlotMap<LocalKey, ()>,
    pub exported_locals: BTreeMap<String, Item>,
}

#[derive(Debug, Clone, Copy, derive_more::IsVariant)]
pub enum Item
{
    Local(LocalKey),
    Namespace(NamespaceKey),
}
