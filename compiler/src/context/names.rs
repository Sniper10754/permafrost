use alloc::{string::String, vec::Vec};
use dbg_pls::DebugPls;
use frostbite_reports::sourcemap::{SourceKey, SourceUrl};
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

#[derive(Debug, Clone, Default)]
pub struct ModuleImportError;

#[derive(Debug, Clone, Default)]
pub struct NamedContext
{
    pub modules: SlotMap<NamedModuleKey, NamedModule>,
    pub modules_to_srcs: SecondaryMap<SourceKey, NamedModuleKey>,

    pub named_asts: SecondaryMap<SourceKey, NamedAst>,
}

#[derive(Debug, Clone)]
pub struct NamedModule
{
    pub src_id: SourceKey,
    pub exports: Vec<Export>,
}

#[derive(Debug, Clone)]
pub struct Export
{
    visibility: Visibility,
    kind: ExportKind,
}

#[derive(Debug, Clone)]
pub enum ExportKind
{
    Local(LocalKey),
    Module(NamedModuleKey),
}

#[derive(Debug, Clone)]
pub enum Visibility
{
    Global,

    Local(LocalIdentifier),
}

#[derive(Debug, Clone)]
pub enum LocalIdentifier
{
    Anonymous,
    Sparse(String),

    #[cfg(feature = "std")]
    PathBuf(std::path::PathBuf),
}

impl From<SourceUrl> for LocalIdentifier
{
    fn from(value: SourceUrl) -> Self
    {
        match value {
            SourceUrl::Anonymous => Self::Anonymous,
            SourceUrl::Sparse(value) => Self::Sparse(value),

            #[cfg(feature = "std")]
            SourceUrl::PathBuf(value) => Self::PathBuf(value),
        }
    }
}
