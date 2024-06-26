#[cfg(feature = "std")]
extern crate std;

use alloc::string::String;
use slotmap::{new_key_type, SlotMap};

new_key_type! {
    #[derive(derive_more::Display)]
    #[display(fmt = "{}", "_0.as_ffi() as u32")]
    pub struct SourceKey;
}

/// Map representing a list of pairs composed by source identifiers and source codes
#[derive(
    Debug,
    Default,
    derive_more::Deref,
    derive_more::DerefMut,
    derive_more::Index,
    derive_more::IndexMut,
)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct SourceMap(SlotMap<SourceKey, SourceDescription>);

#[derive(Debug, Clone, derive_more::Display)]
#[display(fmt = "{url}")]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct SourceDescription
{
    pub url: SourceUrl,
    pub source_code: String,
}

#[derive(Debug, Clone, derive_more::Display, derive_more::From)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum SourceUrl
{
    #[cfg(feature = "std")]
    #[display(fmt = "{}", "_0.display()")]
    PathBuf(std::path::PathBuf),

    Sparse(String),

    Anonymous,
}

impl<'a> From<&'a str> for SourceUrl
{
    fn from(value: &'a str) -> Self
    {
        Self::Sparse(value.into())
    }
}
