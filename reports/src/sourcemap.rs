#[cfg(feature = "std")]
extern crate std;

use core::ops::Index;

use alloc::string::String;
use slotmap::{new_key_type, SlotMap};

new_key_type! {
    #[derive(derive_more::Display)]
    #[display(fmt = "{}", "_0.as_ffi() as u32")]
    pub struct SourceKey;
}

/// Map representing a list of pairs composed by source identifiers and source codes
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct SourceMap(SlotMap<SourceKey, SourceDescription>);

impl Default for SourceMap
{
    fn default() -> Self
    {
        Self::new()
    }
}

impl SourceMap
{
    #[must_use]
    pub fn new() -> Self
    {
        Self(Default::default())
    }

    #[must_use]
    pub fn get(
        &self,
        src_id: SourceKey,
    ) -> Option<&SourceDescription>
    {
        self.0.get(src_id)
    }

    pub fn insert(
        &mut self,
        source_description: SourceDescription,
    ) -> SourceKey
    {
        self.0.insert(source_description)
    }

    pub fn iter(&self) -> impl Iterator<Item = (SourceKey, &SourceDescription)>
    {
        self.0.iter()
    }
}

impl Index<SourceKey> for SourceMap
{
    type Output = SourceDescription;

    fn index(
        &self,
        index: SourceKey,
    ) -> &Self::Output
    {
        &self.0[index]
    }
}

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
