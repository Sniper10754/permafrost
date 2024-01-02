use core::ops::Index;

use alloc::string::String;
use slotmap::{new_key_type, SlotMap};

new_key_type! {
    #[derive(derive_more::Display)]
    #[display(fmt = "{}", "_0.as_ffi() as u32")]
    pub struct SourceId;
}

/// Map representing a list of pairs composed by source identifiers and source codes
#[derive(Debug)]
pub struct SourceMap(SlotMap<SourceId, SourceDescription>);

impl Default for SourceMap {
    fn default() -> Self {
        Self::new()
    }
}

impl SourceMap {
    #[must_use]
    pub fn new() -> Self {
        Self(Default::default())
    }

    #[must_use]
    pub fn get(&self, src_id: SourceId) -> Option<&SourceDescription> {
        self.0.get(src_id)
    }

    pub fn insert(&mut self, source_description: SourceDescription) -> SourceId {
        self.0.insert(source_description)
    }

    pub fn iter(&self) -> impl Iterator<Item = (SourceId, &SourceDescription)> {
        self.0.iter()
    }
}

impl Index<SourceId> for SourceMap {
    type Output = SourceDescription;

    fn index(&self, index: SourceId) -> &Self::Output {
        &self.0[index]
    }
}

#[derive(Debug, Clone, derive_more::Display)]
#[display(fmt = "{url}")]
pub struct SourceDescription {
    pub url: SourceUrl,
    pub source_code: String,
}

#[derive(Debug, Clone, derive_more::Display, derive_more::From)]
pub enum SourceUrl {
    #[cfg(feature = "std")]
    #[display(fmt = "{}", "_0.display()")]
    Path(std::path::PathBuf),

    String(String),

    Unknown,
}
