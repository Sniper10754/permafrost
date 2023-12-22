use core::ops::Index;

use alloc::{collections::BTreeMap, string::String};

/// Map representing a list of pairs composed by source identifiers and source codes
#[derive(Debug, Default)]
pub struct SourceMap(BTreeMap<SourceId, SourceDescription>);

impl SourceMap {
    #[must_use] pub fn new() -> Self {
        Self::default()
    }

    #[must_use] pub fn get(&self, src_id: SourceId) -> Option<&SourceDescription> {
        self.0.get(&src_id)
    }

    pub fn insert(&mut self, src_id: SourceId, source_description: SourceDescription) {
        self.0.insert(src_id, source_description);
    }

    pub fn iter(&self) -> impl Iterator<Item = (&SourceId, &SourceDescription)> {
        self.0.iter()
    }
}

impl Index<SourceId> for SourceMap {
    type Output = SourceDescription;

    fn index(&self, index: SourceId) -> &Self::Output {
        &self.0[&index]
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::From,
    derive_more::Display,
)]

pub struct SourceId(pub usize);

#[derive(Debug, derive_more::Display)]
#[display(fmt = "{url}")]
pub struct SourceDescription {
    pub url: SourceUrl,
    pub source_code: String,
}

#[derive(Debug, derive_more::Display)]
pub enum SourceUrl {
    #[cfg(feature = "std")]
    #[display(fmt = "{}", "_0.display()")]
    Path(std::path::PathBuf),

    String(String),

    Unknown,
}
