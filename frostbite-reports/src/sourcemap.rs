use core::ops::Index;

use alloc::{collections::BTreeMap, string::String};

/// Map representing a list of pairs composed by source identifiers and source codes
#[derive(Debug)]
pub struct SourceMap(BTreeMap<SourceId, SourceDescription>, SourceId);

impl Default for SourceMap {
    fn default() -> Self {
        Self::new()
    }
}

impl SourceMap {
    #[must_use]
    pub fn new() -> Self {
        Self(BTreeMap::new(), SourceId(0))
    }

    #[must_use]
    pub fn get(&self, src_id: SourceId) -> Option<&SourceDescription> {
        self.0.get(&src_id)
    }

    pub fn push(&mut self, source_description: SourceDescription) -> SourceId {
        self.1 += SourceId(1);

        self.0.insert(self.1, source_description);

        self.1
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
    derive_more::Add,
    derive_more::AddAssign,
    derive_more::Sub,
    derive_more::SubAssign,
    derive_more::Div,
    derive_more::DivAssign,
    derive_more::Mul,
    derive_more::MulAssign,
    derive_more::From,
    derive_more::Display,
)]

pub struct SourceId(pub usize);

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
