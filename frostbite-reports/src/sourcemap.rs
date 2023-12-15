use core::ops::Index;

use alloc::{borrow::Cow, collections::BTreeMap};

/// Map representing a list of pairs composed by source identifiers and source codes
#[derive(Debug, Default)]
pub struct SourceMap<'id, 'src> {
    pairs: BTreeMap<SourceId<'id>, &'src str>,
}

impl<'id, 'src> SourceMap<'id, 'src> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&mut self, src_id: &SourceId<'id>) -> Option<&'src str> {
        self.pairs.get(src_id).map(|src| *src)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&SourceId<'id>, &'src str)> {
        self.pairs.iter().map(|(id, src)| (id, *src))
    }
}

impl<'id, 'src> Index<&SourceId<'id>> for SourceMap<'id, 'src> {
    type Output = str;

    fn index(&self, index: &SourceId<'id>) -> &Self::Output {
        self.pairs[index]
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, derive_more::From)]
pub enum SourceId<'id> {
    #[cfg(feature = "std")]
    Filepath(std::path::PathBuf),

    String(Cow<'id, str>),

    Undefined,
}

impl<'id> SourceId<'id> {
    pub fn matches(&self, other: &str) -> bool {
        match self {
            #[cfg(feature = "std")]
            SourceId::Filepath(this) => this.as_os_str() == other,

            SourceId::String(this) => this == other,

            SourceId::Undefined => false,
        }
    }
}

impl<'id> PartialEq<str> for SourceId<'id> {
    fn eq(&self, other: &str) -> bool {
        self.matches(other)
    }
}
