use core::ops::Index;

use alloc::collections::BTreeMap;

/// Map representing a list of pairs composed by source identifiers and source codes
#[derive(Debug, Default)]
pub struct SourceMap<'id, 'src> {
    pairs: BTreeMap<SourceId<'id>, &'src str>,
}

impl<'id, 'src> SourceMap<'id, 'src> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, src_id: &SourceId<'id>) -> Option<&'src str> {
        self.pairs.get(src_id).copied()
    }

    pub fn insert(&mut self, src_id: SourceId<'id>, source_code: &'src str) {
        self.pairs.insert(src_id, source_code);
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
pub enum SourceId<'id> {
    #[cfg(feature = "std")]
    #[display(fmt = "{}", "_0.to_string_lossy()")]
    Filepath(&'id std::path::Path),

    String(&'id str),

    Undefined,
}
