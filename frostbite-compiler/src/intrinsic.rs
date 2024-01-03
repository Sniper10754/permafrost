use alloc::{collections::BTreeMap, string::String};

use crate::tir::Type;

#[derive(Debug, Default, derive_more::From)]
pub struct IntrinsicContext {
    pub symbols: BTreeMap<String, Type>,
}
