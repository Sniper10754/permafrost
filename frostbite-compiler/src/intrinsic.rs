use alloc::{collections::BTreeMap, string::String};
use frostbite_bytecode::Module;

use crate::tir::TypeKey;

#[derive(Debug, Default, derive_more::From)]
pub struct IntrinsicContext
{
    pub symbols: BTreeMap<String, TypeKey>,
    pub modules: BTreeMap<String, Module>,
}
