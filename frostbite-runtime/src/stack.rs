use alloc::{collections::BTreeMap, vec, vec::Vec};
use frostbite_reports::sourcemap::SourceId;

use crate::{internals::Shared, value::Value};

#[derive(Debug, Clone, derive_more::Deref, derive_more::DerefMut)]
pub struct Stack<'id, 'ast>(Vec<StackFrame<'id, 'ast>>);

impl<'id, 'ast> Default for Stack<'id, 'ast> {
    fn default() -> Self {
        Self(vec![StackFrame::new(SourceId::Undefined, None)])
    }
}

impl<'id, 'ast, T: IntoIterator<Item = StackFrame<'id, 'ast>>> From<T> for Stack<'id, 'ast> {
    fn from(value: T) -> Self {
        Self(value.into_iter().collect())
    }
}

#[derive(Debug, Clone)]
pub struct StackFrame<'id, 'ast> {
    pub symbols: BTreeMap<&'ast str, Shared<Value<'id, 'ast>>>,
    pub frame: frostbite_reports::Frame<'id>,
}

impl<'id, 'ast> StackFrame<'id, 'ast> {
    pub fn new(source_id: SourceId<'id>, position: Option<frostbite_reports::Position>) -> Self {
        Self {
            symbols: BTreeMap::default(),
            frame: frostbite_reports::Frame {
                source_id,
                position,
            },
        }
    }
}
