use alloc::{collections::BTreeMap, string::String};

use crate::stack_value::StackValue;

#[derive(Debug, Clone, Default)]
pub struct Frame {
    pub names: BTreeMap<String, StackValue>,
}
