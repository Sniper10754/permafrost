use slotmap::{new_key_type, SlotMap};

use crate::tir::{Type, TypeKey};

new_key_type! {
    pub struct ModuleKey;
}

pub struct Module
{
    pub symbols: SlotMap<TypeKey, Type>,
}
