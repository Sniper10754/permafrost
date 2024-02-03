use core::ops::{Deref, DerefMut};

use alloc::{collections::BTreeMap, string::String, vec::Vec};

use delegate::delegate;
use derive_more::*;

use crate::value::Value;

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Stack
{
    plates: Vec<Plate>,
}

impl Stack
{
    pub fn new() -> Self
    {
        Self {
            plates: [Plate::default()].into(),
        }
    }

    delegate! {
        to self.plates.first().unwrap().names {
            #[call(get)]
            pub fn get_local(&self, name: &str) -> Option<&Value>;
        }
        to self.plates.first_mut().unwrap().names {
            #[call(insert)]
            pub fn insert_local(&mut self, name: String, value: Value);
            #[call(get_mut)]
            #[unwrap]
            pub fn get_local_mut(&mut self, name: &str) -> &Value;
        }
        to self.plates.first_mut().unwrap().stack {
            pub fn push(&mut self, value: Value);
            pub fn pop(&mut self) -> Option<Value>;
        }
    }

    pub fn enter_scope(&mut self)
    {
        self.plates.push(Plate::default())
    }

    pub fn leave_scope(&mut self)
    {
        self.plates.pop();

        debug_assert!(
            !self.plates.is_empty(),
            "There must be at least one plate in the stack"
        );
    }
}

impl Deref for Stack
{
    type Target = [Value];

    fn deref(&self) -> &Self::Target
    {
        self.plates.first().unwrap()
    }
}

impl DerefMut for Stack
{
    fn deref_mut(&mut self) -> &mut Self::Target
    {
        self.plates.first_mut().unwrap()
    }
}

#[derive(Debug, Clone, Default, PartialEq, Deref, DerefMut)]
pub struct Plate
{
    names: BTreeMap<String, Value>,

    #[deref]
    #[deref_mut]
    stack: Vec<Value>,
}

impl Plate
{
    delegate! {
        to self.names {
            #[call(insert)]
            pub fn insert_local(&mut self, name: String, value: Value);
            #[call(get)]
            #[unwrap]
            pub fn get_local(&self, name: &str) -> &Value;
            #[call(get_mut)]
            #[unwrap]
            pub fn get_local_mut(&mut self, name: &str) -> &Value;
        }
        to self.stack {
            pub fn push(&mut self, value: Value);
            pub fn pop(&mut self) -> Option<Value>;
        }
    }
}
