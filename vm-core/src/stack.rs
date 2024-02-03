use core::ops::{Deref, DerefMut};

use alloc::{collections::BTreeMap, string::String, vec::Vec};

use delegate::delegate;
use derive_more::*;
use frostbite_bytecode::ConstantValue;

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
            #[unwrap]
            pub fn get_local(&self, name: &str) -> &StackValue;
        }
        to self.plates.first_mut().unwrap().names {
            #[call(insert)]
            pub fn insert_local(&mut self, name: String, value: StackValue);
            #[call(get_mut)]
            #[unwrap]
            pub fn get_local_mut(&mut self, name: &str) -> &StackValue;
        }
        to self.plates.first_mut().unwrap().stack {
            pub fn push(&mut self, value: StackValue);

            #[unwrap]
            pub fn pop(&mut self) -> StackValue;
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
    type Target = [StackValue];

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
    names: BTreeMap<String, StackValue>,

    #[deref]
    #[deref_mut]
    stack: Vec<StackValue>,
}

impl Plate
{
    delegate! {
        to self.names {
            #[call(insert)]
            pub fn insert_local(&mut self, name: String, value: StackValue);
            #[call(get)]
            #[unwrap]
            pub fn get_local(&self, name: &str) -> &StackValue;
            #[call(get_mut)]
            #[unwrap]
            pub fn get_local_mut(&mut self, name: &str) -> &StackValue;
        }
        to self.stack {
            pub fn push(&mut self, value: StackValue);
            pub fn pop(&mut self) -> Option<StackValue>;
        }
    }
}

#[derive(Debug, Clone, PartialEq, From)]
pub enum StackValue
{
    Int(i32),
    Float(f32),
    String(String),
    Bool(bool),
    Function(frostbite_bytecode::Function),
    Nil,
}

impl From<ConstantValue> for StackValue
{
    fn from(value: ConstantValue) -> Self
    {
        match value {
            ConstantValue::Int(value) => Self::from(value),
            ConstantValue::Float(value) => Self::from(value),
            ConstantValue::String(value) => Self::from(value),
            ConstantValue::Bool(value) => Self::from(value),
            ConstantValue::Function(value) => Self::from(value),
            ConstantValue::Unit => Self::Nil,
        }
    }
}
