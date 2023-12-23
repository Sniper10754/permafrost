#![no_std]

extern crate alloc;

use alloc::{collections::BTreeMap, string::String, vec, vec::Vec};
use core::fmt::Debug;

/// !: https://github.com/near/borsh#specification
use borsh::{BorshDeserialize, BorshSerialize};
use derive_more::*;

pub type Pool<T> = BTreeMap<PoolIndex, T>;

#[derive(
    Debug,
    Clone,
    Copy,
    Default,
    Hash,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Display,
    From,
    Into,
    Deref,
    DerefMut,
    BorshSerialize,
    BorshDeserialize,
)]
pub struct PoolIndex(pub usize);

#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
#[borsh(use_discriminant = true)]
#[repr(u8)]
pub enum Instruction {
    Load(PoolIndex) = 1,
    Pop = 2,
    Call(PoolIndex) = 3,

    Nop = 127,
}

#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct Manifest {
    pub constants_pool: Pool<ConstantValue>,
    pub symbols_pool: Pool<String>,
}

#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub enum ConstantValue {
    Int(i32),
    Float(f32),
    String(String),
}

#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct Program {
    pub manifest: Manifest,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Clone, Copy)]
pub struct DeserializationError;

impl From<borsh::io::Error> for DeserializationError {
    fn from(_value: borsh::io::Error) -> Self {
        Self
    }
}

pub fn encode(program: Program) -> Vec<u8> {
    let mut buffer = vec![];

    BorshSerialize::serialize(&program, &mut buffer).expect("writing to a vec should not fail");

    buffer
}

pub fn decode(bytes: &mut &[u8]) -> Result<Program, DeserializationError> {
    BorshDeserialize::deserialize(bytes).map_err(|err| err.into())
}
