#![no_std]

extern crate alloc;

use alloc::{collections::BTreeMap, string::String, vec::Vec};
use core::fmt::Debug;

/// !: https://github.com/near/borsh#specification
use borsh::{BorshDeserialize, BorshSerialize};

pub type PoolIndex = usize;
pub type Pool<T> = BTreeMap<PoolIndex, T>;

#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
#[borsh(use_discriminant = true)]
pub enum BytecodeVersion {
    Number(f32),
}

#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct Manifest {
    pub bytecode_version: BytecodeVersion,
}

#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
#[borsh(use_discriminant = true)]
#[repr(u8)]
pub enum Instruction {
    /// Loads a constant element into the stack from the constants pool
    Load(PoolIndex) = 1,

    /// Removes an element from the stack
    Pop = 2,

    /// Calls a function
    Call(PoolIndex) = 3,

    /// Returns from a function
    Return,

    /// Perform an `Add` operation on the last two elements on the stack.
    Add,
    /// Perform an `Sub` operation on the last two elements on the stack.
    Sub,
    /// Perform an `Mul` operation on the last two elements on the stack.
    Mul,
    /// Perform an `Div` operation on the last two elements on the stack.
    Div,

    /// Does nothing
    Nop,
}

#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub enum ConstantValue {
    Int(i32),
    Float(f32),
    String(String),
}

#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct Function {
    pub body: Vec<Instruction>,
}

#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct Module {
    pub manifest: Manifest,
    pub constants_pool: Pool<ConstantValue>,
    pub functions: Vec<Function>,
    pub body: Vec<Instruction>,
}

#[derive(Debug, Clone, Copy)]
pub struct DeserializationError;

impl From<borsh::io::Error> for DeserializationError {
    fn from(_value: borsh::io::Error) -> Self {
        Self
    }
}

pub fn encode(module: &Module, buf: &mut Vec<u8>) {
    BorshSerialize::serialize(module, buf).expect("writing to a vec should not fail");
}

pub fn decode(bytes: &mut &[u8]) -> Result<Module, DeserializationError> {
    BorshDeserialize::deserialize(bytes).map_err(|err| err.into())
}
