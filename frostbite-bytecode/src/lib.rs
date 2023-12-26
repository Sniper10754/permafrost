#![no_std]

extern crate alloc;

use alloc::{string::String, vec::Vec};
use ciborium::de::Error;
use core::fmt::Debug;
use slab::Slab;

/// !: https://github.com/near/borsh#specification
use serde::{Deserialize, Serialize};

pub type PoolIndex = usize;
pub type Pool<T> = Slab<T>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BytecodeVersion {
    Number(f32),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest {
    pub bytecode_version: BytecodeVersion,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Serialize, Deserialize, derive_more::From)]
pub enum ConstantValue {
    Int(i32),
    Float(f32),
    String(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Function {
    pub body: Vec<Instruction>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    pub manifest: Manifest,
    pub globals: Globals,
    pub body: Vec<Instruction>,
}

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct Globals {
    pub constants_pool: Pool<ConstantValue>,
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone, Copy)]
pub struct DeserializationError;

impl<T> From<Error<T>> for DeserializationError {
    fn from(_: Error<T>) -> Self {
        Self
    }
}

pub fn encode(module: &Module, buf: &mut Vec<u8>) {
    ciborium::into_writer(module, buf).unwrap();
}

pub fn decode(bytes: &[u8]) -> Result<Module, DeserializationError> {
    let module = ciborium::from_reader(bytes)?;

    Ok(module)
}
