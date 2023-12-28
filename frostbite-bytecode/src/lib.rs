#![no_std]

#[cfg(feature = "std")]
extern crate std;

extern crate alloc;

use alloc::{string::String, vec::Vec};
use core::fmt::Debug;
use slotmap::{new_key_type, SlotMap};

use ciborium::de::Error;
use serde::{Deserialize, Serialize};
use variant_name::VariantName;
use variant_name_derive::VariantName;

new_key_type! {
    pub struct ConstantIndex;
    pub struct FunctionIndex;
}

pub mod text_repr;

#[derive(Debug, Clone, derive_more::Display, Serialize, Deserialize)]
pub enum BytecodeVersion {
    Number(f32),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest {
    pub bytecode_version: BytecodeVersion,
}

#[derive(Debug, Clone, VariantName, Serialize, Deserialize)]
#[repr(u8)]
pub enum Instruction {
    /// Loads a constant element into the stack from the constants pool
    Load(ConstantIndex) = 1,

    /// Removes an element from the stack
    Pop = 2,

    /// Calls a function
    Call(ConstantIndex) = 3,

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

#[derive(Debug, Clone, derive_more::Display, Serialize, Deserialize, derive_more::From)]
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
    pub constants_pool: SlotMap<ConstantIndex, ConstantValue>,
    pub functions: SlotMap<FunctionIndex, Function>,
}

#[derive(Debug, Clone, Copy, derive_more::Display)]
#[cfg_attr(feature = "std", derive(derive_more::Error))]
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
