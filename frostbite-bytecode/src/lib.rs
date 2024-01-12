#![no_std]

#[cfg(feature = "std")]
extern crate std;

extern crate alloc;

use alloc::{string::String, vec::Vec};
use core::fmt::Debug;
use slotmap::{new_key_type, SlotMap};

use ciborium::de::Error;
use serde::{Deserialize, Serialize};

pub mod text_repr;

new_key_type! {
    #[derive(derive_more::Display)]
    #[display(fmt = "constant #{}", "_0.as_ffi() as u32")]
    pub struct ConstantIndex;

    #[derive(derive_more::Display)]
    #[display(fmt = "function #{}", "_0.as_ffi() as u32")]
    pub struct FunctionIndex;
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest {
    pub bytecode_version: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[repr(u8)]
pub enum Instruction {
    /// Loads a constant element into the stack from the constants pool
    LoadConstant(ConstantIndex),

    /// Stores the last value onto the stack as name
    StoreName(String),

    /// Loads the value from name
    LoadName(String),

    /// Removes an element from the stack
    Pop,

    /// Calls a function
    Call(FunctionIndex),

    /// Returns from a function
    Return,

    /// Perform an `Add` operation on the last top two elements on the stack, overwrites the top element on the stack with the result
    Add,
    /// Perform an `Sub` operation on the last top two elements on the stack, overwrites the top element on the stack with the result
    Subtract,
    /// Perform an `Mul` operation on the last top two elements on the stack, overwrites the top element on the stack with the result
    Multiply,
    /// Perform an `Div` operation on the last top two elements on the stack, overwrites the top element on the stack with the result
    Divide,

    /// Compares the top two elements on the stack, stores the result as a boolean in the `CR` register
    Cmp,

    /// Jumps to determinate function if the `CR` register is set to 1 | true
    CallEq(FunctionIndex),

    /// Jumps to determinate function if the `CR` register is set to 0 | false
    CallNe(FunctionIndex),

    /// Does nothing
    Nop,
}

#[derive(Debug, Clone, derive_more::Display, Serialize, Deserialize, derive_more::From)]
pub enum ConstantValue {
    Int(i32),
    Float(f32),
    String(String),
    Bool(bool),

    #[display(fmt = "fn ")]
    Function(Function),

    Unit,
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

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
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
