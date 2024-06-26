#![no_std]

#[cfg(feature = "std")]
extern crate std;

extern crate alloc;

use alloc::{string::String, vec::Vec};
use core::{fmt::Debug, num::ParseIntError, str::FromStr};
use slotmap::{new_key_type, SlotMap};

use ciborium::de::Error;
use serde::{Deserialize, Serialize};

use derive_more::*;

pub mod text_repr;

new_key_type! {
    #[derive(derive_more::Display)]
    #[display(fmt = "constant #{}", "_0.as_ffi() as u32")]
    pub struct ConstantKey;

    #[derive(derive_more::Display)]
    #[display(fmt = "function #{}", "_0.as_ffi() as u32")]
    pub struct FunctionKey;
}

#[derive(
    Debug, derive_more::Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize,
)]
#[display(fmt = "{_0}.{_1}.{_2}")]
pub struct CompilerVersion(u64, u64, u64);

impl FromStr for CompilerVersion
{
    type Err = CompilerVersionParseError;

    fn from_str(string: &str) -> Result<Self, Self::Err>
    {
        let [major, minor, patch] = string
            .split('.')
            .map(u64::from_str)
            .collect::<Result<Vec<_>, _>>()?
            .try_into()
            .map_err(|_| CompilerVersionParseError::TooMuchDigits)?;

        Ok(CompilerVersion(major, minor, patch))
    }
}

#[derive(Debug, derive_more::Display, derive_more::From)]
pub enum CompilerVersionParseError
{
    ParseIntError(ParseIntError),
    TooMuchDigits,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest
{
    pub emitted_by_compiler_version: CompilerVersion,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[repr(u8)]
pub enum Instruction
{
    /// Import a module at runtime,
    Import
    {
        module: String
    },

    /// Import a symbol from a module at runtime,
    ImportFromModule
    {
        module: String, symbol: String
    },

    /// Loads a constant element into the stack from the constants pool
    LoadConstant(ConstantKey),

    /// Loads a function element into the stack from the functions pool
    LoadFunction(FunctionKey),

    /// Stores the last value onto the stack as name
    StoreName(String),

    /// Loads the value from name
    LoadName(String),

    /// Removes an element from the stack
    Pop,

    /// Removes an element from the stack and loads it onto name 0
    PopAndStoreName(String),

    /// Calls a function
    Call,

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

    /// Compares the top two elements on the stack, stores the result as a boolean in the `EQ` register
    Cmp,

    /// Jumps to determinate function if the `EQ` register is set to 1 | true
    CallEq,

    /// Jumps to determinate function if the `EQ` register is set to 0 | false
    CallNe,

    /// Does nothing
    Nop,
}

#[derive(
    Debug, Clone, PartialEq, Serialize, Deserialize, derive_more::Display, derive_more::From,
)]
pub enum ConstantValue
{
    Int(i32),
    Float(f32),
    String(String),
    Bool(bool),

    #[display(fmt = "fn ")]
    Function(Function),

    Unit,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, From)]
pub struct Function
{
    pub body: Vec<Instruction>,
}

#[derive(Debug, Clone, Serialize, Deserialize, From)]
pub struct Module
{
    pub manifest: Manifest,
    pub globals: Globals,
    pub body: Vec<Instruction>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize, From)]
pub struct Globals
{
    pub constants_pool: SlotMap<ConstantKey, ConstantValue>,
    pub functions: SlotMap<FunctionKey, Function>,
}

#[derive(Debug, Clone, Copy, Display)]
#[cfg_attr(feature = "std", derive(Error))]
pub struct DeserializationError;

impl<T> From<Error<T>> for DeserializationError
{
    fn from(_: Error<T>) -> Self
    {
        Self
    }
}

pub fn encode(
    module: &Module,
    buf: &mut Vec<u8>,
)
{
    ciborium::into_writer(module, buf).unwrap();
}

pub fn decode(bytes: &[u8]) -> Result<Module, DeserializationError>
{
    let module = ciborium::from_reader(bytes)?;

    Ok(module)
}
