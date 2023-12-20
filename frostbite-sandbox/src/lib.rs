#![no_std]

extern crate std;

extern crate alloc;

use internals::Shared;
use stack::StackFrame;

/// Touch only in case you are working with the runtime internals
pub mod internals;
pub mod intrinsic;

pub mod error;
pub mod eval;
pub mod value;

mod helper;
mod math;
mod stack;
