#![no_std]

extern crate alloc;

use alloc::{collections::BTreeMap, string::String, vec, vec::Vec};

use frostbite_bytecode::{ConstantValue, Instruction, Module};

pub struct FrostbiteVm {
    frames: Vec<Frame>,
    stack: Vec<StackValue>,
    registers: Registers,
}

impl FrostbiteVm {
    pub fn new() -> Self {
        Self {
            frames: vec![Frame::default()],
            stack: vec![],
            registers: Registers::default(),
        }
    }

    pub fn run(mut self, bytecode: &Module) {
        for instruction in bytecode.body.iter() {
            self.run_instruction(instruction, bytecode);
        }
    }

    fn run_instruction(&mut self, instruction: &Instruction, bytecode: &Module) {
        match instruction {
            Instruction::LoadConstant(constant_index) => {
                let constant_value = bytecode.globals.constants_pool[*constant_index].clone();

                self.stack.push(StackValue::from(constant_value));
            }
            Instruction::StoreName(name) => {
                let value = self.stack.first().unwrap().clone();

                self.frames
                    .first_mut()
                    .unwrap()
                    .names
                    .insert(name.clone(), value);
            }
            Instruction::LoadName(name) => {
                let value = self
                    .frames
                    .iter()
                    .find_map(|frame| frame.names.get(name))
                    .unwrap();

                self.stack.push(value.clone());
            }
            Instruction::Pop => {
                self.stack.pop();
            }
            Instruction::Call(function_index) => {
                let function = bytecode.globals.functions[*function_index].clone();

                self.frames.push(Frame::default());

                function
                    .body
                    .iter()
                    .for_each(|i| self.run_instruction(i, bytecode));
            }
            Instruction::Return => {
                self.frames.pop();
            }
            Instruction::Add
            | Instruction::Subtract
            | Instruction::Multiply
            | Instruction::Divide => {
                let [.., lhs, rhs] = self.stack.as_mut_slice() else {
                    unreachable!()
                };

                let result = match instruction {
                    Instruction::Add => lhs.clone() + rhs.clone(),
                    Instruction::Subtract => lhs.clone() - rhs.clone(),
                    Instruction::Multiply => lhs.clone() * rhs.clone(),
                    Instruction::Divide => lhs.clone() / rhs.clone(),

                    _ => unreachable!(),
                };

                *rhs = result;
            }
            Instruction::Cmp => {
                let [.., lhs, rhs] = self.stack.as_slice() else {
                    unreachable!();
                };

                self.registers.cr = lhs == rhs;
            }
            Instruction::Nop => todo!(),
        }
    }
}

impl Default for FrostbiteVm {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Default)]
struct Registers {
    cr: bool,
}

#[derive(Debug, Clone, Default)]
struct Frame {
    names: BTreeMap<String, StackValue>,
}

#[derive(Debug, Clone, PartialEq, derive_more::From)]
enum StackValue {
    Int(i32),
    Float(f32),
    String(String),
    Bool(bool),

    Unit,
}

impl core::ops::Add for StackValue {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (StackValue::Int(lhs), StackValue::Int(rhs)) => (lhs + rhs).into(),
            (StackValue::Float(lhs), StackValue::Float(rhs)) => (lhs + rhs).into(),
            (StackValue::Int(lhs), StackValue::Float(rhs)) => (lhs as f32 + rhs).into(),
            (StackValue::Float(lhs), StackValue::Int(rhs)) => (lhs + rhs as f32).into(),

            _ => unreachable!(),
        }
    }
}

impl core::ops::Sub for StackValue {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (StackValue::Int(lhs), StackValue::Int(rhs)) => (lhs - rhs).into(),
            (StackValue::Float(lhs), StackValue::Float(rhs)) => (lhs - rhs).into(),
            (StackValue::Int(lhs), StackValue::Float(rhs)) => (lhs as f32 - rhs).into(),
            (StackValue::Float(lhs), StackValue::Int(rhs)) => (lhs - rhs as f32).into(),

            _ => unreachable!(),
        }
    }
}

impl core::ops::Mul for StackValue {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (StackValue::Int(lhs), StackValue::Int(rhs)) => (lhs * rhs).into(),
            (StackValue::Float(lhs), StackValue::Float(rhs)) => (lhs * rhs).into(),
            (StackValue::Int(lhs), StackValue::Float(rhs)) => (lhs as f32 * rhs).into(),
            (StackValue::Float(lhs), StackValue::Int(rhs)) => (lhs * rhs as f32).into(),

            _ => unreachable!(),
        }
    }
}

impl core::ops::Div for StackValue {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (StackValue::Int(lhs), StackValue::Int(rhs)) => (lhs / rhs).into(),
            (StackValue::Float(lhs), StackValue::Float(rhs)) => (lhs / rhs).into(),
            (StackValue::Int(lhs), StackValue::Float(rhs)) => (lhs as f32 / rhs).into(),
            (StackValue::Float(lhs), StackValue::Int(rhs)) => (lhs / rhs as f32).into(),

            _ => unreachable!(),
        }
    }
}

impl From<ConstantValue> for StackValue {
    fn from(constant_value: ConstantValue) -> Self {
        match constant_value {
            ConstantValue::Int(int) => Self::from(int),
            ConstantValue::Float(float) => Self::from(float),
            ConstantValue::String(string) => Self::from(string),
            ConstantValue::Bool(bool) => Self::from(bool),

            ConstantValue::Unit => Self::Unit,
        }
    }
}
