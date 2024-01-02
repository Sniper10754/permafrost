#![no_std]

extern crate alloc;

use alloc::{borrow::Cow, vec, vec::Vec};

use frostbite_bytecode::{ConstantIndex, FunctionIndex, Instruction, Module};

mod frames;
mod stack_value;

use frames::Frame;
use stack_value::StackValue;

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
                self.load_constant(bytecode, *constant_index);
            }
            Instruction::StoreName(name) => self.store_name(name),
            Instruction::LoadName(name) => self.load_name(name),
            Instruction::Pop => self.pop_stack(),
            Instruction::Call(function_index) => self.call_function(bytecode, *function_index),
            Instruction::CallEq(function_index) => self.call_function_eq(bytecode, *function_index),
            Instruction::CallNe(function_index) => self.call_function_ne(bytecode, *function_index),
            Instruction::Return => self.handle_return(),
            Instruction::Add
            | Instruction::Subtract
            | Instruction::Multiply
            | Instruction::Divide => self.binary_operation(instruction),
            Instruction::Cmp => self.handle_cmp(),
            Instruction::Nop => (),
        }
    }

    fn load_constant(&mut self, bytecode: &Module, constant_index: ConstantIndex) {
        let constant_value = bytecode.globals.constants_pool[constant_index].clone();

        self.stack.push(constant_value.into());
    }

    fn store_name<'a>(&mut self, name: impl Into<Cow<'a, str>>) {
        let value = self.stack.first().unwrap().clone();

        self.frames
            .first_mut()
            .unwrap()
            .names
            .insert(name.into().into_owned(), value);
    }

    fn load_name(&mut self, name: impl AsRef<str>) {
        let value = self
            .frames
            .iter()
            .find_map(|frame| frame.names.get(name.as_ref()))
            .cloned()
            .unwrap();

        self.stack.push(value);
    }

    fn pop_stack(&mut self) {
        self.stack.pop();
    }

    fn call_function(&mut self, bytecode: &Module, function_index: FunctionIndex) {
        let function = bytecode.globals.functions[function_index].clone();

        self.frames.push(Frame::default());

        function
            .body
            .iter()
            .for_each(|i| self.run_instruction(i, bytecode));

        self.pop_stack();
    }

    fn call_function_eq(&mut self, bytecode: &Module, function_index: FunctionIndex) {
        if self.registers.cr {
            self.call_function(bytecode, function_index);
        }
    }

    fn call_function_ne(&mut self, bytecode: &Module, function_index: FunctionIndex) {
        if !self.registers.cr {
            self.call_function(bytecode, function_index);
        }
    }

    fn handle_return(&mut self) {
        // Nothing to do
    }

    fn binary_operation(&mut self, instruction: &Instruction) {
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

    fn handle_cmp(&mut self) {
        let [.., lhs, rhs] = self.stack.as_slice() else {
            unreachable!();
        };

        self.registers.cr = lhs == rhs;
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
