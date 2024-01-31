#![no_std]

extern crate alloc;

use frostbite_bytecode::{ConstantKey, FunctionKey, Instruction, Module};
use registers::Registers;
use stack::Stack;

pub mod registers;
pub mod stack;

pub struct VirtualMachine
{
    registers: Registers,
    bytecode: Module,
    stack: Stack,
}

impl VirtualMachine
{
    pub fn registers(&self) -> &Registers
    {
        &self.registers
    }

    fn run_instructions(
        &mut self,
        instructions: &[Instruction],
    )
    {
        for (instruction, instruction_index) in instructions.iter().zip(1..) {
            self.registers.pc = instruction_index;

            self.evaluate_instruction(instruction)
        }
    }

    fn evaluate_instruction(
        &mut self,
        instruction: &Instruction,
    )
    {
        match instruction {
            Instruction::Import { module } => self.eval_instruction_import(module),
            Instruction::ImportFromModule { module, symbol } => {
                self.eval_instruction_import_from_module(module, symbol)
            }
            Instruction::LoadConstant(constant) => self.eval_instruction_load_constant(*constant),
            Instruction::LoadFunction(function) => self.eval_instruction_load_function(*function),
            Instruction::StoreName(name) => self.eval_instruction_store_name(name),
            Instruction::LoadName(name) => self.eval_instruction_load_name(name),
            Instruction::Pop => self.eval_instruction_pop(),
            Instruction::PopAndStoreName(name) => self.eval_instruction_pop_and_store_name(name),
            Instruction::Call => self.eval_instruction_call(),
            Instruction::Return => self.eval_instruction_return(),
            Instruction::Add => self.eval_instruction_add(),
            Instruction::Subtract => self.eval_instruction_subtract(),
            Instruction::Multiply => self.eval_instruction_multiply(),
            Instruction::Divide => self.eval_instruction_divide(),
            Instruction::Cmp => self.eval_instruction_cmp(),
            Instruction::CallEq => self.eval_instruction_call_eq(),
            Instruction::CallNe => self.eval_instruction_call_ne(),
            Instruction::Nop => self.eval_instruction_nop(),
        }
    }

    fn eval_instruction_import(
        &mut self,
        module: &str,
    )
    {
        // Implement the logic for the Import instruction
        unimplemented!();
    }

    fn eval_instruction_import_from_module(
        &mut self,
        module: &str,
        symbol: &str,
    )
    {
        // Implement the logic for the ImportFromModule instruction
        unimplemented!();
    }

    fn eval_instruction_load_constant(
        &mut self,
        constant_key: ConstantKey,
    )
    {
        let value = self.bytecode.globals.constants_pool[constant_key]
            .clone()
            .into();

        self.stack.front_mut().unwrap().push_front(value)
    }

    fn eval_instruction_load_function(
        &mut self,
        function_key: FunctionKey,
    )
    {
        // Implement the logic for the LoadFunction instruction
        todo!();
    }

    fn eval_instruction_store_name(
        &mut self,
        name: &str,
    )
    {
        // Implement the logic for the StoreName instruction
        todo!();
    }

    fn eval_instruction_load_name(
        &mut self,
        name: &str,
    )
    {
        // Implement the logic for the LoadName instruction
        todo!();
    }

    fn eval_instruction_pop(&mut self)
    {
        // Implement the logic for the Pop instruction
        todo!();
    }

    fn eval_instruction_pop_and_store_name(
        &mut self,
        name: &str,
    )
    {
        // Implement the logic for the PopAndStoreName instruction
        todo!();
    }

    fn eval_instruction_call(&mut self)
    {
        // Implement the logic for the Call instruction
        todo!();
    }

    fn eval_instruction_return(&mut self)
    {
        // Implement the logic for the Return instruction
        todo!();
    }

    fn eval_instruction_add(&mut self)
    {
        // Implement the logic for the Add instruction
        todo!();
    }

    fn eval_instruction_subtract(&mut self)
    {
        // Implement the logic for the Subtract instruction
        todo!();
    }

    fn eval_instruction_multiply(&mut self)
    {
        // Implement the logic for the Multiply instruction
        todo!();
    }

    fn eval_instruction_divide(&mut self)
    {
        // Implement the logic for the Divide instruction
        todo!();
    }

    fn eval_instruction_cmp(&mut self)
    {
        // Implement the logic for the Cmp instruction
        todo!();
    }

    fn eval_instruction_call_eq(&mut self)
    {
        // Implement the logic for the CallEq instruction
        todo!();
    }

    fn eval_instruction_call_ne(&mut self)
    {
        // Implement the logic for the CallNe instruction
        todo!();
    }

    fn eval_instruction_nop(&mut self)
    {
        // Implement the logic for the Nop instruction
        todo!();
    }
}
