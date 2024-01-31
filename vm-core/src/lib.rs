#![no_std]

extern crate alloc;

use frostbite_bytecode::{ConstantKey, FunctionKey, Instruction, Module};
use registers::Registers;
use stack::{Stack, StackValue};

pub mod registers;
pub mod stack;

pub struct VirtualMachine
{
    registers: Registers,
    stack_frames: Stack,
}

impl VirtualMachine
{
    pub fn registers(&self) -> &Registers
    {
        &self.registers
    }

    pub fn run(
        mut self,
        module: &Module,
    )
    {
        self.run_instructions(module, &module.body)
    }

    fn run_instructions(
        &mut self,
        module: &Module,
        instructions: &[Instruction],
    )
    {
        for (instruction, instruction_index) in instructions.iter().zip(1..) {
            self.registers.pc = instruction_index;

            self.evaluate_instruction(module, instruction)
        }
    }

    fn evaluate_instruction(
        &mut self,
        module: &Module,
        instruction: &Instruction,
    )
    {
        match instruction {
            Instruction::Import { module } => self.eval_instruction_import(module),
            Instruction::ImportFromModule { module, symbol } => {
                self.eval_instruction_import_from_module(module, symbol)
            }
            Instruction::LoadConstant(constant) => {
                self.eval_instruction_load_constant(module, *constant)
            }
            Instruction::LoadFunction(function) => {
                self.eval_instruction_load_function(module, *function)
            }
            Instruction::StoreName(name) => self.eval_instruction_store_name(name),
            Instruction::LoadName(name) => self.eval_instruction_load_name(name),
            Instruction::Pop => {
                self.eval_instruction_pop();
            }
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
        module: &Module,
        constant_key: ConstantKey,
    )
    {
        let value = module.globals.constants_pool[constant_key].clone().into();

        self.stack_frames.front_mut().unwrap().push_front(value)
    }

    fn eval_instruction_load_function(
        &mut self,
        module: &Module,
        function_key: FunctionKey,
    )
    {
        let function = module.globals.functions[function_key].clone();

        self.stack_frames
            .front_mut()
            .unwrap()
            .push_front(function.into())
    }

    fn eval_instruction_store_name(
        &mut self,
        name: &str,
    )
    {
        let value_to_bind = self.stack_frames.front_mut().unwrap().pop_front().unwrap();

        self.stack_frames
            .front_mut()
            .unwrap()
            .names
            .insert(name.into(), value_to_bind);
    }

    fn eval_instruction_load_name(
        &mut self,
        name: &str,
    )
    {
        let value = self
            .stack_frames
            .iter()
            .find_map(|frame| {
                frame
                    .names
                    .contains_key(name)
                    .then_some(frame.names.get(name).unwrap())
            })
            .cloned()
            .unwrap();

        self.stack_frames.front_mut().unwrap().push_front(value);
    }

    fn eval_instruction_pop(&mut self) -> StackValue
    {
        // Implement the logic for the Pop instruction
        self.stack_frames.front_mut().unwrap().pop_front().unwrap()
    }

    fn eval_instruction_pop_and_store_name(
        &mut self,
        name: &str,
    )
    {
        let value = self.eval_instruction_pop();

        self.stack_frames
            .front_mut()
            .unwrap()
            .names
            .insert(name.into(), value);
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
