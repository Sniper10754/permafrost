#![no_std]

extern crate alloc;

use num_traits::Num;
use permafrost_bytecode::{Instruction, Module};

use math::BinaryOperationKind::{self, *};
use registers::Registers;
use stack::Stack;
use value::Value;

pub mod math;
pub mod registers;
pub mod stack;
pub mod value;

#[derive(Debug, Default)]
pub struct Vm
{
    stack: Stack,
    registers: Registers,
}

impl Vm
{
    pub fn new() -> Self
    {
        Vm {
            stack: Stack::new(),
            registers: Registers::default(),
        }
    }

    pub fn execute(
        &mut self,
        instructions: &[Instruction],
        module: &Module,
    )
    {
        for instruction in instructions {
            match instruction {
                Instruction::Import { module: _ } => {
                    // Handle Import instruction
                }
                Instruction::ImportFromModule {
                    module: _,
                    symbol: _,
                } => {
                    // Handle ImportFromModule instruction
                }
                Instruction::LoadConstant(key) => {
                    // Handle LoadConstant instruction
                    let constant = module.globals.constants_pool[*key].clone();

                    self.stack.push(constant.into());
                }
                Instruction::LoadFunction(key) => {
                    // Handle LoadFunction instruction
                    let function = module.globals.functions[*key].clone();

                    self.stack.push(function.into());
                }
                Instruction::StoreName(name) => {
                    // Handle StoreName instruction

                    let value = self.stack.pop().unwrap();

                    self.stack.insert_local(name.into(), value);
                }
                Instruction::LoadName(name) => {
                    // Handle LoadName instruction
                    let value = self.stack.get_local(name).cloned().unwrap();

                    self.stack.push(value);
                }
                Instruction::Pop => {
                    // Handle Pop instruction
                    self.stack.pop();
                }
                Instruction::PopAndStoreName(name) => {
                    // Handle PopAndStoreName instruction
                    let value = self.stack.pop().unwrap();

                    self.stack.insert_local(name.clone(), value);
                }
                Instruction::Call => {
                    // Handle Call instruction
                    self.call_function(module);
                }
                Instruction::Return => {
                    // Handle Return instruction
                    return;
                }
                Instruction::Add => {
                    // Handle Add instruction
                    self.perform_binary_operation(BinaryOperationKind::Add);
                }
                Instruction::Subtract => {
                    // Handle Subtract instruction
                    self.perform_binary_operation(BinaryOperationKind::Sub);
                }
                Instruction::Multiply => {
                    // Handle Multiply instruction
                    self.perform_binary_operation(BinaryOperationKind::Mul);
                }
                Instruction::Divide => {
                    // Handle Divide instruction
                    self.perform_binary_operation(BinaryOperationKind::Div);
                }
                Instruction::Cmp => {
                    // Handle Cmp instruction
                    self.compare_top_two_elements();
                }
                Instruction::CallEq => {
                    // Handle CallEq instruction
                    self.jump_if_eq();
                }
                Instruction::CallNe => {
                    // Handle CallNe instruction
                    self.jump_if_ne();
                }
                Instruction::Nop => {
                    // Handle Nop instruction (do nothing)
                }
            }
        }
    }

    // Helper methods for instruction execution

    fn call_function(
        &mut self,
        module: &Module,
    )
    {
        let function = self.stack.pop().unwrap();

        let Value::Function(function) = function else {
            unreachable!()
        };

        self.stack.enter_scope();

        self.execute(&function.body, module);

        self.stack.leave_scope();
    }

    fn perform_binary_operation(
        &mut self,
        bok: BinaryOperationKind,
    )
    {
        let [.., lhs, rhs] = &mut *self.stack else {
            unreachable!()
        };

        let value: Value = match (lhs, &mut *rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Self::do_binary_operation(bok, *lhs, *rhs).into(),
            (Value::Float(lhs), Value::Float(rhs)) => {
                Self::do_binary_operation(bok, *lhs, *rhs).into()
            }
            (Value::Float(lhs), Value::Int(rhs)) => {
                Self::do_binary_operation(bok, *lhs, *rhs as f32).into()
            }
            (Value::Int(lhs), Value::Float(rhs)) => {
                Self::do_binary_operation(bok, *lhs as f32, *rhs).into()
            }

            (..) => unreachable!(),
        };

        *rhs = value;
    }

    #[inline]
    fn do_binary_operation<N>(
        operator: BinaryOperationKind,
        lhs: N,
        rhs: N,
    ) -> N
    where
        N: Num,
    {
        match operator {
            Add => lhs + rhs,
            Sub => lhs - rhs,
            Mul => lhs * rhs,
            Div => lhs / rhs,
        }
    }

    fn compare_top_two_elements(&mut self)
    {
        let [.., lhs, rhs] = &*self.stack else {
            unreachable!()
        };

        self.registers.eq = lhs == rhs;
    }

    fn jump_if_eq(&mut self)
    {
        if self.registers.eq {
            // Implement jump logic here
        }
    }

    fn jump_if_ne(&mut self)
    {
        if !self.registers.eq {
            // Implement jump logic here
        }
    }

    pub fn stack(&self) -> &Stack
    {
        &self.stack
    }

    pub fn registers(&self) -> Registers
    {
        self.registers
    }
}
