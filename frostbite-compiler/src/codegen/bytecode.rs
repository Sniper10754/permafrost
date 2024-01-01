use alloc::vec::Vec;
use frostbite_bytecode::{
    BytecodeVersion, Function, FunctionIndex, Globals, Instruction, Manifest, Module,
};
use frostbite_parser::ast::{tokens::BinaryOperatorKind, Spanned};
use frostbite_reports::ReportContext;
use slotmap::SecondaryMap;

use crate::tir::{self, TypedAst, TypedExpression, TypedFunctionExpr};

use super::{CodegenBackend, CodegenError};

#[derive(Debug, Default)]
pub struct BytecodeCodegenBackend {
    functions: SecondaryMap<tir::TypeIndex, FunctionIndex>,
}

impl BytecodeCodegenBackend {
    fn compile_program(&mut self, t_ir: &TypedAst, module: &mut Module) {
        let body = &mut module.body;
        let globals = &mut module.globals;

        for node in &t_ir.nodes {
            self.compile_node(body, globals, node);
        }
    }

    fn compile_node(
        &mut self,
        instructions: &mut Vec<Instruction>,
        globals: &mut Globals,
        t_ir_node: &TypedExpression,
    ) {
        match t_ir_node {
            TypedExpression::Int(..)
            | TypedExpression::Float(..)
            | TypedExpression::Bool(..)
            | TypedExpression::String(..) => {
                self.compile_constant(instructions, globals, t_ir_node)
            }

            TypedExpression::Ident {
                type_index: _,
                refers_to: _,
                str_value: Spanned(_, name),
            } => {
                instructions.push(Instruction::LoadName(name.into()));
            }

            TypedExpression::BinaryOperation { lhs, operator, rhs } => {
                self.compile_binary_operation(instructions, globals, lhs, operator.kind, rhs)
            }

            TypedExpression::Assign {
                local_index: _,
                lhs,
                value,
            } => {
                self.compile_assignment(globals, instructions, lhs, value);
            }

            TypedExpression::Function(TypedFunctionExpr {
                type_index,
                body: function_body,
                ..
            }) => self.compile_function_node(globals, function_body, *type_index),

            TypedExpression::Call {
                callee, arguments, ..
            } => self.compile_function_call(globals, instructions, callee, arguments),

            TypedExpression::Return(_, Spanned(_, value)) => {
                self.compile_node(instructions, globals, value);

                instructions.push(Instruction::Return);
            }

            TypedExpression::Block { expressions } => expressions
                .iter()
                .for_each(|expr| self.compile_node(instructions, globals, expr)),

            TypedExpression::Poisoned | TypedExpression::Uninitialized => unreachable!(),
        }
    }

    fn compile_constant(
        &mut self,
        instructions: &mut Vec<Instruction>,
        globals: &mut Globals,
        node: &TypedExpression,
    ) {
        let constant_index = globals.constants_pool.insert(match node {
            TypedExpression::Int(Spanned(_, constant)) => (*constant).into(),
            TypedExpression::Float(Spanned(_, constant)) => (*constant).into(),
            TypedExpression::String(Spanned(_, constant)) => constant.clone().into(),

            _ => unreachable!(),
        });

        instructions.push(Instruction::LoadConstant(constant_index));
    }

    fn compile_binary_operation(
        &mut self,
        instructions: &mut Vec<Instruction>,
        globals: &mut Globals,
        lhs: &TypedExpression,
        operator: BinaryOperatorKind,
        rhs: &TypedExpression,
    ) {
        self.compile_node(instructions, globals, rhs);
        self.compile_node(instructions, globals, lhs);

        match operator {
            BinaryOperatorKind::Add => instructions.push(Instruction::Add),
            BinaryOperatorKind::Sub => instructions.push(Instruction::Subtract),
            BinaryOperatorKind::Mul => instructions.push(Instruction::Multiply),
            BinaryOperatorKind::Div => instructions.push(Instruction::Divide),
            BinaryOperatorKind::Equal => {
                instructions.push(Instruction::Cmp);

                // binary operations must produce the result on the stack
                // we can achieve this using function calls pushing a result on the stack

                {
                    let true_temp_function =
                        self.compile_function(globals, &TypedExpression::Bool(Spanned(0..0, true)));

                    let true_temp_function_index = globals.functions.insert(true_temp_function);

                    instructions.push(Instruction::CallIf(true_temp_function_index))
                }

                {
                    let false_temp_function = self
                        .compile_function(globals, &TypedExpression::Bool(Spanned(0..0, false)));

                    let false_temp_function_index = globals.functions.insert(false_temp_function);

                    instructions.push(Instruction::Call(false_temp_function_index));
                }

                {
                    let true_temp_function = self.compile_function(
                        globals,
                        &TypedExpression::Bool(Spanned(Default::default(), true)),
                    );

                    let true_temp_function_index = globals.functions.insert(true_temp_function);

                    instructions.push(Instruction::CallIf(true_temp_function_index));
                }
            }
        };
    }

    fn compile_assignment(
        &mut self,
        globals: &mut Globals,
        instructions: &mut Vec<Instruction>,
        lhs: &tir::Assignable,
        value: &TypedExpression,
    ) {
        self.compile_node(instructions, globals, value);

        match lhs {
            tir::Assignable::Ident(_, Spanned(_, name)) => {
                instructions.push(Instruction::StoreName(name.into()));
            }
        };
    }

    fn compile_function_node(
        &mut self,
        globals: &mut Globals,
        function_body: &TypedExpression,
        type_index: tir::TypeIndex,
    ) {
        let function = self.compile_function(globals, function_body);

        let function_index = globals.functions.insert(function);

        self.functions.insert(type_index, function_index);
    }

    fn compile_function(
        &mut self,
        globals: &mut Globals,
        function_body: &TypedExpression,
    ) -> frostbite_bytecode::Function {
        let mut bytecode_function_body = Vec::new();

        self.compile_node(&mut bytecode_function_body, globals, function_body);

        bytecode_function_body.push(Instruction::Return);

        Function {
            body: bytecode_function_body,
        }
    }

    fn compile_function_call(
        &mut self,
        globals: &mut Globals,
        instructions: &mut Vec<Instruction>,
        callee: &tir::Callable,
        arguments: &[TypedExpression],
    ) {
        arguments.iter().for_each(|argument| {
            self.compile_node(instructions, globals, argument);
        });

        match callee {
            tir::Callable::Ident(type_index, _) => {
                let function_index = self.functions[*type_index];

                instructions.push(Instruction::Call(function_index));
            }
        };
    }
}

impl CodegenBackend for BytecodeCodegenBackend {
    type Output = Module;

    fn codegen(
        mut self,
        _report_ctx: &mut ReportContext,
        t_ir: &TypedAst,
    ) -> Result<Self::Output, CodegenError> {
        let mut module = Module {
            manifest: Manifest {
                bytecode_version: BytecodeVersion::Experimental(),
            },
            globals: Globals::default(),
            body: Vec::new(),
        };

        self.compile_program(t_ir, &mut module);

        Ok(module)
    }
}
