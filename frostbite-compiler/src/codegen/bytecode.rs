use alloc::vec::Vec;
use frostbite_bytecode::{
    BytecodeVersion, ConstantValue, Function, FunctionIndex, Globals, Instruction, Manifest, Module,
};
use frostbite_parser::ast::{tokens::OperatorKind, Spanned};
use frostbite_reports::ReportContext;
use slotmap::SecondaryMap;

use crate::tir::{self, TirNode, TirTree, TypeIndex};

use super::{CodegenBackend, CodegenError};

#[derive(Debug, Default)]
pub struct BytecodeCodegenBackend {
    functions: SecondaryMap<TypeIndex, FunctionIndex>,
}

impl BytecodeCodegenBackend {
    fn compile_program(&mut self, t_ir: &TirTree, module: &mut Module) {
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
        t_ir_node: &TirNode,
    ) {
        match t_ir_node {
            TirNode::Int(..) | TirNode::Float(..) | TirNode::String(..) => {
                self.compile_constant(instructions, globals, t_ir_node)
            }
            TirNode::Ident {
                type_index: _,
                refers_to: _,
                str_value: Spanned(_, name),
            } => {
                instructions.push(Instruction::LoadName(name.into()));
            }
            TirNode::BinaryOperation { lhs, operator, rhs } => {
                self.compile_binary_operation(instructions, globals, lhs, operator.kind, rhs)
            }
            TirNode::Assign {
                local_index: _,
                lhs,
                value,
            } => {
                self.compile_assignment(globals, instructions, lhs, value);
            }
            TirNode::Function {
                type_index,
                name: _,
                arguments: _,
                return_type: _,
                body: function_body,
            } => self.compile_function_node(globals, function_body, *type_index),
            TirNode::Call {
                callee,
                arguments,
                return_type: _,
            } => self.compile_function_call(globals, instructions, callee, arguments),

            TirNode::Poisoned | TirNode::Uninitialized => unreachable!(),
        }
    }

    fn compile_constant(
        &mut self,
        instructions: &mut Vec<Instruction>,
        globals: &mut Globals,
        node: &TirNode,
    ) {
        let constant_index = globals.constants_pool.insert(match node {
            TirNode::Int(Spanned(_, constant)) => (*constant).into(),
            TirNode::Float(Spanned(_, constant)) => (*constant).into(),
            TirNode::String(Spanned(_, constant)) => constant.clone().into(),

            _ => unreachable!(),
        });

        instructions.push(Instruction::LoadConstant(constant_index));
    }

    fn compile_binary_operation(
        &mut self,
        instructions: &mut Vec<Instruction>,
        globals: &mut Globals,
        lhs: &TirNode,
        operator: OperatorKind,
        rhs: &TirNode,
    ) {
        self.compile_node(instructions, globals, rhs);
        self.compile_node(instructions, globals, lhs);

        instructions.push(match operator {
            OperatorKind::Add => Instruction::Add,
            OperatorKind::Sub => Instruction::Subtract,
            OperatorKind::Mul => Instruction::Multiply,
            OperatorKind::Div => Instruction::Divide,
        });
    }

    fn compile_assignment(
        &mut self,
        globals: &mut Globals,
        instructions: &mut Vec<Instruction>,
        lhs: &tir::Assignable,
        value: &TirNode,
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
        function_body: &TirNode,
        type_index: TypeIndex,
    ) {
        let mut bytecode_function_body = Vec::new();

        let unit_constant_index = globals.constants_pool.insert(ConstantValue::Unit);

        bytecode_function_body.push(Instruction::LoadConstant(unit_constant_index));

        self.compile_node(&mut bytecode_function_body, globals, function_body);

        bytecode_function_body.push(Instruction::Return);

        let function_index = globals.functions.insert(Function {
            body: bytecode_function_body,
        });

        self.functions.insert(type_index, function_index);
    }

    fn compile_function_call(
        &mut self,
        globals: &mut Globals,
        instructions: &mut Vec<Instruction>,
        callee: &tir::Callable,
        arguments: &[TirNode],
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
        t_ir_tree: &TirTree,
    ) -> Result<Self::Output, CodegenError> {
        let mut module = Module {
            manifest: Manifest {
                bytecode_version: BytecodeVersion::Experimental(),
            },
            globals: Globals::default(),
            body: Vec::new(),
        };

        self.compile_program(t_ir_tree, &mut module);

        Ok(module)
    }
}
