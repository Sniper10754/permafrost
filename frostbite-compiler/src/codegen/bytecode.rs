use alloc::vec::Vec;
use frostbite_bytecode::{
    BytecodeVersion, Function, FunctionIndex, Globals, Instruction, Manifest, Module,
};
use frostbite_parser::ast::Spanned;
use frostbite_reports::ReportContext;
use slotmap::SecondaryMap;

use crate::tir::{self, TirNode, TirTree, TypeIndex};

use super::{CodegenBackend, CodegenError};

#[derive(Debug, Default)]
pub struct BytecodeCodegenBackend {
    functions: SecondaryMap<TypeIndex, FunctionIndex>,
}

impl BytecodeCodegenBackend {
    fn compile_program(
        &mut self,
        report_ctx: &mut ReportContext,
        t_ir: &TirTree,
        module: &mut Module,
    ) {
        let body = &mut module.body;
        let globals = &mut module.globals;

        for node in &t_ir.nodes {
            self.compile_node(report_ctx, t_ir, body, globals, node);
        }
    }

    fn compile_node(
        &mut self,
        _report_ctx: &mut ReportContext,
        _t_ir: &TirTree,
        instructions: &mut Vec<Instruction>,
        globals: &mut Globals,
        t_ir_node: &TirNode,
    ) {
        match t_ir_node {
            TirNode::Int(Spanned(_, value)) => {
                let idx = globals.constants_pool.insert((*value).into());

                instructions.push(Instruction::LoadConstant(idx))
            }
            TirNode::Float(Spanned(_, value)) => {
                let idx = globals.constants_pool.insert((*value).into());

                instructions.push(Instruction::LoadConstant(idx))
            }
            TirNode::String(Spanned(_, value)) => {
                let idx = globals.constants_pool.insert(value.clone().into());

                instructions.push(Instruction::LoadConstant(idx))
            }
            TirNode::Ident {
                type_index: _,
                refers_to: _,
                str_value: Spanned(_, name),
            } => {
                instructions.push(Instruction::LoadName(name.into()));
            }
            TirNode::BinaryOperation { lhs, operator, rhs } => {
                
            }
            TirNode::Assign {
                local_index: _,
                lhs,
                value,
            } => {
                self.compile_node(_report_ctx, _t_ir, instructions, globals, value);

                match lhs {
                    tir::Assignable::Ident(_, Spanned(_, name)) => {
                        instructions.push(Instruction::StoreName(name.into()));
                    }
                }
            }
            TirNode::Function {
                type_index,
                name: _,
                arguments: _,
                return_type: _,
                body: function_body,
            } => {
                let mut bytecode_function_body = Vec::new();

                self.compile_node(
                    _report_ctx,
                    _t_ir,
                    &mut bytecode_function_body,
                    globals,
                    function_body,
                );

                let function_index = globals.functions.insert(Function {
                    body: bytecode_function_body,
                });

                self.functions.insert(*type_index, function_index);
            }
            TirNode::Call {
                callee,
                arguments,
                return_type: _,
            } => {
                // Push arguments onto the stack

                arguments.iter().for_each(|argument| {
                    self.compile_node(_report_ctx, _t_ir, instructions, globals, argument);
                });

                match callee {
                    tir::Callable::Ident(type_index, ..) => {
                        let function_index = self.functions[*type_index];

                        instructions.push(Instruction::Call(function_index));
                    }
                }
            }

            TirNode::Poisoned | TirNode::Uninitialized => unreachable!(),
        }
    }
}

impl CodegenBackend for BytecodeCodegenBackend {
    type Output = Module;

    fn codegen(
        mut self,
        report_ctx: &mut ReportContext,
        t_ir_tree: &TirTree,
    ) -> Result<Self::Output, CodegenError> {
        let mut module = Module {
            manifest: Manifest {
                bytecode_version: BytecodeVersion::Experimental(),
            },
            globals: Globals::default(),
            body: Vec::new(),
        };

        self.compile_program(report_ctx, t_ir_tree, &mut module);

        Ok(module)
    }
}
