use alloc::vec::Vec;
use frostbite_bytecode::{
    BytecodeVersion, Function, FunctionIndex, Globals, Instruction, Manifest, Module,
};
use frostbite_parser::ast::{tokens::OperatorKind, Spanned};
use frostbite_reports::ReportContext;
use slotmap::SecondaryMap;

use crate::tir::{self, Assignable, TirNode, TirTree, TypeIndex};

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
        globals: &mut Module,
    ) {
        let body = &mut globals.body;
        let globals = &mut globals.globals;

        extern crate std;

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
                refers_to,
                str_value: Spanned(_, name),
            } => match refers_to {
                tir::RefersTo::Local(_) => instructions.push(Instruction::LoadName(name.into())),
                tir::RefersTo::Type(_) => (),
            },
            TirNode::BinaryOperation { lhs, operator, rhs } => {
                self.compile_node(_report_ctx, _t_ir, instructions, globals, lhs);
                self.compile_node(_report_ctx, _t_ir, instructions, globals, rhs);

                instructions.push(match operator.1 {
                    OperatorKind::Add => Instruction::Add,
                    OperatorKind::Sub => Instruction::Subtract,
                    OperatorKind::Mul => Instruction::Multiply,
                    OperatorKind::Div => Instruction::Divide,
                });
            }
            TirNode::Assign {
                lhs,
                value,
                local_index: _,
            } => {
                self.compile_node(_report_ctx, _t_ir, instructions, globals, value);

                match lhs {
                    Assignable::Ident(_, Spanned(_, name)) => {
                        instructions.push(Instruction::StoreName(name.into()))
                    }
                }
            }
            TirNode::Function {
                type_index,
                name: _,
                arguments: _,
                return_type: _,
                body,
            } => {
                let mut body_instructions = Vec::new();

                self.compile_node(_report_ctx, _t_ir, &mut body_instructions, globals, body);

                body_instructions.push(Instruction::Return);

                let fn_index = globals.functions.insert(Function {
                    body: body_instructions,
                });

                self.functions.insert(*type_index, fn_index);
            }
            TirNode::Call {
                callee,
                arguments,
                return_type: _,
            } => {
                arguments.iter().for_each(|arg| {
                    self.compile_node(_report_ctx, _t_ir, instructions, globals, arg);
                });

                match callee {
                    tir::Callable::Ident(type_index, _) => {
                        let fn_index = self.functions[*type_index];

                        instructions.push(Instruction::Call(fn_index));
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
