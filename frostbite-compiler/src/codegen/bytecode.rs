use alloc::vec::Vec;
use frostbite_bytecode::{BytecodeVersion, Globals, Instruction, Manifest, Module};
use frostbite_parser::ast::Spanned;
use frostbite_reports::ReportContext;

use crate::tir::{TirNode, TirTree};

use super::{CodegenBackend, CodegenError};

#[derive(Debug, Default)]
pub struct BytecodeCodegenBackend;

impl BytecodeCodegenBackend {
    fn compile_program(
        &self,
        report_ctx: &mut ReportContext,
        t_ir: &TirTree,
        globals: &mut Module,
    ) {
        let body = &mut globals.body;
        let globals = &mut globals.globals;

        for node in &t_ir.nodes {
            self.compile_node(report_ctx, body, globals, node);
        }
    }

    fn compile_node(
        &self,
        _report_ctx: &mut ReportContext,
        instructions: &mut Vec<Instruction>,
        globals: &mut Globals,
        t_ir_node: &TirNode,
    ) {
        match t_ir_node {
            TirNode::Int(Spanned(_, value)) => {
                let idx = globals.constants_pool.insert((*value).into());

                instructions.push(Instruction::Load(idx))
            }
            TirNode::Float(Spanned(_, value)) => {
                let idx = globals.constants_pool.insert((*value).into());

                instructions.push(Instruction::Load(idx))
            }
            TirNode::String(Spanned(_, value)) => {
                let idx = globals.constants_pool.insert(value.clone().into());

                instructions.push(Instruction::Load(idx))
            }
            TirNode::Ident {
                r#type: _,
                refers_to: _t_ir_node,
                str_value: _,
            } => {}
            TirNode::BinaryOperation {
                lhs: _,
                operator: _,
                rhs: _,
            } => todo!(),
            TirNode::Assign { lhs: _, value: _ } => todo!(),
            TirNode::Function {
                name: _,
                arguments: _,
                return_type: _,
                body: _,
            } => todo!(),
            TirNode::Call {
                callee: _,
                arguments: _,
                return_type: _,
            } => todo!(),

            TirNode::Poisoned | TirNode::Uninitialized => unreachable!(),
        }
    }
}

impl CodegenBackend for BytecodeCodegenBackend {
    type Output = Module;

    fn codegen(
        self,
        report_ctx: &mut ReportContext,
        t_ir_tree: &TirTree,
    ) -> Result<Self::Output, CodegenError> {
        let mut module = Module {
            manifest: Manifest {
                bytecode_version: BytecodeVersion::Number(0.1),
            },
            globals: Globals::default(),
            body: Vec::new(),
        };

        self.compile_program(report_ctx, t_ir_tree, &mut module);

        Ok(module)
    }
}
