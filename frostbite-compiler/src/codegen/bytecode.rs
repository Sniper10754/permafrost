use alloc::{collections::BTreeMap, vec::Vec};
use frostbite_bytecode::{BytecodeVersion, Globals, Instruction, Manifest, Module, Pool};
use frostbite_parser::ast::Spanned;
use frostbite_reports::ReportContext;

use crate::tir::{TirNode, TirTree};

use super::CodegenBackend;

#[derive(Debug, Default)]
pub struct BytecodeCodegenBackend;

impl BytecodeCodegenBackend {
    fn compile_program(
        &self,
        report_ctx: &mut ReportContext,
        t_ir: &mut TirTree,
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
        let mut instruction = None;

        match t_ir_node {
            TirNode::Int(Spanned(_, value)) => {
                let idx = globals.constants_pool.insert((*value).into());

                instruction = Some(Instruction::Load(idx))
            }
            TirNode::Float(Spanned(_, value)) => {
                let idx = globals.constants_pool.insert((*value).into());

                instruction = Some(Instruction::Load(idx))
            }
            TirNode::String(Spanned(_, value)) => {
                let idx = globals.constants_pool.insert(value.clone().into());

                instruction = Some(Instruction::Load(idx))
            }
            TirNode::Ident {
                r#type: a,
                str_value: b,
            } => {}
            TirNode::BinaryOperation { lhs, operator, rhs } => todo!(),
            TirNode::Assign { lhs, value } => todo!(),
            TirNode::Function {
                name,
                arguments,
                return_type,
                body,
            } => todo!(),
            TirNode::Call {
                callee,
                arguments,
                return_type,
            } => todo!(),

            TirNode::Poisoned | TirNode::Uninitialized => unreachable!(),
        }

        if let Some(i) = instruction {
            instructions.push(i)
        }
    }
}

impl CodegenBackend for BytecodeCodegenBackend {
    type Output = Module;

    fn codegen(
        self,
        _report_ctx: &mut ReportContext,
        _program: &TirTree,
    ) -> Result<Self::Output, ()> {
        let module = Module {
            manifest: Manifest {
                bytecode_version: BytecodeVersion::Number(0.1),
            },
            globals: Globals::default(),
            body: Vec::new(),
        };

        Ok(module)
    }
}
