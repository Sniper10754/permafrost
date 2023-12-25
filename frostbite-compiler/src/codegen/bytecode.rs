use alloc::{collections::BTreeMap, vec::Vec};
use frostbite_bytecode::{BytecodeVersion, Manifest, Module, Pool};
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
        module: &mut Module,
    ) {
        for node in &t_ir.nodes {
            self.compile_node(report_ctx, node, module);
        }
    }

    fn compile_node(
        &self,
        report_ctx: &mut ReportContext,
        t_ir_node: &TirNode,
        module: &mut Module,
    ) {
        match t_ir_node {
            TirNode::Int(_) => {
                // module.constants_pool.push();
            }
            TirNode::Float(_) => todo!(),
            TirNode::Ident(_, _) => todo!(),
            TirNode::String(_) => todo!(),
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
            constants_pool: Pool::new(),
            functions: Vec::new(),
            body: Vec::new(),
        };

        Ok(module)
    }
}
