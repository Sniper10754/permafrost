use alloc::{collections::BTreeMap, vec::Vec};
use frostbite_bytecode::{BytecodeVersion, Manifest, Module};
use frostbite_reports::ReportContext;

use crate::hir::{HirNode, HirTree};

use super::CodegenBackend;

#[derive(Debug, Default)]
pub struct BytecodeCodegenBackend;

impl BytecodeCodegenBackend {
    fn compile_program(
        &self,
        report_ctx: &mut ReportContext,
        hir: &mut HirTree,
        module: &mut Module,
    ) {
        for node in &hir.nodes {
            self.compile_node(report_ctx, node, module);
        }
    }

    fn compile_node(
        &self,
        report_ctx: &mut ReportContext,
        hir_node: &HirNode,
        module: &mut Module,
    ) {
        match hir_node {
            HirNode::Int(_) => {
                module.constants_pool.insert(key, value);
            }
            HirNode::Float(_) => todo!(),
            HirNode::Ident(_, _) => todo!(),
            HirNode::String(_) => todo!(),
            HirNode::BinaryOperation { lhs, operator, rhs } => todo!(),
            HirNode::Assign { lhs, value } => todo!(),
            HirNode::Function {
                name,
                arguments,
                return_type,
                body,
            } => todo!(),
            HirNode::Call {
                callee,
                arguments,
                return_type,
            } => todo!(),

            HirNode::Poisoned | HirNode::Uninitialized => unreachable!(),
        }
    }
}

impl CodegenBackend for BytecodeCodegenBackend {
    type Output = Module;

    fn codegen(
        self,
        _report_ctx: &mut ReportContext,
        _program: &HirTree,
    ) -> Result<Self::Output, ()> {
        let module = Module {
            manifest: Manifest {
                bytecode_version: BytecodeVersion::Number(0.1),
            },
            constants_pool: BTreeMap::new(),
            functions: Vec::new(),
            instructions: Vec::new(),
        };

        Ok(module)
    }
}
