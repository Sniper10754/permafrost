#![no_std]

extern crate std;

extern crate alloc;

use alloc::rc::Rc;

use error::InterpretationError;
use eval::Sandbox;
use frostbite_parser::ast::{Expr, Program};
use frostbite_reports::sourcemap::SourceId;
use internals::Shared;
use intrinsic::IntrinsicContext;
use stack::StackFrame;
use value::Value;

/// Touch only in case you are working with the runtime internals
pub mod internals;

/// Touch only in case you are working with the runtime internals
pub mod intrinsic;

pub mod error;
pub mod value;

mod eval;
mod helper;
mod math;
mod stack;

pub type ExternalFunction<'id, 'ast> = Rc<dyn Fn(&[Shared<Value<'id, 'ast>>]) -> Value<'id, 'ast>>;

pub struct Runtime<'id, 'ast> {
    sandbox: Sandbox<'id, 'ast>,
}

impl<'id, 'ast> Default for Runtime<'id, 'ast> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'id, 'ast> Runtime<'id, 'ast> {
    pub fn new() -> Self {
        let intrinsic_ctx = Rc::new(IntrinsicContext::new());

        Self {
            sandbox: Sandbox::new(SourceId::Undefined, intrinsic_ctx),
        }
    }

    pub fn with_intrinsic_ctx(intrinsic_ctx: IntrinsicContext<'id, 'ast>) -> Self {
        let intrinsic_ctx = Rc::new(intrinsic_ctx);

        Self {
            sandbox: Sandbox::new(SourceId::Undefined, intrinsic_ctx),
        }
    }

    pub fn eval_ast_expr(
        &mut self,
        ast_node: &Expr<'ast>,
    ) -> Result<Shared<Value<'id, 'ast>>, InterpretationError<'ast>> {
        self.sandbox.eval(ast_node)
    }

    pub fn eval_ast_tree(
        &mut self,
        ast_tree: &Program<'ast>,
    ) -> Result<(), InterpretationError<'ast>> {
        for node in ast_tree.exprs.iter() {
            self.eval_ast_expr(node)?;
        }

        Ok(())
    }

    pub fn sandbox(&self) -> &Sandbox<'id, 'ast> {
        &self.sandbox
    }

    pub fn sandbox_mut(&mut self) -> &mut Sandbox<'id, 'ast> {
        &mut self.sandbox
    }
}
