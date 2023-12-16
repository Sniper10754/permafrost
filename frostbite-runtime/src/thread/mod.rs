use core::convert::Infallible;

use alloc::boxed::Box;
use frostbite_parser::ast::{Expr, Program};
use frostbite_reports::sourcemap::SourceId;

use crate::{
    error::InterpretationError, eval::Sandbox, internals::Shared, intrinsic::IntrinsicContext,
    value::Value, Runtime,
};

mod dumb_backend;

pub type DefaultThreadBackend = dumb_backend::DumbBackend;

pub trait ThreadBackend {
    type BackendJoinHandle: ThreadJoinHandle;

    fn spawn_thread(&self, thread: RuntimeThread<'_, '_, '_>) -> Self::BackendJoinHandle;
}

impl<T: ThreadBackend + ?Sized> ThreadBackend for Box<T> {
    type BackendJoinHandle = T::BackendJoinHandle;

    fn spawn_thread(&self, thread: RuntimeThread<'_, '_, '_>) -> Self::BackendJoinHandle {
        (**self).spawn_thread(thread)
    }
}

impl<T: ThreadJoinHandle + ?Sized> ThreadJoinHandle for Box<T> {
    fn join<'id, 'ast>(&self) -> Value<'id, 'ast> {
        (**self).join()
    }

    fn try_join<'id, 'ast>(&self) -> Option<Value<'id, 'ast>> {
        (**self).try_join()
    }
}

pub trait ThreadJoinHandle {
    fn join<'id, 'ast>(&self) -> Value<'id, 'ast>;
    fn try_join<'id, 'ast>(&self) -> Option<Value<'id, 'ast>>;
}

pub struct RuntimeThread<'runtime, 'id, 'ast> {
    sandbox: Sandbox<'runtime, 'id, 'ast>,
    runtime: &'runtime Runtime<'runtime, 'id, 'ast>,
}

impl<'runtime, 'id, 'ast> RuntimeThread<'runtime, 'id, 'ast> {
    pub fn new(
        runtime: &'runtime Runtime<'runtime, 'id, 'ast>,
        source_id: SourceId<'id>,
        intrinsic_ctx: &'runtime IntrinsicContext<'id, 'ast>,
    ) -> Self {
        Self {
            sandbox: Sandbox::new(source_id, intrinsic_ctx),
            runtime,
        }
    }

    pub fn eval_program(
        &mut self,
        program: &Program<'ast>,
    ) -> Result<(), InterpretationError<'ast>> {
        for expr in program.exprs.iter() {
            self.eval(expr)?;
        }

        Ok(())
    }

    pub fn eval(
        &mut self,
        expr: &Expr<'ast>,
    ) -> Result<Shared<Value<'id, 'ast>>, InterpretationError<'ast>> {
        self.sandbox.eval(expr)
    }
}
