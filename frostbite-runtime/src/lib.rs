#![no_std]

extern crate std;

extern crate alloc;

use alloc::{boxed::Box, vec, vec::Vec};

use frostbite_reports::sourcemap::SourceId;
use internals::Shared;
use intrinsic::IntrinsicContext;
use stack::StackFrame;
use thread::{RuntimeThread, ThreadBackend, ThreadJoinHandle};
use value::Value;

/// Touch only in case you are working with the runtime internals
pub mod internals;

pub mod error;
pub mod value;

mod eval;
mod helper;
mod intrinsic;
mod math;
mod stack;
mod thread;

pub type ExternalFunction<'id, 'ast> = dyn Fn(&[Shared<Value<'id, 'ast>>]) -> Value<'id, 'ast>;

pub struct Runtime<'itself, 'id, 'ast> {
    threads: Vec<RuntimeThread<'itself, 'id, 'ast>>,
    intrinsic_ctx: IntrinsicContext<'id, 'ast>,
    thread_backend: Box<dyn ThreadBackend<BackendJoinHandle = Box<dyn ThreadJoinHandle>>>,
}

impl<'itself, 'id, 'ast> Default for Runtime<'itself, 'id, 'ast> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'itself, 'id, 'ast> Runtime<'itself, 'id, 'ast> {
    pub fn new() -> Self {
        Self {
            threads: vec![],
            intrinsic_ctx: IntrinsicContext::new(),
            thread_backend: Box::new(thread::DefaultThreadBackend::new()),
        }
    }

    pub fn run(&self, source_id: SourceId<'id>) -> Box<dyn ThreadJoinHandle> {
        let runtime_thread = RuntimeThread::new(&self, source_id, &self.intrinsic_ctx);

        self.thread_backend.spawn_thread(runtime_thread)
    }

    pub fn intrinsic_ctx<'ctx>(&'ctx mut self) -> &'ctx mut IntrinsicContext<'id, 'ast> {
        &mut self.intrinsic_ctx
    }
}
