use super::{ThreadBackend, ThreadJoinHandle};

pub struct DumbBackend;

impl DumbBackend {
    pub fn new() -> Self {
        Self
    }
}

impl ThreadBackend for DumbBackend {
    type BackendJoinHandle = DumbJoinHandle;

    fn spawn_thread(&self, thread: super::RuntimeThread<'_, '_, '_>) -> Self::BackendJoinHandle {
        
    }
}

pub struct DumbJoinHandle {}

impl ThreadJoinHandle for DumbJoinHandle {
    fn join<'id, 'ast>(&self) -> crate::value::Value<'id, 'ast> {
        todo!()
    }

    fn try_join<'id, 'ast>(&self) -> Option<crate::value::Value<'id, 'ast>> {
        todo!()
    }
}
