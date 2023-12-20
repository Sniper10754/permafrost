use alloc::rc::Rc;

pub type Shared<T> = Rc<T>;

pub mod thread_backend {}
