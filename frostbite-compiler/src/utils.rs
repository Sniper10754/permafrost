use alloc::{collections::BTreeMap, string::String, vec::Vec};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Scope<V> {
    locals: BTreeMap<String, V>,
}

impl<V> Scope<V> {
    pub fn new() -> Self {
        Self {
            locals: BTreeMap::new(),
        }
    }

    pub fn insert_local(&mut self, local: impl Into<String>, v: V) {
        self.locals.insert(local.into(), v);
    }

    pub fn local(&self, name: &str) -> Option<&V> {
        self.locals.get(name)
    }

    pub fn local_mut(&mut self, name: &str) -> Option<&mut V> {
        self.locals.get_mut(name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Scopes<V> {
    scopes: Vec<Scope<V>>,
}

impl<V> Scopes<V> {
    pub fn new() -> Self {
        Self {
            scopes: [Scope::new()].into(),
        }
    }

    pub fn insert_local(&mut self, local: impl Into<String>, v: V) {
        self.scopes.last_mut().unwrap().insert_local(local, v)
    }

    pub fn local(&self, name: &str) -> Option<&V> {
        self.scopes.iter().rev().find_map(|scope| scope.local(name))
    }

    pub fn local_mut(&mut self, name: &str) -> Option<&mut V> {
        self.scopes
            .iter_mut()
            .rev()
            .find_map(|scope| scope.local_mut(name))
    }

    pub fn len(&self) -> usize {
        self.scopes.len()
    }

    pub fn truncate(&mut self, new_len: usize) {
        self.scopes.truncate(new_len);
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(Scope::new())
    }

    pub fn leave_scope(&mut self) {
        self.scopes.pop();
    }
}
