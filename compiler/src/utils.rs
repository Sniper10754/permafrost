mod scopes_abstraction
{

    use alloc::{collections::BTreeMap, string::String, vec::Vec};

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub struct SymbolAlreadyExists;

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Scope<V>
    {
        locals: BTreeMap<String, V>,
    }

    impl<V> Scope<V>
    {
        pub fn new() -> Self
        {
            Self {
                locals: BTreeMap::new(),
            }
        }

        pub fn insert_local(
            &mut self,
            local: impl Into<String>,
            v: V,
        )
        {
            self.locals.insert(local.into(), v);
        }

        pub fn local(
            &self,
            name: &str,
        ) -> Option<&V>
        {
            self.locals.get(name)
        }

        pub fn local_mut(
            &mut self,
            name: &str,
        ) -> Option<&mut V>
        {
            self.locals.get_mut(name)
        }
    }

    impl<V> Default for Scope<V>
    {
        fn default() -> Self
        {
            Self::new()
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Scopes<V>
    {
        scopes: Vec<Scope<V>>,
    }

    impl<V> Scopes<V>
    {
        pub fn new() -> Self
        {
            Self {
                scopes: [Scope::new()].into(),
            }
        }

        pub fn insert_forced(
            &mut self,
            local: impl Into<String>,
            v: V,
        )
        {
            self.scopes.last_mut().unwrap().insert_local(local, v)
        }

        #[must_use = "Scope result must not be ignored"]
        pub fn try_insert(
            &mut self,
            local: impl Into<String>,
            v: V,
        ) -> Result<(), SymbolAlreadyExists>
        {
            let local_name = local.into();

            if self.local(&local_name).is_some() {
                return Err(SymbolAlreadyExists);
            } else {
                self.insert_forced(local_name, v)
            }

            Ok(())
        }

        pub fn local(
            &self,
            name: impl AsRef<str>,
        ) -> Option<&V>
        {
            let name = name.as_ref();

            self.scopes.iter().rev().find_map(|scope| scope.local(name))
        }

        pub fn local_mut(
            &mut self,
            name: impl AsRef<str>,
        ) -> Option<&mut V>
        {
            let name = name.as_ref();

            self.scopes
                .iter_mut()
                .rev()
                .find_map(|scope| scope.local_mut(name))
        }

        pub fn scopes(&self) -> impl Iterator<Item = &Scope<V>>
        {
            self.scopes.iter()
        }

        pub fn scopes_mut(&mut self) -> impl Iterator<Item = &mut Scope<V>>
        {
            self.scopes.iter_mut()
        }

        pub fn len(&self) -> usize
        {
            self.scopes.len()
        }

        pub fn is_empty(&self) -> bool
        {
            self.len() == 0
        }

        pub fn truncate(
            &mut self,
            new_len: usize,
        )
        {
            self.scopes.truncate(new_len);
        }

        pub fn enter_scope(&mut self)
        {
            self.scopes.push(Scope::new())
        }

        pub fn leave_scope(&mut self)
        {
            self.scopes.pop();
        }
    }

    impl<V> Default for Scopes<V>
    {
        fn default() -> Self
        {
            Self::new()
        }
    }
}

mod compilation_results
{
    use delegate::delegate;
    use permafrost_reports::sourcemap::SourceKey;
    use slotmap::SecondaryMap;

    use crate::codegen::CodegenBackend;

    pub struct CompilationResult<C>
    where
        C: CodegenBackend,
    {
        pub(crate) compiled_files: SecondaryMap<SourceKey, C::Output>,
    }

    impl<C> CompilationResult<C>
    where
        C: CodegenBackend,
    {
        delegate! {
            to self.compiled_files {
                #[call(get)]
                pub fn get_file(&self, source_key: SourceKey) -> Option<&C::Output>;
                #[call(remove)]
                pub fn retrieve(&mut self, source_key: SourceKey) -> Option<C::Output>;
            }
        }
    }

    impl<C> FromIterator<(SourceKey, C::Output)> for CompilationResult<C>
    where
        C: CodegenBackend,
    {
        fn from_iter<T: IntoIterator<Item = (SourceKey, C::Output)>>(iter: T) -> Self
        {
            Self {
                compiled_files: iter.into_iter().collect(),
            }
        }
    }
}

pub use self::{compilation_results::*, scopes_abstraction::*};
