use alloc::collections::BTreeMap;

use crate::ExternalFunction;

#[derive(Clone)]
pub struct IntrinsicContext<'id, 'ast> {
    pub intrinsic_functions: BTreeMap<&'static str, ExternalFunction<'id, 'ast>>,
}

impl<'id, 'ast> IntrinsicContext<'id, 'ast> {
    pub fn new() -> Self {
        Self::with(BTreeMap::default())
    }

    pub fn with(intrinsic_functions: BTreeMap<&'static str, ExternalFunction<'id, 'ast>>) -> Self {
        Self {
            intrinsic_functions,
        }
    }
}

impl<'id, 'ast> Default for IntrinsicContext<'id, 'ast> {
    fn default() -> Self {
        Self::new()
    }
}
