use alloc::{boxed::Box, collections::BTreeMap};

use crate::ExternalFunction;

pub struct IntrinsicContext<'id, 'ast> {
    pub intrinsic_functions: BTreeMap<&'static str, Box<ExternalFunction<'id, 'ast>>>,
}

impl<'id, 'ast> IntrinsicContext<'id, 'ast> {
    pub fn new() -> Self {
        Self::with(BTreeMap::default())
    }

    pub fn with(
        intrinsic_functions: BTreeMap<&'static str, Box<ExternalFunction<'id, 'ast>>>,
    ) -> Self {
        Self {
            intrinsic_functions,
        }
    }
}
