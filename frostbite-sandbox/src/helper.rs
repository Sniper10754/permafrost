use crate::{
    internals::Shared,
    stack::{Stack, StackFrame},
    value::Value,
};

pub fn find_symbol_from_stack_frames<'id, 'ast, 'stack_frame>(
    symbol: &'ast str,
    stack_frames: &'stack_frame mut Stack<'id, 'ast>,
) -> Option<(
    &'stack_frame mut StackFrame<'id, 'ast>,
    Shared<Value<'id, 'ast>>,
)> {
    let stack_frame = stack_frames
        .iter_mut()
        .find(|stack_frame| stack_frame.symbols.contains_key(symbol))?;

    let symbol = stack_frame.symbols[symbol].clone();

    Some((stack_frame, symbol))
}
