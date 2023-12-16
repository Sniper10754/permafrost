use crate::{value::Value, Shared, StackFrame};

pub fn find_symbol_from_stack_frames<'ast, 'stack_frame, I>(
    symbol: &'ast str,
    stack_frames: I,
) -> Option<(&'stack_frame mut StackFrame<'ast>, Shared<Value<'ast>>)>
where
    I: IntoIterator<Item = &'stack_frame mut StackFrame<'ast>>,
{
    let stack_frame = stack_frames
        .into_iter()
        .find(|stack_frame| stack_frame.symbols.contains_key(symbol))?;

    let symbol = stack_frame.symbols[symbol].clone();

    Some((stack_frame, symbol))
}
