use frostbite_parser::ast::tokens::OperatorKind;
use num_traits::Num;

pub struct OverflowError;

pub fn do_binary_operation<N: Num>(
    lhs: N,
    rhs: N,
    operation_kind: OperatorKind,
) -> Result<N, OverflowError> {
    let checked_number = match operation_kind {
        OperatorKind::Add => lhs.add(rhs),
        OperatorKind::Sub => lhs.sub(rhs),
        OperatorKind::Mul => lhs.mul(rhs),
        OperatorKind::Div => lhs.div(rhs),
    };

    checked_number.ok_or(OverflowError)
}
