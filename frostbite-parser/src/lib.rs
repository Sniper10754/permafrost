#![no_std]

extern crate alloc;

pub mod ast;
pub mod error;

pub use frostbite_report_interface as report;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub parser);

#[cfg(test)]
mod tests {
    use crate::ast::{BinaryOperator, Expr};

    extern crate std;

    macro_rules! parse_expr {
        ($text_to_parse:expr) => {
            super::parser::ExprParser::new().parse($text_to_parse)
        };
    }

    macro_rules! boxed {
        ($value:expr) => {
            std::boxed::Box::new($value)
        };
    }

    #[test]
    fn test() {
        assert_eq!(
            parse_expr!("1 + 2 + 3"),
            Ok(Expr::BinaryOperation {
                lhs: boxed!(Expr::Int(0..1, 1)),
                operator: BinaryOperator::Add,
                rhs: boxed!(Expr::BinaryOperation {
                    lhs: boxed!(Expr::Int(4..5, 2)),
                    operator: BinaryOperator::Add,
                    rhs: boxed!(Expr::Int(8..9, 3))
                }),
            })
        );
    }
}
