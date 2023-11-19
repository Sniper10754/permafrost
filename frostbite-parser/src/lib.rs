#![no_std]

extern crate alloc;

pub mod ast;

pub use frostbite_report_interface as report;

mod __parser {
    use lalrpop_util::lalrpop_mod;

    lalrpop_mod!(parser);

    pub use parser::*;
}

pub mod parser {
    pub use super::__parser::ExprParser;
}

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
            parse_expr!("1 + 2 + 3").unwrap(),
            Expr::BinaryOperation {
                lhs: boxed!(Expr::Int(0..1, 1)),
                operator: BinaryOperator::Add,
                rhs: boxed!(Expr::BinaryOperation {
                    lhs: boxed!(Expr::Int(4..5, 2)),
                    operator: BinaryOperator::Add,
                    rhs: boxed!(Expr::Int(8..9, 3))
                }),
            }
        );
    }
}
