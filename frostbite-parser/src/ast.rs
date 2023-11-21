use alloc::{boxed::Box, vec::Vec};
use core::ops::Range;

use self::tokens::BinaryOperator;

pub type Span = Range<usize>;

pub mod tokens {
    use derive_more::*;

    use super::{Span, Spannable};

    #[derive(Debug, Clone, Copy, PartialEq, Display)]
    pub enum BinaryOperator {
        #[display(fmt = "+")]
        Add,
        #[display(fmt = "-")]
        Sub,
        #[display(fmt = "*")]
        Mul,
        #[display(fmt = "/")]
        Div,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Display)]
    pub enum TypeAnnotation {
        #[display(fmt = "int")]
        Int,
        #[display(fmt = "float")]
        Float,
        #[display(fmt = "str")]
        String,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Eq(pub Span);

    impl Spannable for Eq {
        fn span(&self) -> Span {
            self.0.clone()
        }
    }
}

pub trait Spannable {
    fn span(&self) -> Span;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program<'a> {
    pub exprs: Vec<Expr<'a>>,
}

impl<'a> Spannable for Expr<'a> {
    fn span(&self) -> Span {
        match self {
            Expr::Int(span, _) => span.clone(),
            Expr::Float(span, _) => span.clone(),
            Expr::Ident(span, _) => span.clone(),
            Expr::String(span, _) => span.clone(),
            Expr::BinaryOperation {
                lhs,
                operator: _,
                rhs,
            } => (lhs.span().start)..(rhs.span().end),
            Expr::Assign {
                lhs,
                eq_token: _,
                value,
            } => (lhs.span().start)..(value.span().end),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Int(Span, i32),
    Float(Span, f32),
    Ident(Span, &'a str),
    String(Span, &'a str),

    BinaryOperation {
        lhs: Box<Self>,
        operator: BinaryOperator,
        rhs: Box<Self>,
    },

    Assign {
        lhs: Box<Expr<'a>>,
        eq_token: tokens::Eq,
        value: Box<Self>,
    },
}
