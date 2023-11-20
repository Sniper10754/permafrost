use alloc::{boxed::Box, vec::Vec};
use core::ops::Range;
use derive_more::*;

pub type Span = Range<usize>;

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
            Expr::Assign { lhs, value } => (lhs.span().start)..(value.span().end),
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
        value: Box<Self>,
    },
}

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
