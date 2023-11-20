use alloc::boxed::Box;
use core::ops::Range;

pub type Span = Range<usize>;

#[derive(Debug, PartialEq)]
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

    Let {
        name: &'a str,
        type_annotation: Option<TypeAnnotation>,
        value: Option<Box<Self>>,
    },
}

#[derive(Debug, PartialEq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq)]
pub enum TypeAnnotation {
    Int,
    Float,
    String,
}
