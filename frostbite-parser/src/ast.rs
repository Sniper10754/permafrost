use alloc::{boxed::Box, vec::Vec};
use core::ops::Range;

use derive_more::From;

use self::tokens::{
    Arrow, Eq, FunctionToken, LeftParenthesisToken, Operator, RightParenthesisToken, TypeAnnotation,
};

pub type Span = Range<usize>;

pub mod tokens {
    use derive_more::Display;

    use super::{Span, Spannable};

    macro_rules! token {
        ($name:ident) => {
            #[derive(Debug, Clone, PartialEq)]
            pub struct $name(pub crate::ast::Span);

            impl Spannable for $name {
                fn span(&self) -> crate::ast::Span {
                    self.0.clone()
                }
            }

            impl From<Span> for $name {
                fn from(span: Span) -> Self {
                    Self(span)
                }
            }
        };
    }

    #[derive(Debug, Clone, PartialEq, Display)]
    #[display(fmt = "{_1}")]
    pub struct Operator(pub Span, pub OperatorKind);

    impl Spannable for Operator {
        fn span(&self) -> Span {
            self.0.clone()
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Display)]
    pub enum OperatorKind {
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
    pub enum TypeAnnotation<'a> {
        #[display(fmt = "int")]
        Int,
        #[display(fmt = "float")]
        Float,
        #[display(fmt = "str")]
        String,

        #[display(fmt = "not specified")]
        NotSpecified,

        Other(&'a str),
    }

    token!(Eq);
    token!(FunctionToken);
    token!(LeftParenthesisToken);
    token!(RightParenthesisToken);
    token!(Arrow);
}

pub trait Spannable {
    fn span(&self) -> Span;
}

#[derive(Debug, Clone, PartialEq, From)]
pub struct Spanned<T>(pub Span, pub T);

impl<T> Spanned<T> {
    pub fn new(span: Span, t: T) -> Self {
        Self(span, t)
    }

    pub fn map<U, F>(self, f: F) -> Spanned<U>
    where
        F: FnOnce(T) -> U,
    {
        Spanned(self.0, f(self.1))
    }
}

impl<T> Spannable for Spanned<T> {
    fn span(&self) -> Span {
        self.0.clone()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program<'a> {
    pub exprs: Vec<Expr<'a>>,
}

impl<'a> Spannable for Expr<'a> {
    fn span(&self) -> Span {
        match self {
            Expr::Int(span, _)
            | Expr::Float(span, _)
            | Expr::Ident(span, _)
            | Expr::String(span, _) => span.clone(),
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

            Expr::Function { fn_token, body, .. } => (fn_token.span().start)..(body.span().end),
            Expr::Poisoned => Span::default(),
            Expr::Call {
                callee,
                arguments: _,
                lpt: _,
                rpt,
            } => (callee.span().start)..(rpt.span().end),
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
        operator: Operator,
        rhs: Box<Self>,
    },

    Assign {
        lhs: Box<Expr<'a>>,
        eq_token: tokens::Eq,
        value: Box<Self>,
    },

    Function {
        fn_token: FunctionToken,
        name: Option<Spanned<&'a str>>,
        lpt: LeftParenthesisToken,
        arguments: Vec<Argument<'a>>,
        rpt: RightParenthesisToken,
        return_type_token: Option<Arrow>,
        return_type_annotation: Option<Spanned<TypeAnnotation<'a>>>,
        equals: Eq,
        body: Box<Expr<'a>>,
    },

    Call {
        callee: Box<Self>,
        lpt: LeftParenthesisToken,
        arguments: Vec<Expr<'a>>,
        rpt: RightParenthesisToken,
    },

    Poisoned,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument<'a> {
    pub name: Spanned<&'a str>,
    pub type_annotation: Spanned<TypeAnnotation<'a>>,
}
