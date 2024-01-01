use alloc::{boxed::Box, vec::Vec};
use core::ops::Range;

use derive_more::From;

use self::tokens::{
    ArrowToken, Eq, FunctionToken, LeftBraceToken, LeftParenthesisToken, Operator, ReturnToken,
    RightBraceToken, RightParenthesisToken, TypeAnnotation,
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
    #[display(fmt = "{kind}")]
    pub struct Operator {
        pub span: Span,
        pub kind: BinaryOperatorKind,
    }

    impl Spannable for Operator {
        fn span(&self) -> Span {
            self.span.clone()
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Display)]
    pub enum BinaryOperatorKind {
        #[display(fmt = "+")]
        Add,
        #[display(fmt = "-")]
        Sub,
        #[display(fmt = "*")]
        Mul,
        #[display(fmt = "/")]
        Div,
        #[display(fmt = "==")]
        Equal,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Display)]
    pub enum TypeAnnotation<'a> {
        #[display(fmt = "int")]
        Int,
        #[display(fmt = "float")]
        Float,
        #[display(fmt = "str")]
        String,
        #[display(fmt = "bool")]
        Bool,

        #[display(fmt = "any")]
        Any,

        #[display(fmt = "()")]
        Unit,

        #[display(fmt = "object {_0}")]
        Object(&'a str),
    }

    token!(Eq);
    token!(FunctionToken);
    token!(LeftParenthesisToken);
    token!(RightParenthesisToken);
    token!(LeftBraceToken);
    token!(RightBraceToken);
    token!(ArrowToken);
    token!(ReturnToken);
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

    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned(self.0.clone(), &self.1)
    }

    pub fn as_mut(&mut self) -> Spanned<&mut T> {
        Spanned(self.0.clone(), &mut self.1)
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

impl<T> From<(&Span, T)> for Spanned<T> {
    fn from((span, value): (&Span, T)) -> Self {
        Self(span.clone(), value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program<'a> {
    pub exprs: Vec<Expr<'a>>,
}

impl<'a> Spannable for Expr<'a> {
    fn span(&self) -> Span {
        match self {
            Expr::Int(Spanned(span, _))
            | Expr::Float(Spanned(span, _))
            | Expr::Ident(Spanned(span, _))
            | Expr::String(Spanned(span, _)) => span.clone(),

            Expr::Bool(Spanned(span, _)) => span.clone(),

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
            Expr::Call {
                callee: _,
                left_paren,
                arguments: _,
                right_paren,
            } => (left_paren.span().start)..(right_paren.span().end),

            Expr::Block {
                left_brace,
                expressions: _,
                right_brace,
            } => (left_brace.span().start)..(right_brace.span().end),

            Expr::Return(ret_token, expr) => {
                (ret_token.span().start)
                    ..expr
                        .as_ref()
                        .map(|expr| expr.span().end)
                        .unwrap_or(ret_token.span().end)
            }

            Expr::Poisoned => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Int(Spanned<i32>),
    Float(Spanned<f32>),
    Ident(Spanned<&'a str>),
    Bool(Spanned<bool>),
    String(Spanned<&'a str>),

    BinaryOperation {
        lhs: Box<Self>,
        operator: Operator,
        rhs: Box<Self>,
    },

    Assign {
        lhs: Box<Self>,
        eq_token: tokens::Eq,
        value: Box<Self>,
    },

    Function {
        fn_token: FunctionToken,
        name: Option<Spanned<&'a str>>,
        lpt: LeftParenthesisToken,
        arguments: Vec<Argument<'a>>,
        rpt: RightParenthesisToken,
        return_type_token: Option<ArrowToken>,
        return_type_annotation: Option<Spanned<TypeAnnotation<'a>>>,
        equals: Eq,
        body: Box<Self>,
    },

    Call {
        callee: Box<Self>,
        left_paren: LeftParenthesisToken,
        arguments: Vec<Expr<'a>>,
        right_paren: RightParenthesisToken,
    },

    Block {
        left_brace: LeftBraceToken,
        expressions: Vec<Self>,
        right_brace: RightBraceToken,
    },

    Return(ReturnToken, Option<Box<Self>>),

    Poisoned,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument<'a> {
    pub name: Spanned<&'a str>,
    pub type_annotation: Spanned<TypeAnnotation<'a>>,
}
