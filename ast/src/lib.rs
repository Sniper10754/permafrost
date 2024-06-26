#![no_std]

extern crate alloc;

use alloc::{boxed::Box, string::String, vec::Vec};
use core::ops::{Deref, DerefMut, Range};
use enum_dispatch::enum_dispatch;
use tokens::PublicToken;

use derive_more::{Deref, DerefMut, From};

use self::tokens::{
    ArrowToken, Eq, FunctionToken, LeftBraceToken, LeftParenthesisToken, ModToken, Operator,
    ReturnToken, RightBraceToken, RightParenthesisToken, TypeAnnotation,
};

pub type NamespacePathSegment = Spanned<String>;
pub type NamespacePath = [NamespacePathSegment];
pub type OwnedNamespacePath = Box<NamespacePath>;

pub type Span = Range<usize>;

pub mod tokens
{
    use derive_more::Display;

    use super::{Span, Spannable};

    use alloc::string::String;

    macro_rules! token {
        ($name:ident) => {
            #[derive(Debug, Clone, PartialEq, Eq, Hash)]
            pub struct $name(pub $crate::Span);

            impl Spannable for $name
            {
                fn span(&self) -> $crate::Span
                {
                    self.0.clone()
                }
            }

            impl From<Span> for $name
            {
                fn from(span: $crate::Span) -> Self
                {
                    Self(span)
                }
            }
        };
    }

    macro_rules! tokens {
        ($($name:ident),+) => {
            $(
                token!($name);
            )+
        };
    }

    #[derive(Debug, Clone, Hash, PartialEq, Display)]
    #[display(fmt = "{kind}")]
    pub struct Operator
    {
        pub span: Span,
        pub kind: BinaryOperatorKind,
    }

    impl Spannable for Operator
    {
        fn span(&self) -> Span
        {
            self.span.clone()
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Hash, Display)]
    pub enum BinaryOperatorKind
    {
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

    #[derive(Debug, Clone, PartialEq, Display)]
    pub enum TypeAnnotation
    {
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
        Object(String),
    }

    tokens![
        Eq,
        FunctionToken,
        LeftParenthesisToken,
        RightParenthesisToken,
        LeftBraceToken,
        RightBraceToken,
        ArrowToken,
        ReturnToken,
        ModToken,
        PublicToken
    ];
}

#[enum_dispatch]
pub trait Spannable
{
    fn span(&self) -> Span;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, From, Deref, DerefMut)]
pub struct Spanned<T>(
    pub Span,
    #[deref]
    #[deref_mut]
    pub T,
);

impl<T> Spanned<T>
{
    pub fn new(
        span: Span,
        t: T,
    ) -> Self
    {
        Self(span, t)
    }

    pub fn value(&self) -> &T
    {
        &self.1
    }

    pub fn value_mut(&mut self) -> &mut T
    {
        &mut self.1
    }

    pub fn as_deref<O>(&self) -> Spanned<&O>
    where
        T: Deref<Target = O>,
        O: ?Sized,
    {
        Spanned(self.0.clone(), self.1.deref())
    }

    pub fn as_deref_mut<O>(&mut self) -> Spanned<&mut O>
    where
        T: DerefMut<Target = O>,
        O: ?Sized,
    {
        Spanned(self.0.clone(), self.1.deref_mut())
    }

    pub fn as_ref(&self) -> Spanned<&T>
    {
        Spanned(self.0.clone(), &self.1)
    }

    pub fn as_mut(&mut self) -> Spanned<&mut T>
    {
        Spanned(self.0.clone(), &mut self.1)
    }

    pub fn map<U, F>(
        self,
        f: F,
    ) -> Spanned<U>
    where
        F: FnOnce(T) -> U,
    {
        Spanned(self.0, f(self.1))
    }

    pub fn into_tuple(self) -> (Span, T)
    {
        (self.0, self.1)
    }
}

impl<T> Spanned<&T>
{
    pub fn cloned(&self) -> Spanned<T>
    where
        T: Clone,
    {
        Spanned(self.0.clone(), self.1.clone())
    }

    pub fn copied(&self) -> Spanned<T>
    where
        T: Copy,
    {
        Spanned(self.0.clone(), *self.1)
    }
}

impl<T> Spanned<&mut T>
{
    pub fn cloned(&self) -> Spanned<T>
    where
        T: Clone,
    {
        Spanned(self.0.clone(), self.1.clone())
    }

    pub fn copied(&self) -> Spanned<T>
    where
        T: Copy,
    {
        Spanned(self.0.clone(), *self.1)
    }
}

impl<T: Copy> Spanned<T>
{
    pub fn value_copied(&self) -> T
    {
        self.1
    }
}

impl<T> Spannable for Spanned<T>
{
    fn span(&self) -> Span
    {
        self.0.clone()
    }
}

impl<T> From<(&Span, T)> for Spanned<T>
{
    fn from((span, value): (&Span, T)) -> Self
    {
        Self(span.clone(), value)
    }
}

impl<T> AsRef<T> for Spanned<T>
{
    fn as_ref(&self) -> &T
    {
        self
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program
{
    pub exprs: Vec<Expr>,
}

impl Spannable for Expr
{
    fn span(&self) -> Span
    {
        match self {
            Expr::Int(Spanned(span, _))
            | Expr::Float(Spanned(span, _))
            | Expr::Ident(Spanned(span, _))
            | Expr::String(Spanned(span, _))
            | Expr::Bool(Spanned(span, _)) => span.clone(),

            Expr::ImportDirective { namespace_path } => namespace_path.span(),

            Expr::NamespaceDirective(mod_token, namespace_name) => {
                (mod_token.span().start)..(namespace_name.span().end)
            }

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
                callee,
                left_paren: _,
                arguments: _,
                right_paren,
            } => (callee.span().start)..(right_paren.span().end),

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
pub enum Expr
{
    Int(Spanned<i32>),
    Float(Spanned<f32>),
    Ident(Spanned<String>),
    Bool(Spanned<bool>),
    String(Spanned<String>),

    NamespaceDirective(ModToken, NamespaceDirectiveKind),

    ImportDirective
    {
        namespace_path: Spanned<Box<NamespacePath>>,
    },

    BinaryOperation
    {
        lhs: Box<Self>,
        operator: Operator,
        rhs: Box<Self>,
    },

    Assign
    {
        lhs: Box<Self>,
        eq_token: tokens::Eq,
        value: Box<Self>,
    },

    Function
    {
        visibility: ItemVisibility,

        fn_token: FunctionToken,
        name: Option<Spanned<String>>,

        lpt: LeftParenthesisToken,
        arguments: Vec<Argument>,
        rpt: RightParenthesisToken,

        return_type_token: Option<ArrowToken>,
        return_type_annotation: Option<Spanned<TypeAnnotation>>,

        equals: Eq,

        body: Box<Self>,
    },

    Call
    {
        callee: Box<Self>,
        left_paren: LeftParenthesisToken,
        arguments: Vec<Self>,
        right_paren: RightParenthesisToken,
    },

    Block
    {
        left_brace: LeftBraceToken,
        expressions: Vec<Self>,
        right_brace: RightBraceToken,
    },

    Return(ReturnToken, Option<Box<Self>>),

    Poisoned,
}

#[derive(Debug, Clone, PartialEq, Eq, From)]
pub enum ItemVisibility
{
    Public(PublicToken),

    Unspecified,
}

impl ItemVisibility
{
    pub fn span(&self) -> Option<Span>
    {
        use ItemVisibility::*;

        match self {
            Public(token) => Some(token.span()),
            Unspecified => None,
        }
    }
}

impl From<Option<ItemVisibility>> for ItemVisibility
{
    fn from(value: Option<ItemVisibility>) -> Self
    {
        value.unwrap_or(ItemVisibility::Unspecified)
    }
}

#[derive(Debug, Clone, PartialEq, derive_more::From)]
pub struct Argument
{
    pub name: Spanned<String>,
    pub type_annotation: Spanned<TypeAnnotation>,
}

#[derive(Debug, Clone, PartialEq)]
#[enum_dispatch(Spannable)]
pub enum NamespaceDirectiveKind
{
    ImportLocalNamespace(Spanned<String>),
}
