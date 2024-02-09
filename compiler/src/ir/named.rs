use alloc::{boxed::Box, string::String, vec::Vec};
use dbg_pls::DebugPls;
use permafrost_ast::{
    tokens::{
        ArrowToken, FunctionToken, LeftBraceToken, LeftParenthesisToken, Operator, ReturnToken,
        RightBraceToken, RightParenthesisToken, TypeAnnotation,
    },
    Span, Spannable, Spanned,
};
use slotmap::{new_key_type, SlotMap};

use crate::context::names::{Export, NamespaceKey};

new_key_type! {
    #[derive(derive_more::Display)]
    #[display(fmt = "{}", "self.0.as_ffi() as u32")]
    pub struct LocalKey;
}

impl DebugPls for LocalKey
{
    fn fmt(
        &self,
        f: dbg_pls::Formatter<'_>,
    )
    {
        f.debug_tuple_struct("LocalKey")
            .field(&(self.0.as_ffi() as u32))
            .finish()
    }
}

#[derive(Debug, Clone, Default)]
pub struct NamedAst
{
    pub exprs: Vec<NamedExpr>,
    pub locals: SlotMap<LocalKey, ()>,
}

impl DebugPls for NamedAst
{
    fn fmt(
        &self,
        f: dbg_pls::Formatter<'_>,
    )
    {
        struct LocalsDebugPls<'a>(&'a SlotMap<LocalKey, ()>);

        impl<'a> DebugPls for LocalsDebugPls<'a>
        {
            fn fmt(
                &self,
                f: dbg_pls::Formatter<'_>,
            )
            {
                f.debug_map().entries(self.0.iter()).finish()
            }
        }

        f.debug_struct("TypedAst")
            .field("nodes", &self.exprs)
            .field("locals", &LocalsDebugPls(&self.locals))
            .finish()
    }
}

impl Spannable for NamedExpr
{
    fn span(&self) -> Span
    {
        match self {
            NamedExpr::Int(Spanned(span, _))
            | NamedExpr::Float(Spanned(span, _))
            | NamedExpr::Ident {
                local_key: _,
                identifier: Spanned(span, _),
            }
            | NamedExpr::String(Spanned(span, _))
            | NamedExpr::Bool(Spanned(span, _))
            | NamedExpr::NamespaceDirective(span)
            | NamedExpr::UseDirective(span) => span.clone(),

            NamedExpr::BinaryOperation {
                lhs,
                operator: _,
                rhs,
            } => (lhs.span().start)..(rhs.span().end),
            NamedExpr::Assign { lhs, body: value } => (lhs.span().start)..(value.span().end),

            NamedExpr::Function { fn_token, body, .. } => {
                (fn_token.span().start)..(body.span().end)
            }
            NamedExpr::Call {
                callee: _,
                left_paren,
                arguments: _,
                right_paren,
            } => (left_paren.span().start)..(right_paren.span().end),

            NamedExpr::Block {
                left_brace,
                expressions: _,
                right_brace,
            } => (left_brace.span().start)..(right_brace.span().end),

            NamedExpr::Return(ret_token, expr) => {
                (ret_token.span().start)
                    ..expr
                        .as_ref()
                        .map(|expr| expr.span().end)
                        .unwrap_or(ret_token.span().end)
            }

            NamedExpr::Poisoned => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum NamedExpr
{
    Int(Spanned<i32>),
    Float(Spanned<f32>),
    Bool(Spanned<bool>),
    String(Spanned<String>),

    UseDirective(Span),
    NamespaceDirective(Span),

    Ident
    {
        local_key: LocalKey,
        identifier: Spanned<String>,
    },

    BinaryOperation
    {
        lhs: Box<Self>,
        operator: Operator,
        rhs: Box<Self>,
    },

    Assign
    {
        lhs: Assignable,
        body: Box<Self>,
    },

    Function
    {
        local_key: Option<LocalKey>,

        fn_token: FunctionToken,
        name: Option<Spanned<String>>,
        arguments: Vec<Argument>,
        return_type_token: Option<ArrowToken>,
        return_type_annotation: Option<Spanned<TypeAnnotation>>,
        body: Box<Self>,
    },

    Call
    {
        callee: Box<Self>,

        left_paren: LeftParenthesisToken,
        arguments: Vec<NamedExpr>,
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

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum Assignable
{
    Ident(LocalKey, Spanned<String>),
}

impl From<Assignable> for NamedExpr
{
    fn from(value: Assignable) -> Self
    {
        match value {
            Assignable::Ident(local_key, identifier) => NamedExpr::Ident {
                local_key,
                identifier,
            },
        }
    }
}

impl Spannable for Assignable
{
    fn span(&self) -> permafrost_ast::Span
    {
        match self {
            Assignable::Ident(_, spanned) => spanned.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Argument
{
    pub local_key: LocalKey,
    pub name: Spanned<String>,
    pub type_annotation: Spanned<TypeAnnotation>,
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum ImportDirectiveKind
{
    FromNamespaceImportSymbol
    {
        namespace: NamespacePath,
        symbol: Spanned<String>,
    },
    ImportNamespace
    {
        namespace: NamespacePath
    },
}

#[derive(Debug, Clone, PartialEq, DebugPls, derive_more::Display, derive_more::From)]
pub struct NamespacePath
{
    name: String,
}

#[derive(Debug, Clone, Copy, PartialEq, DebugPls, derive_more::From)]
pub enum ResolvedSymbol
{
    LocalKey(LocalKey),
    NamespaceKey(NamespaceKey),
}

impl From<Export> for ResolvedSymbol
{
    fn from(value: Export) -> Self
    {
        match value {
            Export::Local(local_key) => Self::from(local_key),
            Export::Namespace(namespace_key) => Self::from(namespace_key),
        }
    }
}
