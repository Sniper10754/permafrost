use alloc::{boxed::Box, collections::BTreeMap, string::String, vec::Vec};
use dbg_pls::DebugPls;
use frostbite_ast::{
    tokens::{
        FunctionToken, LeftBraceToken, LeftParenthesisToken, Operator, ReturnToken,
        RightBraceToken, RightParenthesisToken,
    },
    Spannable, Spanned,
};
use slotmap::{new_key_type, SecondaryMap, SlotMap};

use super::named::LocalKey;

pub type TypesArena = SlotMap<TypeKey, Type>;

new_key_type! {
    #[derive(derive_more::Display)]
    #[display(fmt = "{}", "self.0.as_ffi() as u32")]
    pub struct TypeKey;
}

impl DebugPls for TypeKey
{
    fn fmt(
        &self,
        f: dbg_pls::Formatter<'_>,
    )
    {
        f.debug_tuple_struct("TypeKey")
            .field(&(self.0.as_ffi() as u32))
            .finish()
    }
}

pub mod display
{
    use alloc::{borrow::Cow, format, string::String};
    use core::fmt::{Display, Write as _};

    use super::{FunctionType, TypeKey};
    use crate::{context::TypeContext, ir::typed::Type::*};

    fn join_map_into_string<K, V>(mut map: impl Iterator<Item = (K, V)>) -> String
    where
        K: Display,
        V: Display,
    {
        let mut buffer = String::new();

        if let Some((k, v)) = map.next() {
            write!(buffer, "{k}: {v}").expect("Unreachable");

            for (k, v) in map {
                write!(buffer, ", {k}: {v}").expect("Unreachable");
            }
        }

        buffer
    }

    pub fn display_type(
        type_key: TypeKey,
        type_ctx: &TypeContext,
    ) -> Cow<'static, str>
    {
        let type_description: Cow<'_, _> = match type_ctx.get_type(type_key) {
            Int => "int".into(),
            Float => "float".into(),
            String => "str".into(),
            Function(FunctionType {
                arguments,
                return_type,
            }) => format!(
                "({}) -> {}",
                join_map_into_string(
                    arguments
                        .iter()
                        .map(|(name, type_idx)| (name.as_str(), display_type(*type_idx, type_ctx)))
                ),
                display_type(*return_type, type_ctx)
            )
            .into(),
            Unit => "()".into(),
            Any => "any".into(),
            Bool => "bool".into(),
        };

        format!("{type_description} [{type_key}]").into()
    }
}

#[derive(Debug, derive_more::From, derive_more::Into)]
pub struct Typed<T>(TypeKey, T);

/// A High level representation of the input
#[derive(Debug, Default)]
pub struct TypedAst
{
    pub nodes: Vec<TypedExpression>,
    pub locals: SecondaryMap<LocalKey, TypeKey>,
}

impl DebugPls for TypedAst
{
    fn fmt(
        &self,
        f: dbg_pls::Formatter<'_>,
    )
    {
        struct LocalsDebugPls<'a>(&'a SecondaryMap<LocalKey, TypeKey>);

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
            .field("nodes", &self.nodes)
            .field("locals", &LocalsDebugPls(&self.locals))
            .finish()
    }
}

#[derive(Debug, Clone, DebugPls)]
pub struct TypedFunction
{
    pub fn_token: FunctionToken,
    pub name: Option<Spanned<String>>,
    pub arguments: BTreeMap<String, TypeKey>,
    pub return_type: TypeKey,
    pub body: Box<TypedExpression>,
}

#[derive(Debug, Clone, DebugPls)]
pub struct TypedExpression
{
    pub type_key: TypeKey,
    pub kind: TypedExpressionKind,
}

impl TypedFunction
{
    pub fn is_body_block(&self) -> bool
    {
        matches!(&self.body.kind, TypedExpressionKind::Block { .. })
    }
}

#[derive(Debug, Clone, DebugPls, derive_more::IsVariant)]
pub enum TypedExpressionKind
{
    Int(Spanned<i32>),
    Float(Spanned<f32>),
    Bool(Spanned<bool>),
    String(Spanned<String>),

    Ident
    {
        str_value: Spanned<String>,
    },

    BinaryOperation
    {
        lhs: Box<TypedExpression>,
        operator: Operator,
        rhs: Box<TypedExpression>,
    },

    Assign
    {
        lhs: Assignable,
        value: Box<TypedExpression>,
    },

    Function(TypedFunction),

    Call
    {
        callee: Callable,
        right_parent: RightParenthesisToken,
        arguments: Vec<TypedExpression>,
        left_parent: LeftParenthesisToken,
        return_type: TypeKey,
    },

    Return(TypeKey, ReturnToken, Option<Box<TypedExpression>>),

    Block
    {
        left_brace: LeftBraceToken,
        expressions: Vec<TypedExpression>,
        right_brace: RightBraceToken,
    },
}

impl Spannable for TypedExpressionKind
{
    fn span(&self) -> frostbite_ast::Span
    {
        use TypedExpressionKind::*;

        match self {
            Int(Spanned(span, _))
            | Float(Spanned(span, _))
            | Bool(Spanned(span, _))
            | String(Spanned(span, _)) => span.clone(),

            Ident {
                str_value: Spanned(span, _),
            } => span.clone(),
            BinaryOperation {
                lhs,
                operator: _,
                rhs,
            } => (lhs.kind.span().start)..(rhs.kind.span().end),
            Assign { lhs, value } => (lhs.span().start)..(value.kind.span().end),
            Function(TypedFunction { fn_token, body, .. }) => {
                (fn_token.span().start)..(body.kind.span().end)
            }
            Call {
                callee: _,
                left_parent,
                arguments: _,
                right_parent,
                return_type: _,
            } => (left_parent.span().start)..(right_parent.0.end),
            Return(_, ret_token, value) => {
                (ret_token.0.start)
                    ..(value
                        .as_ref()
                        .map(|value| value.kind.span())
                        .map(|span| span.start)
                        .unwrap_or(ret_token.span().end))
            }
            Block {
                left_brace,
                expressions: _,
                right_brace,
            } => (left_brace.span().start)..(right_brace.span().end),
        }
    }
}

#[derive(Debug, Clone, DebugPls)]
pub enum Callable
{
    Function(TypeKey, Spanned<String>),
}

impl Spannable for Callable
{
    fn span(&self) -> frostbite_ast::Span
    {
        match self {
            Callable::Function(_, Spanned(span, _)) => span.clone(),
        }
    }
}

#[non_exhaustive]
#[derive(Debug, Clone, DebugPls)]
pub enum Assignable
{
    Ident(TypeKey, Spanned<String>),
}

impl Spannable for Assignable
{
    fn span(&self) -> frostbite_ast::Span
    {
        match self {
            Assignable::Ident(_, Spanned(span, _)) => span.clone(),
        }
    }
}

impl TryFrom<TypedExpression> for Assignable
{
    type Error = ();

    fn try_from(value: TypedExpression) -> Result<Self, Self::Error>
    {
        use TypedExpressionKind::*;

        match value.kind {
            Ident { str_value: ident } => Ok(Assignable::Ident(value.type_key, ident)),

            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType
{
    pub arguments: BTreeMap<String, TypeKey>,
    pub return_type: TypeKey,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type
{
    Int,
    Float,
    String,
    Bool,

    Function(FunctionType),

    Unit,

    Any,
}
