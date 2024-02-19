use alloc::{boxed::Box, collections::BTreeMap, string::String, vec::Vec};
use permafrost_ast::{
    tokens::{
        FunctionToken, LeftBraceToken, LeftParenthesisToken, Operator, ReturnToken,
        RightBraceToken, RightParenthesisToken,
    },
    Span, Spannable, Spanned,
};
use slotmap::{new_key_type, SecondaryMap, SlotMap};

use super::named::LocalKey;

pub type TypesArena = SlotMap<TypeKey, Type>;

new_key_type! {
    #[derive(derive_more::Display)]
    #[display(fmt = "{}", "self.0.as_ffi() as u32")]
    pub struct TypeKey;
}

pub mod display
{
    use alloc::{borrow::Cow, format};
    use itertools::Itertools;

    use super::{FunctionType, TypeKey};
    use crate::{context::TypeContext, ir::typed::Type::*};

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
                arguments
                    .iter()
                    .map(|(name, type_idx)| (name.as_str(), display_type(*type_idx, type_ctx)))
                    .map(|(name, ty)| format!("{name}: {ty}"))
                    .join(", "),
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

#[derive(Debug, Clone)]
pub struct TypedFunction
{
    pub fn_token: FunctionToken,
    pub name: Option<Spanned<String>>,
    pub arguments: BTreeMap<String, TypeKey>,
    pub return_type: TypeKey,
    pub body: Box<TypedExpression>,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone, derive_more::IsVariant)]
pub enum TypedExpressionKind
{
    Int(Spanned<i32>),
    Float(Spanned<f32>),
    Bool(Spanned<bool>),
    String(Spanned<String>),

    NamespaceStatement(Span),

    Ident(Spanned<String>),

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
    fn span(&self) -> permafrost_ast::Span
    {
        use TypedExpressionKind::*;

        match self {
            Int(Spanned(span, _))
            | Float(Spanned(span, _))
            | Bool(Spanned(span, _))
            | String(Spanned(span, _))
            | NamespaceStatement(span) => span.clone(),

            Ident(Spanned(span, _)) => span.clone(),
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

#[derive(Debug, Clone)]
pub enum Callable
{
    Function(TypeKey, Spanned<String>),
}

impl Spannable for Callable
{
    fn span(&self) -> permafrost_ast::Span
    {
        match self {
            Callable::Function(_, Spanned(span, _)) => span.clone(),
        }
    }
}

#[non_exhaustive]
#[derive(Debug, Clone)]
pub enum Assignable
{
    Ident(TypeKey, Spanned<String>),
}

impl Spannable for Assignable
{
    fn span(&self) -> permafrost_ast::Span
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
            Ident(ident) => Ok(Assignable::Ident(value.type_key, ident)),

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
