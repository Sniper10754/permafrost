use alloc::{boxed::Box, collections::BTreeMap, string::String, vec::Vec};
use frostbite_parser::ast::{
    tokens::{
        FunctionToken, LeftBraceToken, LeftParenthesisToken, Operator, ReturnToken,
        RightBraceToken, RightParenthesisToken, TypeAnnotation,
    },
    Spannable, Spanned,
};
use slotmap::{new_key_type, SlotMap};

pub type TypesArena = SlotMap<TypeKey, Type>;

new_key_type! {
    #[derive(derive_more::Display)]
    #[display(fmt = "{}", "self.0.as_ffi() as u32")]
    pub struct LocalKey;
    #[derive(derive_more::Display)]
    #[display(fmt = "{}", "self.0.as_ffi() as u32")]
    pub struct TypeKey;
}

pub mod display
{
    use alloc::{borrow::Cow, format, string::String};
    use core::fmt::{self, Display, Write as _};
    use frostbite_parser::ast::Spanned;

    use super::{FunctionType, TypeKey, TypedAst, TypedExpression, TypesArena};
    use crate::tir::{Assignable, Callable, Type::*, TypedExpressionKind, TypedFunction};

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
        types_arena: &TypesArena,
    ) -> Cow<'static, str>
    {
        let type_description: Cow<'_, _> =
            match &types_arena[type_key] {
                Int => "int".into(),
                Float => "float".into(),
                String => "str".into(),
                Function(FunctionType {
                    arguments,
                    return_type,
                }) => format!(
                    "({}) -> {}",
                    join_map_into_string(arguments.iter().map(|(name, type_idx)| (
                        name.as_str(),
                        display_type(*type_idx, types_arena)
                    ))),
                    display_type(*return_type, types_arena)
                )
                .into(),
                Unit => "()".into(),
                Object(object_name) => format!("object {object_name}").into(),
                Any => "any".into(),
                Bool => "bool".into(),
            };

        format!("{type_description} [{type_key}]").into()
    }

    pub fn display_tree(
        t_ast: &TypedAst,
        types_arena: &TypesArena,
    ) -> String
    {
        let mut buf = String::new();

        t_ast
            .nodes
            .iter()
            .try_for_each::<_, fmt::Result>(|node| {
                display_node(&mut buf, t_ast, types_arena, node)?;

                writeln!(buf)?;

                Ok(())
            })
            .expect("infallible");

        buf
    }

    pub fn display_node(
        buf: &mut String,
        t_ast: &TypedAst,
        types_arena: &TypesArena,
        node: &TypedExpression,
    ) -> fmt::Result
    {
        use TypedExpressionKind::*;

        match &node.typed_expression_kind {
            Int(Spanned(_, value)) => write!(buf, "{value}"),
            Float(Spanned(_, value)) => write!(buf, "{value}"),
            Bool(Spanned(_, value)) => write!(buf, "{value}"),
            String(Spanned(_, value)) => write!(buf, "{value}"),
            Ident {
                refers_to,
                str_value: Spanned(_, name),
            } => {
                write!(buf, "{name} (")?;

                write!(
                    buf,
                    "{}",
                    display_type(refers_to.into_type(t_ast), types_arena)
                )?;

                write!(buf, ") ")?;

                Ok(())
            }
            BinaryOperation { lhs, operator, rhs } => {
                display_node(buf, t_ast, types_arena, lhs)?;

                write!(buf, "{} ", operator.kind)?;

                display_node(buf, t_ast, types_arena, rhs)?;

                Ok(())
            }
            Assign { lhs, value } => {
                match lhs {
                    Assignable::Ident(type_key, Spanned(_, name)) => {
                        write!(buf, "{name} [{type_key}]")?;
                    }
                }

                write!(buf, " = ")?;

                display_node(buf, t_ast, types_arena, value)?;

                Ok(())
            }
            Function(TypedFunction {
                fn_token: _,
                name,
                arguments,
                return_type,
                body,
            }) => {
                write!(buf, "function ")?;

                if let Some(Spanned(_, name)) = name {
                    write!(buf, "{name}")?;
                }

                write!(buf, "(")?;

                {
                    let mut arguments_iter = arguments.iter();

                    if let Some((name, type_key)) = arguments_iter.next() {
                        write!(buf, "{name}: {}", display_type(*type_key, types_arena))?;

                        arguments_iter.try_for_each(|(name, type_key)| {
                            write!(buf, ", {name}: {}", display_type(*type_key, types_arena))
                        })?;
                    }
                }

                write!(buf, ") ")?;
                write!(buf, "-> {}", display_type(*return_type, types_arena))?;
                writeln!(buf)?;
                write!(buf, "\t= ")?;

                display_node(buf, t_ast, types_arena, body)?;

                Ok(())
            }
            Call {
                callee,
                right_parent: _,
                arguments,
                left_parent: _,
                return_type,
            } => {
                match callee {
                    Callable::Function(refers_to, Spanned(_, name)) => {
                        write!(
                            buf,
                            "{name} (which has type {})",
                            display_type(refers_to.into_type(t_ast), types_arena)
                        )?;
                    }
                }

                write!(buf, "(")?;
                {
                    let mut arguments_iter = arguments.iter();

                    if let Some(argument) = arguments_iter.next() {
                        display_node(buf, t_ast, types_arena, argument)?;

                        arguments_iter.try_for_each(|argument| {
                            write!(buf, ", ")?;

                            display_node(buf, t_ast, types_arena, argument)?;

                            Ok(())
                        })?;
                    }
                }
                write!(buf, ")")?;

                write!(
                    buf,
                    " # (returns {})",
                    display_type(*return_type, types_arena)
                )?;

                Ok(())
            }
            Return(..) => todo!(),
            Block {
                left_brace: _,
                expressions,
                right_brace: _,
            } => {
                writeln!(buf, "{{")?;

                expressions
                    .iter()
                    .try_for_each(|expr| display_node(buf, t_ast, types_arena, expr))?;

                writeln!(buf, "}}")?;

                Ok(())
            }
        }
    }
}

#[derive(Debug, derive_more::From, derive_more::Into)]
pub struct Typed<T>(TypeKey, T);

/// A High level representation of the input
#[derive(Debug, Default)]
pub struct TypedAst
{
    pub nodes: Vec<TypedExpression>,
    pub locals: SlotMap<LocalKey, TypeKey>,
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
    pub typed_expression_kind: TypedExpressionKind,
}

impl TypedFunction
{
    pub fn is_body_block(&self) -> bool
    {
        matches!(
            &self.body.typed_expression_kind,
            TypedExpressionKind::Block { .. }
        )
    }
}

#[derive(Debug, Clone, derive_more::IsVariant)]
pub enum TypedExpressionKind
{
    Int(Spanned<i32>),
    Float(Spanned<f32>),
    Bool(Spanned<bool>),
    String(Spanned<String>),

    Ident
    {
        refers_to: RefersTo,
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
    fn span(&self) -> frostbite_parser::ast::Span
    {
        use TypedExpressionKind::*;

        match self {
            Int(Spanned(span, _)) => span.clone(),
            Float(Spanned(span, _)) => span.clone(),
            Bool(Spanned(span, _)) => span.clone(),
            String(Spanned(span, _)) => span.clone(),
            Ident {
                refers_to: _,
                str_value: Spanned(span, _),
            } => span.clone(),
            BinaryOperation {
                lhs,
                operator: _,
                rhs,
            } => (lhs.typed_expression_kind.span().start)..(rhs.typed_expression_kind.span().end),
            Assign { lhs, value } => (lhs.span().start)..(value.typed_expression_kind.span().end),
            Function(TypedFunction { fn_token, body, .. }) => {
                (fn_token.span().start)..(body.typed_expression_kind.span().end)
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
                        .map(|value| value.typed_expression_kind.span())
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
    Function(RefersTo, Spanned<String>),
}

impl Spannable for Callable
{
    fn span(&self) -> frostbite_parser::ast::Span
    {
        match self {
            Callable::Function(_, Spanned(span, _)) => span.clone(),
        }
    }
}

#[derive(Debug, Clone, Copy, derive_more::From)]
pub enum RefersTo
{
    Local(LocalKey),
    Type(TypeKey),
}

impl RefersTo
{
    pub fn into_type(
        self,
        t_ast: &TypedAst,
    ) -> TypeKey
    {
        match self {
            RefersTo::Local(local_index) => t_ast.locals[local_index],
            RefersTo::Type(type_index) => type_index,
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
    fn span(&self) -> frostbite_parser::ast::Span
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

        match value.typed_expression_kind {
            Ident {
                refers_to: _,
                str_value: ident,
            } => Ok(Assignable::Ident(value.type_key, ident)),

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

    Object(String),

    Any,
}

impl From<TypeAnnotation> for Type
{
    fn from(value: TypeAnnotation) -> Self
    {
        match value {
            TypeAnnotation::Int => Self::Int,
            TypeAnnotation::Float => Self::Float,
            TypeAnnotation::String => Self::String,
            TypeAnnotation::Any => Self::Any,
            TypeAnnotation::Unit => Self::Unit,
            TypeAnnotation::Object(obj) => Self::Object(obj),
            TypeAnnotation::Bool => Self::Bool,
        }
    }
}
