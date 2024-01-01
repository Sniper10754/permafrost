use alloc::{boxed::Box, collections::BTreeMap, string::String, vec::Vec};
use frostbite_parser::ast::{
    tokens::{FunctionToken, Operator, RightParenthesisToken, TypeAnnotation},
    Spannable, Spanned,
};
use slotmap::{new_key_type, SlotMap};

new_key_type! {
    pub struct LocalIndex;
    pub struct TypeIndex;
}

pub mod display {
    use alloc::{borrow::Cow, format, string::String};
    use core::fmt::{Display, Write as _};

    use super::{FunctionType, TypeIndex, TypedAst};
    use crate::tir::Type::*;

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

    pub fn display_type(type_index: TypeIndex, t_ir_tree: &TypedAst) -> Cow<'static, str> {
        match &t_ir_tree.types_arena[type_index] {
            Int => "int".into(),
            Float => "float".into(),
            String => "str".into(),
            Function(FunctionType {
                arguments,
                return_type,
            }) => {
                format!(
                    "fn ({}) -> {}",
                    join_map_into_string(arguments.iter().map(|(name, type_idx)| (
                        name.as_str(),
                        display_type(*type_idx, t_ir_tree)
                    ))),
                    display_type(*return_type, t_ir_tree)
                )
                .into()
            }
            Unit => "()".into(),
            Object(object_name) => format!("object {object_name}").into(),
            Any => "any".into(),
            Bool => "bool".into(),
        }
    }
}

#[derive(Debug, derive_more::From, derive_more::Into)]
pub struct Typed<T>(TypeIndex, T);

/// A High level representation of the input
#[derive(Debug, Default)]
pub struct TypedAst {
    pub nodes: Vec<TypedExpression>,
    pub locals: SlotMap<LocalIndex, TypeIndex>,
    pub types_arena: SlotMap<TypeIndex, Type>,
}

#[derive(Debug, Clone)]
pub struct TypedFunctionExpr {
    pub fn_token: FunctionToken,
    pub type_index: TypeIndex,
    pub name: Option<Spanned<String>>,
    pub arguments: BTreeMap<String, TypeIndex>,
    pub return_type: TypeIndex,
    pub body: Box<TypedExpression>,
}

#[derive(Debug, Clone, derive_more::IsVariant)]
pub enum TypedExpression {
    Int(Spanned<i32>),
    Float(Spanned<f32>),
    Bool(Spanned<bool>),
    String(Spanned<String>),

    Ident {
        type_index: TypeIndex,
        refers_to: RefersTo,
        str_value: Spanned<String>,
    },

    BinaryOperation {
        lhs: Box<Self>,
        operator: Operator,
        rhs: Box<Self>,
    },

    Assign {
        local_index: LocalIndex,
        lhs: Assignable,
        value: Box<Self>,
    },

    Function(TypedFunctionExpr),

    Call {
        callee: Callable,
        arguments: Vec<Self>,
        return_type: TypeIndex,
        right_parent: RightParenthesisToken,
    },

    Return(TypeIndex, Option<Box<Self>>),

    Block {
        expressions: Vec<Self>,
    },

    Poisoned,
    Uninitialized,
}

impl Spannable for TypedExpression {
    fn span(&self) -> frostbite_parser::ast::Span {
        match self {
            TypedExpression::Int(Spanned(span, _)) => span.clone(),
            TypedExpression::Float(Spanned(span, _)) => span.clone(),
            TypedExpression::Bool(Spanned(span, _)) => span.clone(),
            TypedExpression::String(Spanned(span, _)) => span.clone(),
            TypedExpression::Ident {
                type_index,
                refers_to,
                str_value: Spanned(span, _),
            } => span.clone(),
            TypedExpression::BinaryOperation { lhs, operator, rhs } => {
                (lhs.span().start)..(rhs.span().end)
            }
            TypedExpression::Assign {
                local_index,
                lhs,
                value,
            } => (lhs.span().start)..(value.span().end),
            TypedExpression::Function(TypedFunctionExpr {
                fn_token,
                type_index,
                name,
                arguments,
                return_type,
                body,
            }) => (fn_token.span().start)..(body.span().end),
            TypedExpression::Call {
                callee,
                arguments,
                return_type,
                right_parent,
            } => (callee.span().start)..(right_parent.0.end),
            TypedExpression::Return(span, _) => span.clone(),
            TypedExpression::Block { expressions } => todo!(),
            TypedExpression::Poisoned => todo!(),
            TypedExpression::Uninitialized => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Callable {
    Ident(TypeIndex, Spanned<String>),
}

impl Spannable for Callable {
    fn span(&self) -> frostbite_parser::ast::Span {
        match self {
            Callable::Ident(_, Spanned(span, _)) => span.clone(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum RefersTo {
    Local(LocalIndex),
    Type(TypeIndex),
}

impl RefersTo {
    pub fn into_type(self, t_ir_tree: &TypedAst) -> TypeIndex {
        match self {
            RefersTo::Local(local_index) => t_ir_tree.locals[local_index],
            RefersTo::Type(type_index) => type_index,
        }
    }
}

#[non_exhaustive]
#[derive(Debug, Clone)]
pub enum Assignable {
    Ident(TypeIndex, Spanned<String>),
}

impl Spannable for Assignable {
    fn span(&self) -> frostbite_parser::ast::Span {
        match self {
            Assignable::Ident(_, Spanned(span, _)) => span.clone(),
        }
    }
}

impl TryFrom<TypedExpression> for Assignable {
    type Error = ();

    fn try_from(value: TypedExpression) -> Result<Self, Self::Error> {
        match value {
            TypedExpression::Ident {
                type_index: ty,
                refers_to: _,
                str_value: ident,
            } => Ok(Assignable::Ident(ty, ident)),

            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub arguments: BTreeMap<String, TypeIndex>,

    pub return_type: TypeIndex,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    String,
    Bool,

    Function(FunctionType),

    Unit,

    Object(String),

    Any,
}

impl<'a> From<TypeAnnotation<'a>> for Type {
    fn from(value: TypeAnnotation<'a>) -> Self {
        match value {
            TypeAnnotation::Int => Self::Int,
            TypeAnnotation::Float => Self::Float,
            TypeAnnotation::String => Self::String,
            TypeAnnotation::Any => Self::Any,
            TypeAnnotation::NotSpecified => Self::Unit,
            TypeAnnotation::Unit => Self::Unit,
            TypeAnnotation::Object(obj) => Self::Object(obj.into()),
            TypeAnnotation::Bool => Self::Bool,
        }
    }
}
