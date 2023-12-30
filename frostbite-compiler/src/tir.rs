use alloc::{boxed::Box, collections::BTreeMap, string::String, vec::Vec};
use frostbite_parser::ast::{
    tokens::{Operator, TypeAnnotation},
    Spanned,
};
use slotmap::{new_key_type, SlotMap};

new_key_type! {
    pub struct LocalIndex;
    pub struct TypeIndex;
}

pub mod display {

    use alloc::{borrow::Cow, format, string::String};
    use core::fmt::{Display, Write as _};

    use super::{TirFunction, TirTree, TypeIndex};
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

    pub fn display_type(type_index: TypeIndex, t_ir_tree: &TirTree) -> Cow<'static, str> {
        match &t_ir_tree.types_arena[type_index] {
            Int => "int".into(),
            Float => "float".into(),
            String => "str".into(),
            Function(TirFunction {
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
        }
    }
}

#[derive(Debug, derive_more::From, derive_more::Into)]
pub struct Typed<T>(TypeIndex, T);

/// A High level representation of the input
#[derive(Debug, Default)]
pub struct TirTree {
    pub nodes: Vec<TirNode>,
    pub locals: SlotMap<LocalIndex, TypeIndex>,
    pub types_arena: SlotMap<TypeIndex, Type>,
}

#[derive(Debug, Clone, derive_more::IsVariant)]
pub enum TirNode {
    Int(Spanned<i32>),
    Float(Spanned<f32>),
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

    Function {
        type_index: TypeIndex,
        name: Option<Spanned<String>>,
        arguments: BTreeMap<String, TypeIndex>,
        return_type: TypeIndex,
        body: Box<Self>,
    },

    Call {
        callee: Callable,
        arguments: Vec<Self>,
        return_type: TypeIndex,
    },

    Poisoned,
    Uninitialized,
}

#[derive(Debug, Clone)]
pub enum Callable {
    Ident(TypeIndex, Spanned<String>),
}

#[derive(Debug, Clone, Copy)]
pub enum RefersTo {
    Local(LocalIndex),
    Type(TypeIndex),
}

impl RefersTo {
    pub fn into_type(self, t_ir_tree: &TirTree) -> TypeIndex {
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

impl TryFrom<TirNode> for Assignable {
    type Error = ();

    fn try_from(value: TirNode) -> Result<Self, Self::Error> {
        match value {
            TirNode::Ident {
                type_index: ty,
                refers_to: _,
                str_value: ident,
            } => Ok(Assignable::Ident(ty, ident)),

            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TirFunction {
    pub arguments: BTreeMap<String, TypeIndex>,

    pub return_type: TypeIndex,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    String,

    Function(TirFunction),

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
        }
    }
}
