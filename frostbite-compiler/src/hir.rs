use alloc::{boxed::Box, collections::BTreeMap, string::String, vec::Vec};
use frostbite_parser::ast::{
    tokens::{Operator, TypeAnnotation},
    Spanned,
};

#[derive(Debug, derive_more::From, derive_more::Into)]
pub struct Typed<T>(Type, T);

/// A High level representation of the input
#[derive(Debug, Default)]
pub struct Hir {
    pub nodes: Vec<HirNode>,
}

#[derive(Debug)]
pub enum HirNode {
    Int(Spanned<i32>),
    Float(Spanned<f32>),
    Ident(Type, Spanned<String>),
    String(Spanned<String>),

    BinaryOperation {
        lhs: Box<Self>,
        operator: Operator,
        rhs: Box<Self>,
    },

    Assign {
        lhs: Assignable,

        value: Box<Self>,
    },

    Function {
        name: Option<Spanned<String>>,
        arguments: BTreeMap<String, Type>,
        return_type: Type,
        body: Box<Self>,
    },

    Call {
        callee: Spanned<Type>,
        arguments: Vec<Self>,
        return_type: Type,
    },
}

#[non_exhaustive]
#[derive(Debug)]
pub enum Assignable {
    Ident(Type, Spanned<String>),
}

impl TryFrom<HirNode> for Assignable {
    type Error = ();

    fn try_from(value: HirNode) -> Result<Self, Self::Error> {
        match value {
            HirNode::Ident(r#type, ident @ Spanned(..)) => Ok(Assignable::Ident(r#type, ident)),

            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub enum Type {
    Int,
    Float,
    String,

    Function {
        arguments: BTreeMap<String, Self>,
        return_value: Box<Self>,
    },

    Unit,

    Object(String),
}

impl<'a> From<TypeAnnotation<'a>> for Type {
    fn from(value: TypeAnnotation<'a>) -> Self {
        match value {
            TypeAnnotation::Int => Self::Int,
            TypeAnnotation::Float => Self::Float,
            TypeAnnotation::String => Self::String,
            TypeAnnotation::NotSpecified => Self::Unit,
            TypeAnnotation::Unit => Self::Unit,
            TypeAnnotation::Object(obj) => Self::Object(obj.into()),
        }
    }
}
