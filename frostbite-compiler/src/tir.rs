use alloc::{boxed::Box, collections::BTreeMap, string::String, vec::Vec};
use frostbite_parser::ast::{
    tokens::{Operator, TypeAnnotation},
    Spanned,
};
use slotmap::{new_key_type, SlotMap};

mod utils {
    pub fn write_map_as_kvs<I, A, B>(map: I, f: &mut dyn core::fmt::Write) -> core::fmt::Result
    where
        I: IntoIterator<Item = (A, B)>,
        A: core::fmt::Display,
        B: core::fmt::Display,
    {
        let mut iter = map.into_iter();

        if let Some((k, v)) = iter.next() {
            core::write!(f, "{}: {}", k, v)?;

            for (k, v) in iter {
                core::write!(f, ", {}: {}", k, v)?;
            }
        }
        Ok(())
    }
}

new_key_type! {
    pub struct LocalIndex;
    pub struct TypeIndex;
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

#[derive(Debug, Clone)]
pub enum TirNode {
    Int(Spanned<i32>),
    Float(Spanned<f32>),
    String(Spanned<String>),

    Ident {
        r#type: TypeIndex,
        refers_to: RefersTo,
        str_value: Spanned<String>,
    },

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
                r#type,
                refers_to: _,
                str_value: ident,
            } => Ok(Assignable::Ident(r#type, ident)),

            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TirFunction {
    pub arguments: BTreeMap<String, TypeIndex>,

    pub return_type: TypeIndex,
}

#[derive(Debug, Clone)]
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
