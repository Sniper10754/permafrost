use core::fmt::Display;

use alloc::{boxed::Box, collections::BTreeMap, string::String, vec::Vec};
use frostbite_parser::ast::{
    tokens::{Operator, TypeAnnotation},
    Spanned,
};

#[derive(Debug, derive_more::From, derive_more::Into)]
pub struct Typed<T>(Type, T);

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

/// A High level representation of the input
#[derive(Debug, Default)]
pub struct TirTree {
    pub nodes: Vec<TirNode>,
}

impl Display for TirTree {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        for node in &self.nodes {
            writeln!(f, "{node}")?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum TirNode {
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

    Poisoned,
    Uninitialized,
}

impl Display for TirNode {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            TirNode::Int(Spanned(_, value)) => write!(f, "{value}"),
            TirNode::Float(Spanned(_, value)) => write!(f, "{value}"),
            TirNode::Ident(r#type, Spanned(_, value)) => write!(f, "{value}: ({})", r#type),
            TirNode::String(Spanned(_, value)) => write!(f, "{value}"),
            TirNode::BinaryOperation { lhs, operator, rhs } => write!(f, "{lhs} {operator} {rhs}"),
            TirNode::Assign { lhs, value } => write!(f, "{lhs} = {value}"),
            TirNode::Function {
                name,
                arguments,
                return_type,
                body,
            } => write!(
                f,
                "function {}({}) -> {return_type} = {body}",
                name.as_ref()
                    .map(|Spanned(_, name)| name.as_str())
                    .unwrap_or(""),
                {
                    let mut buf = String::new();

                    utils::write_map_as_kvs(arguments.iter(), &mut buf)?;

                    buf
                }
            ),
            TirNode::Call {
                callee,
                arguments,
                return_type,
            } => write!(f, "({})({}) -> {return_type}", callee.1, {
                use core::fmt::Write as _;

                let mut buf = String::new();

                let mut iter = arguments.iter();
                if let Some(arg) = iter.next() {
                    write!(buf, "{}", arg)?;

                    for arg in iter {
                        write!(buf, ", {}", arg)?;
                    }
                }

                buf
            }),
            TirNode::Poisoned => write!(f, "Poisoned tree branch (compilation error happened)"),
            TirNode::Uninitialized => unreachable!(),
        }
    }
}

#[non_exhaustive]
#[derive(Debug, derive_more::Display)]
pub enum Assignable {
    #[display(fmt = "{}: ({})", "&_1.1", "_0")]
    Ident(Type, Spanned<String>),
}

impl TryFrom<TirNode> for Assignable {
    type Error = ();

    fn try_from(value: TirNode) -> Result<Self, Self::Error> {
        match value {
            TirNode::Ident(r#type, ident @ Spanned(..)) => Ok(Assignable::Ident(r#type, ident)),

            _ => Err(()),
        }
    }
}

#[derive(Debug, derive_more::Display)]
pub enum Type {
    #[display(fmt = "int")]
    Int,
    #[display(fmt = "float")]
    Float,
    #[display(fmt = "str")]
    String,

    #[display(
        fmt = "fn {} -> {}",
        "{
            let mut buf = String::new();
            
            utils::write_map_as_kvs(
                arguments.iter(),
                &mut buf
            )?;

            buf
        }",
        "return_value"
    )]
    Function {
        arguments: BTreeMap<String, Self>,
        return_value: Box<Self>,
    },

    #[display(fmt = "()")]
    Unit,

    #[display(fmt = "class {_0}")]
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
