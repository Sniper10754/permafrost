#![allow(clippy::single_match)]

extern crate std;
use core::{
    cmp::Ordering::{Equal, Greater, Less},
    ops::Range,
};

use alloc::{
    borrow::ToOwned, boxed::Box, collections::BTreeMap, format, string::ToString, vec, vec::Vec,
};
use frostbite_parser::ast::{tokens::TypeAnnotation, Expr, Program, Spannable, Spanned};
use frostbite_reports::{
    sourcemap::{SourceId, SourceMap},
    IntoReport, Level, Report, ReportContext,
};

use crate::tir::{TirNode, TirTree, Type};

type ScopeTable<'ast> = Vec<Scope<'ast>>;
type Scope<'ast> = BTreeMap<&'ast str, Symbol<'ast>>;

#[derive(Debug, derive_more::Display, Clone, PartialEq)]
pub enum Symbol<'ast> {
    #[display(fmt = "int")]
    Int,

    #[display(fmt = "float")]
    Float,

    #[display(fmt = "str")]
    String,

    #[display(fmt = "class {_0}")]
    Other(&'ast str),

    #[display(fmt = "function")]
    Function {
        arguments: BTreeMap<&'ast str, Self>,
        return_type: Box<Self>,
    },

    #[display(fmt = "unit")]
    Unit,

    #[display(fmt = "(unknown)")]
    NotSpecified,
}

impl<'ast> From<TypeAnnotation<'ast>> for Symbol<'ast> {
    fn from(value: TypeAnnotation<'ast>) -> Self {
        match value {
            TypeAnnotation::Int => Self::Int,
            TypeAnnotation::Float => Self::Float,
            TypeAnnotation::String => Self::String,
            TypeAnnotation::NotSpecified => Self::NotSpecified,
            TypeAnnotation::Object(other) => Self::Other(other),
            TypeAnnotation::Unit => Self::Unit,
        }
    }
}

impl<'a> From<Symbol<'a>> for Type {
    fn from(value: Symbol<'a>) -> Self {
        match value {
            Symbol::Int => Type::Int,
            Symbol::Float => Type::Float,
            Symbol::String => Type::String,
            Symbol::Other(obj) => Type::Object(obj.to_owned()),
            Symbol::Function {
                arguments,
                return_type,
            } => Type::Function {
                arguments: arguments
                    .into_iter()
                    .map(|(ident, r#type)| (ident.to_owned(), r#type.into()))
                    .collect(),
                return_value: Box::new((*return_type).into()),
            },
            Symbol::Unit => todo!(),
            Symbol::NotSpecified => todo!(),
        }
    }
}

#[derive(Debug)]
pub enum TypecheckError<'ast> {
    TypeMismatch {
        source_id: SourceId,
        span: Range<usize>,

        expected: Symbol<'ast>,
        found: Symbol<'ast>,
    },
    SymbolNotFound(SourceId, Spanned<&'ast str>),
    IncompatibleOperands(SourceId, Range<usize>),
    CannotAssignTo(SourceId, Range<usize>),
    CannotCallNonIdent(SourceId, Range<usize>),
    CannotCallNonFunction(SourceId, Range<usize>),
    TooManyArguments {
        source_id: SourceId,
        span: Range<usize>,

        call_arguments_len: usize,
        function_arguments_len: usize,
    },
    NotEnoughArguments {
        source_id: SourceId,
        span: Range<usize>,

        call_arguments_len: usize,
        function_arguments_len: usize,
    },
}

impl<'ast> IntoReport for TypecheckError<'ast> {
    fn into_report(self) -> Report {
        match self {
            TypecheckError::TypeMismatch {
                source_id,
                span,
                expected,
                found,
            } => Report::new_diagnostic(
                Level::Error,
                span,
                source_id,
                "Type mismatch",
                Some(format!("Expected type {expected}, found type {found}")),
                [],
                [],
            ),
            TypecheckError::SymbolNotFound(source_id, Spanned(span, ident)) => {
                Report::new_diagnostic(
                    Level::Error,
                    span,
                    source_id,
                    "Symbol not found",
                    Some(format!("Symbol {ident} not found")),
                    [],
                    [],
                )
            }
            TypecheckError::IncompatibleOperands(source_id, span) => Report::new_diagnostic(
                Level::Error,
                span,
                source_id,
                "Incompatible operands",
                None::<&str>,
                [],
                [],
            ),
            TypecheckError::CannotAssignTo(source_id, span) => Report::new_diagnostic(
                Level::Error,
                span,
                source_id,
                "Cannot assign to",
                Some("Only identifiers may be assigned to"),
                [],
                [],
            ),
            TypecheckError::CannotCallNonIdent(source_id, span) => Report::new_diagnostic(
                Level::Error,
                span,
                source_id,
                "Cannot call expression",
                Some("Only identifiers may be called"),
                [],
                [],
            ),
            TypecheckError::CannotCallNonFunction(source_id, span) => Report::new_diagnostic(
                Level::Error,
                span,
                source_id,
                "Cannot call non function",
                Some("Only functions may be called"),
                [],
                [],
            ),
            TypecheckError::TooManyArguments {
                source_id,
                span,
                call_arguments_len: call_arguments,
                function_arguments_len: function_arguments,
            } => Report::new_diagnostic(
                Level::Error,
                span,
                source_id,
                "Too many arguments",
                Some(format!(
                    "Function expected {function_arguments} arguments, but was called with {call_arguments} arguments"
                )),
                [],
                [],
            ),
            TypecheckError::NotEnoughArguments {
                source_id,
                span,
                call_arguments_len,
                function_arguments_len,
            } => Report::new_diagnostic(
                Level::Error,
                span,
                source_id,
                "Not enough arguments",
                Some(format!(
                    "Function expected {function_arguments_len} arguments, but was called with {call_arguments_len} arguments"
                )),
                [],
                [],
            ),
        }
    }
}

pub fn check_types(
    report_ctx: &mut ReportContext,
    source_id: SourceId,
    _map: &SourceMap,
    ast: &Program<'_>,
    t_ir: &mut TirTree,
) {
    let mut rts = RecursiveTypechecker::new();

    for expr in &ast.exprs {
        let mut t_ir_node = TirNode::Uninitialized;

        let result = rts.visit_expr(source_id, expr, &mut t_ir_node);

        match result {
            Ok(_) => {}
            Err(error) => report_ctx.push(error.into_report()),
        };

        t_ir.nodes.push(match t_ir_node {
            TirNode::Uninitialized => TirNode::Poisoned,

            node => node,
        });
    }
}

struct RecursiveTypechecker<'ast> {
    scope_table: ScopeTable<'ast>,
}

impl<'ast> RecursiveTypechecker<'ast> {
    fn new() -> Self {
        Self {
            scope_table: vec![BTreeMap::new()],
        }
    }

    fn infer_type(
        &mut self,
        source_id: SourceId,
        expr: &Expr<'ast>,
    ) -> Result<Symbol<'ast>, TypecheckError<'ast>> {
        match expr {
            Expr::Int(Spanned(_, _)) => Ok(Symbol::Int),
            Expr::Float(Spanned(_, _)) => Ok(Symbol::Float),
            Expr::Ident(Spanned(span, ident)) => match self
                .scope_table
                .iter()
                .find(|scope| scope.contains_key(ident))
            {
                Some(scope) => Ok(scope[ident].clone()),
                None => Err(TypecheckError::SymbolNotFound(
                    source_id,
                    Spanned(span.clone(), ident),
                )),
            },
            Expr::String(Spanned(_, _)) => Ok(Symbol::String),
            Expr::BinaryOperation { lhs, operator, rhs } => match (
                self.infer_type(source_id, lhs)?,
                operator,
                self.infer_type(source_id, rhs)?,
            ) {
                (Symbol::Int, _, Symbol::Int) => Ok(Symbol::Int),
                (Symbol::Float, _, Symbol::Int) => Ok(Symbol::Float),
                (Symbol::Int, _, Symbol::Float) => Ok(Symbol::Float),
                (Symbol::Float, _, Symbol::Float) => Ok(Symbol::Float),

                _ => Err(TypecheckError::IncompatibleOperands(source_id, expr.span())),
            },
            Expr::Assign {
                lhs: _,
                eq_token: _,
                value: _,
            } => Ok(Symbol::Unit),
            Expr::Function {
                fn_token: _,
                name,
                lpt: _,
                arguments,
                rpt: _,
                return_type_token: _,
                return_type_annotation,
                equals: _,
                body: _,
            } => {
                if name.is_some() {
                    Ok(Symbol::Function {
                        arguments: arguments
                            .iter()
                            .map(|argument| (argument.name.1, argument.type_annotation.clone()))
                            .map(|(name, Spanned(_, type_annotation))| {
                                (name, Symbol::from(type_annotation))
                            })
                            .collect(),
                        return_type: Box::new(
                            return_type_annotation
                                .as_ref()
                                .map_or(Symbol::NotSpecified, |Spanned(_, type_annotation)| {
                                    Symbol::from(*type_annotation)
                                }),
                        ),
                    })
                } else {
                    Ok(Symbol::NotSpecified)
                }
            }
            Expr::Call {
                callee,
                lpt: _,
                arguments: _,
                rpt: _,
            } => match &**callee {
                Expr::Ident(Spanned(_, ident)) => {
                    match self
                        .scope_table
                        .iter()
                        .find(|scope| scope.contains_key(ident))
                    {
                        Some(scope) => {
                            if let Symbol::Function {
                                arguments: _,
                                return_type,
                            } = &scope[ident]
                            {
                                Ok((**return_type).clone())
                            } else {
                                Err(TypecheckError::CannotCallNonFunction(
                                    source_id,
                                    callee.span(),
                                ))
                            }
                        }
                        None => Err(TypecheckError::SymbolNotFound(
                            source_id,
                            Spanned(callee.span(), ident),
                        )),
                    }
                }

                _ => Err(TypecheckError::CannotCallNonIdent(source_id, callee.span())),
            },
            Expr::Poisoned => Ok(Symbol::NotSpecified),
        }
    }

    fn visit_expr(
        &mut self,
        source_id: SourceId,
        expr: &Expr<'ast>,
        t_ir_node: &mut TirNode,
    ) -> Result<(), TypecheckError<'ast>> {
        match expr {
            Expr::Int(Spanned(span, value)) => {
                *t_ir_node = TirNode::Int(Spanned(span.clone(), *value))
            }

            Expr::Float(Spanned(span, value)) => {
                *t_ir_node = TirNode::Float(Spanned(span.clone(), *value))
            }

            Expr::String(Spanned(span, value)) => {
                *t_ir_node = TirNode::String(Spanned(span.clone(), value.to_string()))
            }

            Expr::Ident(Spanned(span, ident)) => {
                if let Some(scope) = self
                    .scope_table
                    .iter()
                    .find(|scope| scope.contains_key(ident))
                {
                    let symbol = scope[ident].clone();

                    *t_ir_node = TirNode::Ident(
                        symbol.into(),
                        Spanned(span.clone(), ident).map(ToString::to_string),
                    );
                } else {
                    return Err(TypecheckError::SymbolNotFound(
                        source_id,
                        Spanned(span.clone(), ident),
                    ));
                }

                *t_ir_node = TirNode::Ident(
                    self.infer_type(source_id, expr)?.into(),
                    Spanned(span.clone(), ident.to_string()),
                )
            }
            Expr::BinaryOperation { lhs, operator, rhs } => {
                let (mut t_ir_lhs, mut t_ir_rhs) = (TirNode::Poisoned, TirNode::Poisoned);

                self.visit_expr(source_id, lhs, &mut t_ir_lhs)?;
                self.visit_expr(source_id, rhs, &mut t_ir_rhs)?;

                *t_ir_node = TirNode::BinaryOperation {
                    lhs: Box::new(t_ir_lhs),
                    operator: operator.clone(),
                    rhs: Box::new(t_ir_rhs),
                };
            }
            Expr::Assign {
                lhs,
                eq_token: _,
                value,
            } => match &**lhs {
                Expr::Ident(Spanned(_, ident)) => {
                    extern crate std;
                    let inferred_type = self.infer_type(source_id, value)?;

                    self.scope_table
                        .first_mut()
                        .unwrap()
                        .insert(ident, inferred_type);

                    let (mut t_ir_lhs, mut t_ir_value) = (TirNode::Poisoned, TirNode::Poisoned);

                    self.visit_expr(source_id, lhs, &mut t_ir_lhs)?;
                    self.visit_expr(source_id, value, &mut t_ir_value)?;

                    *t_ir_node = TirNode::Assign {
                        lhs: t_ir_lhs.try_into().expect(
                            "Guaranteed to be a assignable since the match above guards it",
                        ),
                        value: t_ir_value.into(),
                    };
                }

                _ => {
                    return Err(TypecheckError::CannotAssignTo(source_id, expr.span()));
                }
            },
            Expr::Function {
                fn_token: _,
                name,
                lpt: _,
                arguments,
                rpt: _,
                return_type_token: _,
                return_type_annotation,
                equals: _,
                body,
            } => {
                let mut t_ir_arguments = BTreeMap::new();

                for (name, argument_type) in arguments.iter().map(|argument| {
                    (
                        argument.name.1,
                        Spanned(
                            argument.type_annotation.span(),
                            Symbol::from(argument.type_annotation.1),
                        ),
                    )
                }) {
                    match argument_type {
                        Spanned(span, Symbol::Other(identifier)) => {
                            if !self
                                .scope_table
                                .iter()
                                .any(|scope| scope.contains_key(identifier))
                            {
                                return Err(TypecheckError::SymbolNotFound(
                                    source_id,
                                    Spanned(span, identifier),
                                ));
                            }
                        }

                        _ => (),
                    }

                    t_ir_arguments.insert(name.to_string(), argument_type.1.into());
                }

                match return_type_annotation {
                    Some(Spanned(span, TypeAnnotation::Object(object))) => {
                        if !self
                            .scope_table
                            .iter()
                            .any(|scope| scope.contains_key(object))
                        {
                            return Err(TypecheckError::SymbolNotFound(
                                source_id,
                                Spanned(span.clone(), object),
                            ));
                        }
                    }
                    _ => (),
                }

                if let Some(Spanned(_, name)) = name {
                    let fn_symbol = self.infer_type(source_id, expr)?;
                    self.scope_table
                        .first_mut()
                        .unwrap()
                        .insert(name, fn_symbol);
                }

                let mut t_ir_body = TirNode::Poisoned;

                self.scope_table.first_mut().unwrap().extend(
                    arguments
                        .iter()
                        .map(|arg| (arg.name.1, arg.type_annotation.1.into())),
                );

                self.visit_expr(source_id, body, &mut t_ir_body)?;

                let function_return_type_symbol = return_type_annotation
                    .as_ref()
                    .map(|Spanned(_, type_annotation)| *type_annotation)
                    .unwrap_or(TypeAnnotation::Unit)
                    .into();

                if self.infer_type(source_id, body)? != function_return_type_symbol {
                    return Err(TypecheckError::TypeMismatch {
                        source_id,
                        span: return_type_annotation
                            .as_ref()
                            .map(|Spanned(span, _)| span.clone())
                            .unwrap_or(body.span()),
                        expected: function_return_type_symbol,
                        found: self.infer_type(source_id, body)?,
                    });
                }

                *t_ir_node = TirNode::Function {
                    name: name
                        .as_ref()
                        .cloned()
                        .map(|spanned_name| spanned_name.map(ToString::to_string)),
                    arguments: t_ir_arguments,
                    return_type: return_type_annotation
                        .as_ref()
                        .cloned()
                        .map(|Spanned(_, annotation)| annotation)
                        .unwrap_or(TypeAnnotation::NotSpecified)
                        .into(),
                    body: Box::new(t_ir_body),
                }
            }
            Expr::Call {
                callee,
                lpt,
                arguments: call_arguments,
                rpt,
            } => {
                match &**callee {
                    Expr::Ident(Spanned(callee_span, ident)) => {
                        if !self
                            .scope_table
                            .iter()
                            .any(|scope| scope.contains_key(ident))
                        {
                            return Err(TypecheckError::SymbolNotFound(
                                source_id,
                                Spanned(callee_span.clone(), ident),
                            ));
                        }

                        let mut t_ir_call_arguments = vec![];

                        let function_type = self
                            .scope_table
                            .iter_mut()
                            .find(|scope| scope.contains_key(ident))
                            .unwrap()[ident]
                            .clone();

                        let call_arguments_span = (lpt.0.start)..(rpt.0.end);

                        match &function_type {
                            Symbol::Function {
                                arguments: function_arguments,
                                return_type: _,
                            } => {
                                let call_arguments_len = call_arguments.len();
                                let function_arguments_len = function_arguments.len();

                                match call_arguments_len.cmp(&function_arguments_len) {
                                    // The call has more arguments than the function expects
                                    Greater => {
                                        return Err(TypecheckError::TooManyArguments {
                                            source_id,
                                            span: call_arguments_span,
                                            call_arguments_len,
                                            function_arguments_len,
                                        })
                                    }
                                    // The call has less arguments than the function expects
                                    Less => {
                                        return Err(TypecheckError::NotEnoughArguments {
                                            source_id,
                                            span: call_arguments_span,
                                            call_arguments_len,
                                            function_arguments_len,
                                        })
                                    }
                                    // The call has the right amount of arguments
                                    Equal => (),
                                }

                                for argument in call_arguments {
                                    let mut t_ir_node = TirNode::Poisoned;

                                    self.visit_expr(source_id, argument, &mut t_ir_node)?;

                                    t_ir_call_arguments.push(t_ir_node);
                                }

                                let call_arguments_types = call_arguments
                                    .iter()
                                    .map(|argument| {
                                        self.infer_type(source_id, argument).map(
                                            |inferred_argument| {
                                                Spanned(argument.span(), inferred_argument)
                                            },
                                        )
                                    })
                                    .collect::<Result<Vec<_>, _>>()?;

                                for (
                                    Spanned(call_argument_span, call_argument_type),
                                    function_argument_type,
                                ) in Iterator::zip(
                                    call_arguments_types.into_iter(),
                                    function_arguments.values(),
                                ) {
                                    if !(call_argument_type == *function_argument_type) {
                                        return Err(TypecheckError::TypeMismatch {
                                            source_id,
                                            span: call_argument_span,
                                            expected: call_argument_type,
                                            found: function_argument_type.clone(),
                                        });
                                    }
                                }

                                *t_ir_node = TirNode::Call {
                                    callee: Spanned(callee_span.clone(), function_type.into()),
                                    arguments: t_ir_call_arguments,
                                    return_type: self.infer_type(source_id, expr)?.into(),
                                }
                            }

                            _ => {
                                return Err(TypecheckError::CannotCallNonFunction(
                                    source_id,
                                    callee_span.clone(),
                                ))
                            }
                        }
                    }

                    _ => (),
                }
            }

            Expr::Poisoned => unreachable!(),
        };

        Ok(())
    }
}
