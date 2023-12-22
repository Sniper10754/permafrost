use core::{
    cmp::Ordering::{Equal, Greater, Less},
    ops::Range,
};

use alloc::{boxed::Box, collections::BTreeMap, format, vec, vec::Vec};
use frostbite_parser::ast::{tokens::TypeAnnotation, Expr, Program, Spannable, Spanned};
use frostbite_reports::{
    sourcemap::{SourceId, SourceMap},
    IntoReport, Level, Report,
};

use crate::SemanticCheck;

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

    #[display(fmt = "class {}", "_0")]
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
            TypeAnnotation::Other(other) => Self::Other(other),
        }
    }
}

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
    type Arguments = ();

    fn into_report(self, _: Self::Arguments) -> Report {
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
                Some(format!("Expected type {}, found type {}", expected, found)),
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
                    "Function expected {} arguments, but was called with {} arguments",
                    function_arguments, call_arguments
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
                    "Function expected {} arguments, but was called with {} arguments",
                    function_arguments_len, call_arguments_len
                )),
                [],
                [],
            ),
        }
    }
}

pub struct Typecheck;

impl SemanticCheck for Typecheck {
    type Output = ();
    type Error<'ast> = TypecheckError<'ast>;

    fn check<'ast>(
        source_id: SourceId,
        // Will use in import resolution
        _map: &SourceMap,
        ast: &Program<'ast>,
    ) -> Result<Self::Output, Vec<Self::Error<'ast>>> {
        let mut rts = RecursiveTypechecker::new();
        let mut errors = vec![];

        for expr in ast.exprs.iter() {
            match rts.visit_expr(source_id, expr) {
                Ok(_) => (),
                Err(err) => errors.push(err),
            }
        }

        match errors.is_empty() {
            true => Ok(()),
            false => Err(errors),
        }
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
            Expr::Int(_, _) => Ok(Symbol::Int),
            Expr::Float(_, _) => Ok(Symbol::Float),
            Expr::Ident(span, ident) => match self
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
            Expr::String(_, _) => Ok(Symbol::String),
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
                                .map(|Spanned(_, type_annotation)| Symbol::from(*type_annotation))
                                .unwrap_or(Symbol::NotSpecified),
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
                Expr::Ident(_, ident) => {
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
            Expr::Poisoned => todo!(),
        }
    }

    fn visit_expr(
        &mut self,
        source_id: SourceId,
        expr: &Expr<'ast>,
    ) -> Result<(), TypecheckError<'ast>> {
        match expr {
            Expr::Ident(span, ident) => {
                if !self
                    .scope_table
                    .iter()
                    .any(|scope| scope.contains_key(ident))
                {
                    return Err(TypecheckError::SymbolNotFound(
                        source_id,
                        Spanned(span.clone(), ident),
                    ));
                }
            }
            Expr::BinaryOperation {
                lhs,
                operator: _,
                rhs,
            } => {
                self.visit_expr(source_id, lhs)?;
                self.visit_expr(source_id, rhs)?;

                // Check operators by inferring types
                let _ = self.infer_type(source_id, expr)?;
            }
            Expr::Assign {
                lhs,
                eq_token: _,
                value,
            } => match &**lhs {
                Expr::Ident(_, ident) => {
                    let inferred_type = self.infer_type(source_id, value)?;

                    self.scope_table
                        .first_mut()
                        .unwrap()
                        .insert(ident, inferred_type);
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
                return_type_annotation: _,
                equals: _,
                body,
            } => {
                self.visit_expr(source_id, body)?;

                for argument_type in arguments.iter().map(|argument| {
                    Spanned(
                        argument.type_annotation.span(),
                        Symbol::from(argument.type_annotation.1),
                    )
                }) {
                    #[allow(clippy::single_match)]
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
                }

                if let Some(Spanned(_, name)) = name {
                    let fn_symbol = self.infer_type(source_id, expr)?;
                    self.scope_table
                        .first_mut()
                        .unwrap()
                        .insert(name, fn_symbol);
                }
            }
            Expr::Call {
                callee,
                lpt,
                arguments: call_arguments,
                rpt,
            } => {
                #[allow(clippy::single_match)]
                match &**callee {
                    Expr::Ident(span, ident) => {
                        if !self
                            .scope_table
                            .iter()
                            .any(|scope| scope.contains_key(ident))
                        {
                            return Err(TypecheckError::SymbolNotFound(
                                source_id,
                                Spanned(span.clone(), ident),
                            ));
                        }

                        let function_type = self
                            .scope_table
                            .iter_mut()
                            .find(|scope| scope.contains_key(ident))
                            .unwrap()[ident]
                            .clone();

                        let call_arguments_span = (lpt.0.start)..(rpt.0.end);

                        match function_type {
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
                            }

                            _ => {
                                return Err(TypecheckError::CannotCallNonFunction(
                                    source_id,
                                    span.clone(),
                                ))
                            }
                        }
                    }

                    _ => {}
                }
            }

            _ => (),
        };

        Ok(())
    }
}
