use alloc::{borrow::Cow, boxed::Box, format, string::String, vec::Vec};
use frostbite_ast::{
    tokens::{
        ArrowToken, FunctionToken, LeftBraceToken, LeftParenthesisToken, ModToken, ReturnToken,
        RightBraceToken, RightParenthesisToken, TypeAnnotation,
    },
    Expr, ImportDirectiveKind, ModuleDirectiveKind, Span, Spannable, Spanned,
};
use frostbite_reports::{
    sourcemap::{SourceKey, SourceUrl},
    IntoReport, Level, Report,
};

use crate::{
    context::names::NamedModuleKey,
    ir::named::{Argument, Assignable, LocalKey, NamedAst, NamedExpr},
    utils::Scopes,
    Compiler, FROSTBITE_FILE_EXTENSION,
};

#[derive(derive_more::From)]
enum NameResolutionError
{
    CannotAssignTo
    {
        source_key: SourceKey, span: Span
    },

    IdentifierNotFound
    {
        source_key: SourceKey,
        identifier: Spanned<String>,
    },

    ClashingIdentifier
    {
        source_key: SourceKey,
        identifier: Spanned<String>,
    },

    #[cfg(not(feature = "std"))]
    ModuleStatementNotSupported(SourceKey, Span),

    #[cfg(feature = "std")]
    #[from]
    IoError(std::io::Error, SourceKey, Span),
}

impl IntoReport for NameResolutionError
{
    fn into_report(self) -> Report
    {
        match self {
            NameResolutionError::CannotAssignTo { source_key, span } => Report::new(
                Level::Error,
                span,
                source_key,
                "Cannot assign to non-ident",
                Some("You may only assign to identifiers"),
            ),
            NameResolutionError::IdentifierNotFound {
                source_key,
                identifier: Spanned(span, identifier),
            } => Report::new(
                Level::Error,
                span,
                source_key,
                "Identifier not found",
                Some(format!("identifier `{identifier}` not found")),
            ),
            NameResolutionError::ClashingIdentifier {
                source_key,
                identifier: Spanned(span, identifier),
            } => Report::new(
                Level::Error,
                span,
                source_key,
                "Identifier already exists",
                Some(format!("`{identifier}` is already defined elsewhere"))
            ),
            #[cfg(not(feature = "std"))]
            NameResolutionError::ModuleStatementNotSupported(source_key, span) => {
                Report::new(Level::Error, span, source_key, "Mod statement not supported", Some("The mod statement requires the std file api: youre on a platform which doesnt support std."), [], [])
            }
            NameResolutionError::IoError(error, source_key,  span) => Report::new(Level::Error, span, source_key, "Io Error", Some(format!("{error}")))
        }
    }
}

pub fn check_names(
    compiler: &mut Compiler,
    source_key: SourceKey,
)
{
    compiler
        .ctx
        .named_ctx
        .named_asts
        .insert(source_key, NamedAst::default());

    let mut rnc = RecursiveNameChecker {
        compiler,
        scopes: Scopes::new(),
    };

    let ast = rnc.compiler.ctx.asts[source_key].exprs.clone().into_iter();

    for ref expr in ast {
        let result = rnc.visit_expr(source_key, expr);

        match result {
            Ok(named_expr) => rnc.compiler.ctx.named_ctx.named_asts[source_key]
                .exprs
                .push(named_expr),
            Err(error) => rnc.compiler.ctx.report_ctx.push(error.into_report()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum LocalKind
{
    Local(LocalKey),
    Module(NamedModuleKey),
}

pub struct RecursiveNameChecker<'compiler>
{
    compiler: &'compiler mut Compiler,
    scopes: Scopes<LocalKind>,
}

impl<'compiler> RecursiveNameChecker<'compiler>
{
    fn visit_expr(
        &mut self,
        source_key: SourceKey,
        expr: &Expr,
    ) -> Result<NamedExpr, NameResolutionError>
    {
        let old_len = self.scopes.len();

        let expr = match expr {
            Expr::Int(value) => Ok(NamedExpr::Int(value.clone())),
            Expr::Float(value) => Ok(NamedExpr::Float(value.clone())),
            Expr::Bool(value) => Ok(NamedExpr::Bool(value.clone())),
            Expr::String(value) => Ok(NamedExpr::String(value.clone())),
            Expr::Ident(identifier) => {
                self.visit_ident(source_key, identifier.value(), identifier.span())
            }
            Expr::ImportDirective(import_directive) => {
                self.visit_import_directive(source_key, import_directive.as_ref())
            }
            Expr::ModuleDirective(mod_token, module_directive_kind) => {
                self.visit_module_directive(source_key, mod_token.clone(), module_directive_kind)
            }
            Expr::BinaryOperation { lhs, operator, rhs } => Ok(NamedExpr::BinaryOperation {
                lhs: Box::new(self.visit_expr(source_key, lhs)?),
                operator: operator.clone(),
                rhs: Box::new(self.visit_expr(source_key, rhs)?),
            }),
            Expr::Assign {
                lhs,
                eq_token: _,
                value,
            } => self.visit_assign(source_key, lhs, value),
            Expr::Function {
                fn_token,
                name,
                lpt: _,
                arguments,
                rpt: _,
                return_type_token,
                return_type_annotation,
                equals: _,
                body,
            } => self.visit_function(
                source_key,
                fn_token.clone(),
                name.as_ref().map(|name| name.as_deref()),
                arguments,
                return_type_token.clone(),
                return_type_annotation.clone(),
                body,
            ),
            Expr::Call {
                callee,
                left_paren,
                arguments,
                right_paren,
            } => self.visit_call(
                source_key,
                callee,
                left_paren.clone(),
                arguments,
                right_paren.clone(),
            ),
            Expr::Block {
                left_brace,
                expressions,
                right_brace,
            } => self.visit_block(
                source_key,
                left_brace.clone(),
                expressions,
                right_brace.clone(),
            ),
            Expr::Return(return_token, return_value) => {
                self.visit_return(source_key, return_token.clone(), return_value.as_deref())
            }

            Expr::Poisoned => unreachable!(),
        };

        self.scopes.truncate(old_len);

        expr
    }

    fn visit_ident(
        &mut self,
        source_key: SourceKey,
        identifier: &str,
        span: Span,
    ) -> Result<NamedExpr, NameResolutionError>
    {
        let Some(LocalKind::Local(local_key)) = self.scopes.local(identifier).copied() else {
            return Err(NameResolutionError::IdentifierNotFound {
                source_key,
                identifier: Spanned(span, identifier.into()),
            });
        };

        Ok(NamedExpr::Ident {
            local_key,
            identifier: Spanned(span, identifier.into()),
        })
    }

    fn visit_import_directive(
        &mut self,
        _source_key: SourceKey,
        _import_directive_kind: Spanned<&ImportDirectiveKind>,
    ) -> Result<NamedExpr, NameResolutionError>
    {
        todo!()
    }

    fn visit_module_directive(
        &mut self,
        source_key: SourceKey,
        module_token: ModToken,
        module_directive_kind: &ModuleDirectiveKind,
    ) -> Result<NamedExpr, NameResolutionError>
    {
        #[cfg(feature = "std")]
        {
            use std::path::Path;

            let filepath: Cow<'_, Path> = match self.compiler.ctx.src_map[source_key].url {
                SourceUrl::PathBuf(ref pathbuf) => pathbuf.parent().unwrap().into(),
                SourceUrl::Sparse(..) | SourceUrl::Anonymous => {
                    std::env::current_dir().unwrap().into()
                }
            };

            match module_directive_kind {
                ModuleDirectiveKind::ImportLocalModule(local_module_name) => {
                    let module_path = filepath.join(format!(
                        "{}.{FROSTBITE_FILE_EXTENSION}",
                        local_module_name.value()
                    ));

                    let source_code =
                        std::fs::read_to_string(module_path.as_path()).map_err(|err| {
                            NameResolutionError::IoError(err, source_key, module_token.span())
                        })?;

                    let module_source_key = self
                        .compiler
                        .add_source(SourceUrl::PathBuf(module_path), source_code);

                    _ = self.compiler.analyze_module(module_source_key);

                    let module_key =
                        self.compiler.ctx.named_ctx.modules_by_src_keys[module_source_key];

                    self.scopes
                        .try_insert(local_module_name.value(), LocalKind::Module(module_key))
                        .map_err(|_| NameResolutionError::ClashingIdentifier {
                            source_key,
                            identifier: local_module_name.clone(),
                        })?;
                }
            }
        }

        #[cfg(not(feature = "std"))]
        {
            return Err(NameResolutionError::ModuleStatementNotSupported(
                source_key,
                module_token.span(),
            ));
        }

        Ok(NamedExpr::ModuleStatement(module_directive_kind.span()))
    }

    fn visit_assign(
        &mut self,
        source_key: SourceKey,
        lhs: &Expr,
        value: &Expr,
    ) -> Result<NamedExpr, NameResolutionError>
    {
        let body = Box::new(self.visit_expr(source_key, value)?);

        let lhs = match lhs {
            Expr::Poisoned => unreachable!(),

            Expr::Ident(spanned_name) => {
                let local_key = self.insert_forced(source_key, spanned_name.value());

                Assignable::Ident(local_key, spanned_name.clone())
            }

            _ => {
                return Err(NameResolutionError::CannotAssignTo {
                    source_key,
                    span: lhs.span(),
                })
            }
        };

        Ok(NamedExpr::Assign { lhs, body })
    }

    fn visit_function(
        &mut self,
        source_key: SourceKey,
        fn_token: FunctionToken,
        name: Option<Spanned<&str>>,
        arguments: &[frostbite_ast::Argument],
        return_type_token: Option<ArrowToken>,
        return_type_annotation: Option<Spanned<TypeAnnotation>>,
        body: &Expr,
    ) -> Result<NamedExpr, NameResolutionError>
    {
        let local_key = if let Some(ref spanned_name) = name {
            self.insert_clashing(source_key, spanned_name)
        } else {
            None
        };

        self.scopes.enter_scope();

        let arguments = arguments
            .iter()
            .cloned()
            .map(
                |frostbite_ast::Argument {
                     name,
                     type_annotation,
                 }| {
                    Argument {
                        local_key: self.insert_forced(source_key, name.value()),
                        name,
                        type_annotation,
                    }
                },
            )
            .collect::<Vec<_>>();

        arguments.iter().for_each(
            |Argument {
                 local_key,
                 name: spanned_name,
                 type_annotation: _,
             }| {
                self.scopes
                    .insert_forced(spanned_name.value(), LocalKind::Local(*local_key));
            },
        );

        let body = self.visit_expr(source_key, body)?;

        self.scopes.leave_scope();

        Ok(NamedExpr::Function {
            local_key,
            fn_token,
            name: name.map(|name| name.map(|name| name.into())),
            arguments,
            return_type_token,
            return_type_annotation,
            body: Box::new(body),
        })
    }

    fn visit_call(
        &mut self,
        source_key: SourceKey,
        callee: &Expr,
        left_paren: LeftParenthesisToken,
        arguments: &[Expr],
        right_paren: RightParenthesisToken,
    ) -> Result<NamedExpr, NameResolutionError>
    {
        Ok(NamedExpr::Call {
            callee: Box::new(self.visit_expr(source_key, callee)?),
            left_paren,
            arguments: arguments
                .iter()
                .map(|argument| self.visit_expr(source_key, argument))
                .collect::<Result<Vec<_>, _>>()?,
            right_paren,
        })
    }

    fn visit_block(
        &mut self,
        source_key: SourceKey,
        left_brace: LeftBraceToken,
        expressions: &[Expr],
        right_brace: RightBraceToken,
    ) -> Result<NamedExpr, NameResolutionError>
    {
        Ok(NamedExpr::Block {
            left_brace,
            expressions: expressions
                .iter()
                .map(|expr| self.visit_expr(source_key, expr))
                .collect::<Result<Vec<_>, _>>()?,
            right_brace,
        })
    }

    fn visit_return(
        &mut self,
        source_key: SourceKey,
        return_token: ReturnToken,
        return_value: Option<&Expr>,
    ) -> Result<NamedExpr, NameResolutionError>
    {
        Ok(NamedExpr::Return(
            return_token,
            return_value
                .map(|expr| self.visit_expr(source_key, expr))
                .transpose()?
                .map(Box::new),
        ))
    }

    fn insert_clashing(
        &mut self,
        source_key: SourceKey,
        name: &str,
    ) -> Option<LocalKey>
    {
        if self.scopes.local(name).is_none() {
            let local_key = self.compiler.ctx.named_ctx.named_asts[source_key]
                .locals
                .insert(());

            self.scopes
                .try_insert(name, LocalKind::Local(local_key))
                .unwrap();

            Some(local_key)
        } else {
            None
        }
    }

    fn insert_forced(
        &mut self,
        source_key: SourceKey,
        name: &str,
    ) -> LocalKey
    {
        let local_key = self.compiler.ctx.named_ctx.named_asts[source_key]
            .locals
            .insert(());

        self.scopes.insert_forced(name, LocalKind::Local(local_key));

        local_key
    }
}
