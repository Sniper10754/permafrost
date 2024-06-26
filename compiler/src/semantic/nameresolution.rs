use alloc::{borrow::Cow, boxed::Box, format, string::String, vec::Vec};
use derive_more::*;

use permafrost_ast::{
    tokens::{
        ArrowToken, FunctionToken, LeftBraceToken, LeftParenthesisToken, ModToken, ReturnToken,
        RightBraceToken, RightParenthesisToken, TypeAnnotation,
    },
    Expr, ItemVisibility, NamespaceDirectiveKind, NamespacePath, Span, Spannable, Spanned,
};
use permafrost_reports::{
    sourcemap::{SourceKey, SourceUrl},
    IntoReport, Label, Level, Report,
};

use crate::{
    context::names::{Item, NamespaceKey},
    ir::named::{Argument, Assignable, LocalKey, NamedExpr, ResolvedSymbol},
    utils::Scopes,
    Compiler, PERMAFROST_FILE_EXTENSION,
};

#[derive(Debug, From)]
enum MaybePoisoned<T>
{
    #[from(ignore)]
    Poisoned,
    Value(T),
}

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

    PublicItemButNoIdentifier
    {
        source_key: SourceKey,
        public_token_span: Span,
    },

    ImportFromNonNamespace
    {
        source_key: SourceKey,
        segment_span: Span,
    },

    #[cfg(not(feature = "std"))]
    NamespaceStatementNotSupported(SourceKey, Span),

    #[cfg(feature = "std")]
    CouldNotReadNamespaceFile
    {
        error: std::io::Error,
        file_path: std::path::PathBuf,
        source_key: SourceKey,
        span: Span,
    },
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
            NameResolutionError::PublicItemButNoIdentifier {
                source_key,
                public_token_span
            } => Report::new(
                Level::Error,
                public_token_span,
                source_key,
                "Item is public but has no way to be identified",
                Some("This item is exported, but cant be identified in any way since its missing an identifier")
            ),
            NameResolutionError::ImportFromNonNamespace { source_key, segment_span } => Report::new(
                Level::Error, 
                segment_span, 
                source_key, 
                "Trying to import from a non-namespace", 
                Some("This segment of the import is not a namespace: this means that it cant contain other items.")
            ),
            #[cfg(not(feature = "std"))]
            NameResolutionError::NamespaceStatementNotSupported(source_key, span) => {
                Report::new(Level::Error, span, source_key, "Mod statement not supported", Some("The mod statement requires the std file api: your on a platform which doesnt support std."))
            }
            #[cfg(feature = "std")]
            NameResolutionError::CouldNotReadNamespaceFile { error, source_key,  span, file_path } => Report::new(Level::Error, span, source_key, "Input/Output OS Error", Some(format!("{error}"))).with_label(Label::new(format!("Could not read path `{}`", file_path.display()), None, source_key))
        }
    }
}

pub fn check_names(
    compiler: &mut Compiler<'_>,
    src_key: SourceKey,
)
{
    let named_ast = compiler.ctx.named_ctx.new_ast();

    compiler.ctx.named_ctx.insert_ast(src_key, named_ast);

    let mut resolver = NameResolver {
        compiler,
        scopes: Scopes::new(),
    };

    let ast = resolver.compiler.ctx.asts[src_key]
        .exprs
        .clone()
        .into_iter();

    for ref expr in ast {
        let result = resolver.visit_expr(src_key, expr);

        match result {
            Ok(named_expr) => resolver
                .compiler
                .ctx
                .named_ctx
                .get_ast_mut(src_key)
                .exprs
                .push(named_expr),
            Err(error) => resolver.compiler.ctx.report_ctx.push(error.into_report()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, From)]
enum LocalSymbol
{
    Local(LocalKey),
    Namespace(SourceKey, NamespaceKey),
}

impl From<ResolvedSymbol> for LocalSymbol
{
    fn from(value: ResolvedSymbol) -> Self
    {
        match value {
            ResolvedSymbol::LocalKey(local) => Self::from(local),
            ResolvedSymbol::NamespaceKey(source_key, namespace) => Self::from((source_key, namespace)),
        }
    }
}

#[derive(Debug)]
pub struct NameResolver<'compiler, 'ctx>
{
    compiler: &'compiler mut Compiler<'ctx>,
    scopes: Scopes<LocalSymbol>,
}

impl<'compiler, 'ctx> NameResolver<'compiler, 'ctx>
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
            Expr::ImportDirective { namespace_path } => {
                self.visit_import_directive(source_key, namespace_path.as_deref())
            }
            Expr::NamespaceDirective(mod_token, namespace_directive_kind) => self
                .visit_namespace_directive(source_key, mod_token.clone(), namespace_directive_kind),
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
                visibility,
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
                visibility,
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
        let Some(LocalSymbol::Local(local_key)) = self.scopes.local(identifier).copied() else {
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
        source_key: SourceKey,
        namespace_path: Spanned<&NamespacePath>,
    ) -> Result<NamedExpr, NameResolutionError>
    {
        let (
            // The source key of the file the import statement imports from
            imported_from_source_key,
            // The namespace key which contains the symbol
            imported_from_namespace_key,
            // Symbol who was imported
            resolved_symbol,
        ) = self.resolve_namespace_path(source_key, namespace_path.value())?;

        let last_path_element = namespace_path.last().unwrap();

        let local_key = self
            .insert_clashing(source_key, last_path_element.value())
            .ok_or_else(|| NameResolutionError::ClashingIdentifier {
                source_key,
                identifier: last_path_element.clone(),
            })?;

        Ok(NamedExpr::UseDirective {
            span: namespace_path.span(),

            local_key,

            imported_name: last_path_element.clone(),
            imported_from: (imported_from_source_key, imported_from_namespace_key),
            symbol_imported: resolved_symbol,
        })
    }

    fn resolve_namespace_path(
        &mut self,
        importing_file_source_key: SourceKey,
        namespace_path: &NamespacePath,
    ) -> Result<(SourceKey, NamespaceKey, ResolvedSymbol), NameResolutionError>
    {
        let namespace_to_import_from = &namespace_path[0];

        // Search the current scope for the module
        let (namespace_source_key_to_import_from, namespace_key_to_import_from) = if let Some(LocalSymbol::Namespace(source_key, namespace_key)) =
            self.scopes.local(namespace_to_import_from.value())
        {
            (*source_key, *namespace_key)
        } else {
            return Err(NameResolutionError::IdentifierNotFound {
                source_key: importing_file_source_key,
                identifier: namespace_to_import_from.clone(),
            });
        };

        if namespace_path.len() > 1 {
            Ok((
                namespace_source_key_to_import_from,
                namespace_key_to_import_from,
                self.resolve_namespace_path_import(
                    importing_file_source_key,
                    namespace_key_to_import_from,
                    &namespace_path[1..],
                )?,
            ))
        } else {
            Ok((
                namespace_source_key_to_import_from,
                namespace_key_to_import_from,
                ResolvedSymbol::NamespaceKey(namespace_source_key_to_import_from, namespace_key_to_import_from),
            ))
        }
    }

    fn resolve_namespace_path_import(
        &mut self,
        source_key: SourceKey,
        namespace_key: NamespaceKey,
        namespace_path: &NamespacePath,
    ) -> Result<ResolvedSymbol, NameResolutionError>
    {
        let mut namespace_path_iter = namespace_path.iter().peekable();

        let segment = namespace_path_iter.next().unwrap();

        let resolved_symbol =
            self.resolve_namespace_path_segment(source_key, namespace_key, segment.as_deref())?;

        match (resolved_symbol, namespace_path_iter.peek()) {
            (ResolvedSymbol::LocalKey(..), Some(segment)) => {
                Err(NameResolutionError::ImportFromNonNamespace {
                    source_key,
                    segment_span: segment.span(),
                })
            }
            (ResolvedSymbol::LocalKey(..) | ResolvedSymbol::NamespaceKey(..), None) => {
                Ok(resolved_symbol)
            }
            (ResolvedSymbol::NamespaceKey(_, new_namespace_key), Some(..)) => self
                .resolve_namespace_path_import(source_key, new_namespace_key, &namespace_path[1..]),
        }
    }

    fn resolve_namespace_path_segment(
        &mut self,
        source_key: SourceKey,
        namespace_key: NamespaceKey,
        identifier: Spanned<&str>,
    ) -> Result<ResolvedSymbol, NameResolutionError>
    {
        let namespace = self.compiler.ctx.named_ctx.get_namespace(namespace_key);

        match namespace.exported_locals.get(identifier.value_copied()) {
            Some(&item) => Ok(ResolvedSymbol::from(item)),
            None => Err(NameResolutionError::IdentifierNotFound {
                source_key,
                // Spanned<&str> -> Spanned<String>
                identifier: identifier.map(Into::into),
            }),
        }
    }

    fn visit_namespace_directive(
        &mut self,
        source_key: SourceKey,
        namespace_token: ModToken,
        namespace_directive_kind: &NamespaceDirectiveKind,
    ) -> Result<NamedExpr, NameResolutionError>
    {
        #[cfg(feature = "std")]
        {
            let (local_key, namespace_key) = match self.visit_namespace_directive_std(
                source_key,
                namespace_token,
                namespace_directive_kind,
            ) {
                Ok(keys) => keys,
                Err(MaybePoisoned::Poisoned) => return Ok(NamedExpr::Poisoned),
                Err(MaybePoisoned::Value(error)) => return Err(error),
            };

            Ok(NamedExpr::NamespaceDirective {
                span: namespace_directive_kind.span(),
                local_key,
                namespace_key,
            })
        }

        #[cfg(not(feature = "std"))]
        {
            return Err(NameResolutionError::NamespaceStatementNotSupported(
                source_key,
                namespace_token.span(),
            ));
        }
    }

    #[cfg(feature = "std")]
    fn visit_namespace_directive_std(
        &mut self,
        source_key: SourceKey,
        namespace_token: ModToken,
        namespace_directive_kind: &NamespaceDirectiveKind,
    ) -> Result<(LocalKey, NamespaceKey), MaybePoisoned<NameResolutionError>>
    {
        match namespace_directive_kind {
            NamespaceDirectiveKind::ImportLocalNamespace(local_namespace_name) => self
                .import_local_namespace(
                    source_key,
                    namespace_token.clone(),
                    local_namespace_name.as_deref(),
                ),
        }
    }

    #[cfg(feature = "std")]
    fn import_local_namespace(
        &mut self,
        importing_file_source_key: SourceKey,
        module_token: ModToken,
        local_namespace_name: Spanned<&str>,
    ) -> Result<(LocalKey, NamespaceKey), MaybePoisoned<NameResolutionError>>
    {
        let parent_dir: Cow<'_, std::path::Path> = match self.compiler.ctx.src_map
            [importing_file_source_key]
            .url
        {
            SourceUrl::PathBuf(ref pathbuf) => pathbuf.parent().unwrap().into(),
            SourceUrl::Sparse(..) | SourceUrl::Anonymous => std::env::current_dir().unwrap().into(),
        };

        let namespace_path = parent_dir.join(format!(
            "{}.{PERMAFROST_FILE_EXTENSION}",
            local_namespace_name.value()
        ));

        let source_code = std::fs::read_to_string(namespace_path.as_path()).map_err(|error| {
            NameResolutionError::CouldNotReadNamespaceFile {
                error,
                source_key: importing_file_source_key,
                span: module_token.span(),
                file_path: namespace_path.clone(),
            }
        })?;

        if let Ok(namespace_source_key) = self
            .compiler
            .add_source(SourceUrl::PathBuf(namespace_path), source_code)
        {
            let namespace_key = self
                .compiler
                .ctx
                .named_ctx
                .get_ast(namespace_source_key)
                .root_namespace;

            let local_key = self
                .compiler
                .ctx
                .named_ctx
                .root_namespace_of_ast_mut(importing_file_source_key)
                .locals
                .insert(());

            self.scopes
                .try_insert(
                    local_namespace_name.value_copied(),
                    LocalSymbol::Namespace(namespace_source_key, namespace_key),
                )
                .map_err(|_| NameResolutionError::ClashingIdentifier {
                    source_key: importing_file_source_key,
                    identifier: local_namespace_name.map(Into::into),
                })?;

            Ok((local_key, namespace_key))
        } else {
            // Since analyzing the Namespace failed, we cant import it as it may contain false results
            // We can just avoid putting it into scope.

            Err(MaybePoisoned::Poisoned)
        }
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
        visibility: &ItemVisibility,
        fn_token: FunctionToken,
        name: Option<Spanned<&str>>,
        arguments: &[permafrost_ast::Argument],
        return_type_token: Option<ArrowToken>,
        return_type_annotation: Option<Spanned<TypeAnnotation>>,
        body: &Expr,
    ) -> Result<NamedExpr, NameResolutionError>
    {
        let local_key = match name {
            Some(ref spanned_name) => self.insert_clashing(source_key, spanned_name),
            None => None,
        };

        match (&name, visibility) {
            (Some(Spanned(_, name)), ItemVisibility::Public(_)) => {
                let namespace_key = self
                    .compiler
                    .ctx
                    .named_ctx
                    .get_ast(source_key)
                    .root_namespace;

                let namespace = self.compiler.ctx.named_ctx.get_namespace_mut(namespace_key);

                namespace.exported_locals.insert(
                    String::from(*name),
                    Item::Local(local_key.expect("This local is Some in case the name is Some")),
                );
            }
            (None, ItemVisibility::Public(token)) => {
                return Err(NameResolutionError::PublicItemButNoIdentifier {
                    source_key,
                    public_token_span: token.span(),
                })
            }
            (_, ItemVisibility::Unspecified) => (),
        }

        self.scopes.enter_scope();

        let arguments = arguments
            .iter()
            .cloned()
            .map(
                |permafrost_ast::Argument {
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
                    .insert_forced(spanned_name.value(), LocalSymbol::Local(*local_key));
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
            let local_key = self
                .compiler
                .ctx
                .named_ctx
                .root_namespace_of_ast_mut(source_key)
                .locals
                .insert(());

            self.scopes
                .try_insert(name, LocalSymbol::Local(local_key))
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
        let local_key = self
            .compiler
            .ctx
            .named_ctx
            .root_namespace_of_ast_mut(source_key)
            .locals
            .insert(());

        self.scopes
            .insert_forced(name, LocalSymbol::Local(local_key));

        local_key
    }
}
