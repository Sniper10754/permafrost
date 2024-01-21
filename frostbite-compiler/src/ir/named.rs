use alloc::{boxed::Box, string::String, vec::Vec};
use core::ops::Range;
use dbg_pls::DebugPls;
use frostbite_parser::ast::{
    tokens::{
        ArrowToken, FunctionToken, LeftBraceToken, LeftParenthesisToken, Operator, ReturnToken,
        RightBraceToken, RightParenthesisToken, TypeAnnotation,
    },
    Spannable, Spanned,
};

pub type Span = Range<usize>;

#[derive(Debug, Clone, PartialEq)]
pub struct NamedAst
{
    pub exprs: Vec<NamedExpr>,
    pub symbols: 
}

impl Spannable for NamedExpr
{
    fn span(&self) -> Span
    {
        match self {
            NamedExpr::Int(Spanned(span, _))
            | NamedExpr::Float(Spanned(span, _))
            | NamedExpr::Ident(Spanned(span, _))
            | NamedExpr::String(Spanned(span, _))
            | NamedExpr::Bool(Spanned(span, _)) => span.clone(),

            NamedExpr::BinaryOperation {
                lhs,
                operator: _,
                rhs,
            } => (lhs.span().start)..(rhs.span().end),
            NamedExpr::Assign { lhs, value } => (lhs.span().start)..(value.span().end),

            NamedExpr::Function { fn_token, body, .. } => {
                (fn_token.span().start)..(body.span().end)
            }
            NamedExpr::Call {
                callee: _,
                left_paren,
                arguments: _,
                right_paren,
            } => (left_paren.span().start)..(right_paren.span().end),

            NamedExpr::Block {
                left_brace,
                expressions: _,
                right_brace,
            } => (left_brace.span().start)..(right_brace.span().end),

            NamedExpr::Return(ret_token, expr) => {
                (ret_token.span().start)
                    ..expr
                        .as_ref()
                        .map(|expr| expr.span().end)
                        .unwrap_or(ret_token.span().end)
            }

            NamedExpr::Poisoned => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum NamedExpr
{
    Int(Spanned<i32>),
    Float(Spanned<f32>),
    Ident(Spanned<String>),
    Bool(Spanned<bool>),
    String(Spanned<String>),

    BinaryOperation
    {
        lhs: Box<Self>,
        operator: Operator,
        rhs: Box<Self>,
    },

    Assign
    {
        lhs: Box<Self>,
        value: Box<Self>,
    },

    Function
    {
        fn_token: FunctionToken,
        name: Option<Spanned<String>>,
        arguments: Vec<Argument>,
        return_type_token: Option<ArrowToken>,
        return_type_annotation: Option<Spanned<TypeAnnotation>>,
        body: Box<Self>,
    },

    Call
    {
        callee: Box<Self>,
        left_paren: LeftParenthesisToken,
        arguments: Vec<NamedExpr>,
        right_paren: RightParenthesisToken,
    },

    Block
    {
        left_brace: LeftBraceToken,
        expressions: Vec<Self>,
        right_brace: RightBraceToken,
    },

    Return(ReturnToken, Option<Box<Self>>),

    Poisoned,
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Argument
{
    pub name: Spanned<String>,
    pub type_annotation: Spanned<TypeAnnotation>,
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum ImportDirectiveKind
{
    FromModuleImportSymbol
    {
        module: ModulePath,
        symbol: Spanned<String>,
    },
    ImportModule
    {
        module: ModulePath
    },
}

#[derive(Debug, Clone, PartialEq, DebugPls, derive_more::Display, derive_more::From)]
pub struct ModulePath
{
    name: String,
}
