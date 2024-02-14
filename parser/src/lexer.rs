use alloc::{collections::VecDeque, string::String, vec::Vec};

use permafrost_ast::{tokens::BinaryOperatorKind, Spanned};
use permafrost_reports::{sourcemap::SourceKey, IntoReport, ReportContext};

use crate::error::{LexicalError, LexicalErrorKind};

use logos::Logos;

use derive_more::*;

mod helpers
{
    use alloc::string::String;

    use logos::Lexer;
    use num_traits::Num;

    use permafrost_ast::tokens::BinaryOperatorKind;

    use crate::error::LexicalErrorKind;

    use super::Token;

    pub fn parse_number<N: Num>(lexer: &Lexer<'_, Token>) -> Result<N, LexicalErrorKind>
    {
        let slice = lexer.slice();

        N::from_str_radix(slice, 10)
            .map_err(|_| LexicalErrorKind::NumberTooBig { span: lexer.span() })
    }

    pub fn parse_operator(lexer: &Lexer<'_, Token>) -> BinaryOperatorKind
    {
        match lexer.slice() {
            "+" => BinaryOperatorKind::Add,
            "-" => BinaryOperatorKind::Sub,
            "*" => BinaryOperatorKind::Mul,
            "/" => BinaryOperatorKind::Div,
            "==" => BinaryOperatorKind::Equal,

            _ => unreachable!("Caller must guarantee that the current slice of text is a operator"),
        }
    }

    pub fn unquote_str(lexer: &Lexer<'_, Token>) -> String
    {
        let input = lexer.slice();

        let start = 1;
        let end = input.len() - 1;

        (input[start..end]).into()
    }
}

pub type SpannedToken = Spanned<Token>;

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(error = LexicalErrorKind)]
#[logos(skip r"[\t\n\r ]+")]
pub enum Token
{
    #[regex("-?[0-9]+", helpers::parse_number::<i32>)]
    Int(i32),

    #[regex(r#"(-?[0-9]+)?\.[0-9]+"#, helpers::parse_number::<f32>)]
    Float(f32),

    #[regex("[a-zA-Z]([a-zA-Z0-9]|_)*", |lexer| String::from(lexer.slice()))]
    Ident(String),

    #[regex(r#""(\\[\\"]|[^"])*""#, helpers::unquote_str)]
    String(String),

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("=")]
    Eq,

    #[token("function")]
    Fn,

    #[token("pub")]
    Public,

    #[token("from")]
    From,

    #[token("use")]
    Use,

    #[token("::")]
    DoubleColon,

    #[token("return")]
    Return,

    #[token("->")]
    Arrow,

    #[token(",")]
    Comma,

    #[token("mod")]
    Module,

    #[regex(r#"(\+|-|\*|/|==|)"#, helpers::parse_operator)]
    BinaryOperator(BinaryOperatorKind),

    #[token(":")]
    Colon,

    #[token(";")]
    Semicolon,
}

impl Token
{
    pub const fn description(&self) -> &'static str
    {
        match self {
            Token::Int(_) => "Integer literal",
            Token::Float(_) => "Floating-point literal",
            Token::Ident(_) => "Identifier",
            Token::String(_) => "String literal",
            Token::True => "Boolean true",
            Token::False => "Boolean false",
            Token::LParen => "Left parenthesis",
            Token::RParen => "Right parenthesis",
            Token::LBrace => "Left brace",
            Token::RBrace => "Right brace",
            Token::Eq => "Equality operator",
            Token::Fn => "Function keyword",
            Token::Public => "Public keyword",
            Token::From => "From keyword",
            Token::Use => "Use keyword",
            Token::DoubleColon => "Double colon",
            Token::Return => "Return keyword",
            Token::Arrow => "Arrow symbol",
            Token::Comma => "Comma",
            Token::Module => "Module keyword",
            Token::BinaryOperator(op) => op.description(),
            Token::Colon => "Colon",
            Token::Semicolon => "Semicolon",
        }
    }
}

#[derive(Debug, Clone, Constructor)]
pub struct TokenStream(VecDeque<SpannedToken>);

impl IntoIterator for TokenStream
{
    type IntoIter = <VecDeque<SpannedToken> as IntoIterator>::IntoIter;
    type Item = SpannedToken;

    fn into_iter(self) -> Self::IntoIter
    {
        self.0.into_iter()
    }
}

pub fn tokenize(
    report_ctx: &mut ReportContext,
    source_key: SourceKey,
    input: &str,
) -> TokenStream
{
    let lexer = Token::lexer(input).spanned();
    let mut tokens = Vec::new();

    for (token, span) in lexer {
        match token {
            Ok(t) => tokens.push((span, t).into()),
            Err(e) => {
                let kind = match e {
                    LexicalErrorKind::GenericLexerError => LexicalErrorKind::UnknownToken { span },

                    error => error,
                };

                let error = LexicalError { source_key, kind };

                report_ctx.push(error.into_report());
            }
        }
    }

    TokenStream::new(tokens.into())
}
