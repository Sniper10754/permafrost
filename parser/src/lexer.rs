use alloc::{collections::VecDeque, string::String, vec, vec::Vec};

use frostbite_reports::{sourcemap::SourceKey, IntoReport, Level, Report, ReportContext};
use logos::{Logos, Span};

use frostbite_ast::{tokens::BinaryOperatorKind, Spanned};

mod helpers
{
    use alloc::string::String;

    use logos::Lexer;
    use num_traits::Num;

    use frostbite_ast::tokens::BinaryOperatorKind;

    use super::{LexerErrorKind, Token};

    pub fn parse_number<N: Num>(lexer: &Lexer<'_, Token>) -> Result<N, LexerErrorKind>
    {
        let slice = lexer.slice();

        N::from_str_radix(slice, 10)
            .map_err(|_| LexerErrorKind::NumberTooBig { span: lexer.span() })
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

        input[1..(input.len() - 1)].into()
    }
}

pub type SpannedToken = Spanned<Token>;

#[derive(Debug, PartialEq, Clone)]
pub struct LexerError
{
    source_key: SourceKey,
    kind: LexerErrorKind,
}

#[derive(Debug, PartialEq, Clone, Default)]
pub enum LexerErrorKind
{
    NumberTooBig
    {
        span: Span
    },

    UnknownToken
    {
        span: Span
    },

    #[default]
    GenericLexerError,
}

impl IntoReport for LexerError
{
    fn into_report(self) -> frostbite_reports::Report
    {
        let source_key = self.source_key;

        let location;
        let title;
        let description;

        match self.kind {
            LexerErrorKind::NumberTooBig { span } => {
                location = span;

                title = "Number too big";

                description = "Number can't be lexed because is too big";
            }
            LexerErrorKind::UnknownToken { span } => {
                location = span;

                title = "Unknown token";

                description = "Token was not recognized";
            }
            LexerErrorKind::GenericLexerError => unreachable!(),
        }

        Report::new(Level::Error, location, source_key, title, Some(description))
    }
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(error = LexerErrorKind)]
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

    #[token("from")]
    From,

    #[token("import")]
    Import,

    #[token("::")]
    DoubleColon,

    #[token("return")]
    Return,

    #[token("->")]
    Arrow,

    #[token(",")]
    Comma,

    #[regex(r#"(\+|\-|\*/|==|)"#, helpers::parse_operator)]
    BinaryOperator(BinaryOperatorKind),

    #[token(":")]
    Colon,

    #[token(";")]
    Semicolon,
}

#[derive(Debug, Clone)]
pub struct TokenStream
{
    tokens: VecDeque<SpannedToken>,
    index: usize,
}

impl TokenStream
{
    pub fn with_vec_deque(tokens: impl Into<VecDeque<SpannedToken>>) -> Self
    {
        let tokens = tokens.into();
        Self { tokens, index: 0 }
    }

    pub fn with_iter(tokens: impl IntoIterator<Item = SpannedToken>) -> Self
    {
        let tokens = tokens.into_iter().collect::<VecDeque<_>>();
        Self { tokens, index: 0 }
    }

    pub fn skip_token(&mut self) -> Option<SpannedToken>
    {
        self.next()
    }

    pub fn skip_tokens(
        stream: &mut TokenStream,
        count: usize,
    ) -> Vec<SpannedToken>
    {
        let mut taken_tokens = vec![];

        for _ in 0..count {
            if let Some(token) = stream.next() {
                taken_tokens.push(token);
            } else {
                break;
            }
        }

        taken_tokens
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<SpannedToken>
    {
        if self.index < self.tokens.len() {
            let token = self.tokens[self.index].clone();

            self.index += 1;

            Some(token)
        } else {
            None
        }
    }

    #[must_use]
    pub fn peek(&self) -> Option<&SpannedToken>
    {
        self.tokens.get(self.index)
    }

    pub fn take_while<P>(
        stream: &mut TokenStream,
        predicate: P,
    ) -> Vec<SpannedToken>
    where
        P: Fn(&SpannedToken) -> bool,
    {
        let mut taken_tokens = vec![];

        while let Some(token) = stream.peek() {
            if predicate(token) {
                if let Some(token) = stream.next() {
                    taken_tokens.push(token);
                }
            } else {
                break;
            }
        }

        taken_tokens
    }

    #[must_use]
    pub fn previous(&self) -> Option<SpannedToken>
    {
        if self.index > 0 {
            Some(self.tokens[self.index - 1].clone())
        } else {
            None
        }
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
                let error_kind = match e {
                    LexerErrorKind::GenericLexerError => LexerErrorKind::UnknownToken { span },

                    error => error,
                };

                let error = LexerError {
                    source_key,
                    kind: error_kind,
                };

                report_ctx.push(error.into_report());
            }
        }
    }

    TokenStream::with_vec_deque(tokens)
}
