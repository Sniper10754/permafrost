use alloc::{collections::VecDeque, vec, vec::Vec};

use frostbite_reports::{sourcemap::SourceId, IntoReport, Level, Report};
use logos::{Logos, Span};

use crate::ast::{tokens::OperatorKind, Spanned};

mod helpers {
    use logos::Lexer;
    use num_traits::Num;

    use crate::ast::tokens::OperatorKind;

    use super::{LexerError, Token};

    pub fn parse_number<'input, N: Num>(
        lexer: &Lexer<'input, Token<'input>>,
    ) -> Result<N, LexerError> {
        let slice = lexer.slice();

        N::from_str_radix(slice, 10).map_err(|_| LexerError::NumberTooBig { span: lexer.span() })
    }

    pub fn parse_operator<'input>(lexer: &Lexer<'input, Token<'input>>) -> OperatorKind {
        match lexer.slice() {
            "+" => OperatorKind::Add,
            "-" => OperatorKind::Sub,
            "*" => OperatorKind::Mul,
            "/" => OperatorKind::Div,

            _ => unreachable!("Caller must guarantee that the current slice of text is a operator"),
        }
    }

    pub fn unquote_str<'input>(lexer: &Lexer<'input, Token<'input>>) -> &'input str {
        let input = lexer.slice();

        &input[1..(input.len() - 1)]
    }
}

pub type SpannedToken<'a> = Spanned<Token<'a>>;

#[derive(Debug, PartialEq, Clone, Default)]
pub enum LexerError {
    NumberTooBig {
        span: Span,
    },

    UnknownToken {
        span: Span,
    },

    #[default]
    GenericLexerError,
}

impl IntoReport for LexerError {
    type Arguments = SourceId;

    fn into_report(self, src_id: Self::Arguments) -> frostbite_reports::Report {
        let location;
        let title;
        let description;

        match self {
            LexerError::NumberTooBig { span } => {
                location = span;

                title = "Number too big";

                description = "Number can't be lexed because is too big";
            }
            LexerError::UnknownToken { span } => {
                location = span;

                title = "Unknown token";

                description = "Token was not recognized";
            }
            LexerError::GenericLexerError => unreachable!(),
        }

        Report::new_diagnostic(
            Level::Error,
            location,
            src_id,
            title,
            Some(description),
            [],
            [],
        )
    }
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(error = LexerError)]
#[logos(skip r"[\t\n\r ]+")]
pub enum Token<'input> {
    #[regex("-?[0-9]+", helpers::parse_number::<i32>)]
    Int(i32),

    #[regex(r#"(-?[0-9]+)?\.[0-9]+"#, helpers::parse_number::<f32>)]
    Float(f32),

    #[regex("[a-zA-Z]([a-zA-Z0-9]|_)*")]
    Ident(&'input str),

    #[regex(r#""(\\[\\"]|[^"])*""#, helpers::unquote_str)]
    String(&'input str),

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("=")]
    Eq,

    #[token("function")]
    Fn,

    #[token("import")]
    Import,

    #[token("->")]
    Arrow,

    #[token(",")]
    Comma,

    #[regex(r#"[\+\-\*/]"#, helpers::parse_operator)]
    BinaryOperator(OperatorKind),

    #[token(":")]
    Colon,

    #[token(";")]
    Semicolon,
}

#[derive(Debug, Clone)]
pub struct TokenStream<'input> {
    tokens: VecDeque<SpannedToken<'input>>,
    index: usize,
}

impl<'input> TokenStream<'input> {
    pub fn with_vec_deque(tokens: impl Into<VecDeque<SpannedToken<'input>>>) -> Self {
        let tokens = tokens.into();
        Self { tokens, index: 0 }
    }

    pub fn with_iter(tokens: impl IntoIterator<Item = SpannedToken<'input>>) -> Self {
        let tokens = tokens.into_iter().collect::<VecDeque<_>>();
        Self { tokens, index: 0 }
    }

    pub fn skip_token(&mut self) -> Option<SpannedToken<'input>> {
        self.next()
    }

    pub fn skip_tokens(
        stream: &mut TokenStream<'input>,
        count: usize,
    ) -> Vec<SpannedToken<'input>> {
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
    pub fn next(&mut self) -> Option<SpannedToken<'input>> {
        if self.index < self.tokens.len() {
            let token = self.tokens[self.index].clone();

            self.index += 1;

            Some(token)
        } else {
            None
        }
    }

    pub fn peek(&self) -> Option<&SpannedToken<'input>> {
        self.tokens.get(self.index)
    }

    pub fn take_while<P>(
        stream: &mut TokenStream<'input>,
        predicate: P,
    ) -> Vec<SpannedToken<'input>>
    where
        P: Fn(&SpannedToken<'input>) -> bool,
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

    pub fn previous(&self) -> Option<SpannedToken<'input>> {
        if self.index > 0 {
            Some(self.tokens[self.index - 1].clone())
        } else {
            None
        }
    }
}

pub fn tokenize(input: &str) -> Result<TokenStream<'_>, Vec<LexerError>> {
    let lexer = Token::lexer(input).spanned();
    let mut tokens = Vec::new();
    let mut errors = vec![];

    for (token, span) in lexer {
        match token {
            Ok(t) => tokens.push((span, t).into()),
            Err(e) => errors.push(match e {
                LexerError::GenericLexerError => LexerError::UnknownToken { span },

                error => error,
            }),
        }
    }

    if errors.is_empty() {
        Ok(TokenStream::with_vec_deque(tokens))
    } else {
        Err(errors)
    }
}
