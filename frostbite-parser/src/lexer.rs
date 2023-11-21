use core::ops::Range;

use alloc::{collections::VecDeque, vec::Vec};
use frostbite_report_interface::Location;
use logos::Logos;

mod helpers {
    use logos::Lexer;
    use num_traits::Num;

    use super::{LexerError, Token};

    pub fn parse_number<'input, N: Num>(
        lexer: &Lexer<'input, Token<'input>>,
    ) -> Result<N, LexerError> {
        N::from_str_radix(lexer.slice(), 10).map_err(|_| LexerError::NumberTooBig {
            location: lexer.span().into(),
        })
    }
}

pub type Spanned<T> = (Range<usize>, T);
pub type SpannedToken<'a> = Spanned<Token<'a>>;

#[derive(Debug, PartialEq, Clone, Default)]
pub enum LexerError {
    NumberTooBig {
        location: Location,
    },

    #[default]
    Default,
}

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(error = LexerError)]
pub enum Token<'input> {
    #[regex("-?[0-9]+", helpers::parse_number::<i32>)]
    Int(i32),

    #[regex(r#"(-?[0-9]+)?.[0-9]+"#, helpers::parse_number::<f32>)]
    Float(f32),

    #[regex("[a-zA-Z][a-zA-Z0-9]*")]
    Ident(&'input str),

    #[regex(r#"".*""#)]
    String(&'input str),

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
}

pub struct TokenStream<'input> {
    tokens: VecDeque<SpannedToken<'input>>,
}

impl<'input> TokenStream<'input> {
    pub fn with_vec_deque(tokens: impl Into<VecDeque<SpannedToken<'input>>>) -> Self {
        Self {
            tokens: tokens.into(),
        }
    }

    pub fn with_iter(tokens: impl IntoIterator<Item = SpannedToken<'input>>) -> Self {
        Self {
            tokens: tokens.into_iter().collect(),
        }
    }

    pub fn peek(&self) -> Option<&SpannedToken<'_>> {
        self.tokens.front()
    }
}

impl<'input> Iterator for TokenStream<'input> {
    type Item = SpannedToken<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        self.tokens.pop_front()
    }
}

pub fn tokenize(input: &str) -> Result<TokenStream<'_>, LexerError> {
    let lexer = Token::lexer(input).spanned();
    let mut tokens = Vec::new();

    for (token, span) in lexer {
        match token {
            Ok(t) => tokens.push((span, t)),
            Err(e) => return Err(e),
        }
    }

    Ok(TokenStream::with_vec_deque(tokens))
}
