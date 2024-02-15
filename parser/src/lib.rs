extern crate alloc;

use lalrpop_util::lalrpop_mod;

use permafrost_ast::{Expr, Spanned};
use permafrost_reports::{sourcemap::SourceKey, ReportContext};

use error::LexicalError;
use lexer::TokenStream;

pub mod error;
pub mod lexer;

lalrpop_mod! { grammar }

pub struct Parser;

impl Parser
{
    pub fn parse(
        source_key: SourceKey,
        token_stream: TokenStream,
    ) -> Result<Expr, LexicalError>
    {
        grammar::ExprParser::new()
            .parse(
                token_stream
                    .into_iter()
                    .map(|Spanned(span, token)| (span.start, token, span.end)),
            )
            .map_err(|error| LexicalError {
                source_key,
                kind: error.into(),
            })
    }
}
