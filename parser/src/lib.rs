use error::LexicalError;
use lalrpop_util::lalrpop_mod;
use lexer::TokenStream;
use permafrost_ast::{Expr, Spanned};
use permafrost_reports::{sourcemap::SourceKey, ReportContext};

extern crate alloc;

pub mod error;
pub mod lexer;

lalrpop_mod!(grammar);

pub struct Parser;

impl Parser
{
    pub fn parse(
        report_ctx: &mut ReportContext,
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
            .map_err(|error| match error {
                lalrpop_util::ParseError::InvalidToken { location } => todo!(),
                lalrpop_util::ParseError::UnrecognizedEof { location, expected } => todo!(),
                lalrpop_util::ParseError::UnrecognizedToken { token, expected } => todo!(),
                lalrpop_util::ParseError::ExtraToken { token } => todo!(),
                lalrpop_util::ParseError::User { error } => todo!(),
            })
    }
}
