#![no_std]

extern crate alloc;

pub mod ast;

pub use frostbite_report_interface as report;

mod __parser {
    use lalrpop_util::lalrpop_mod;

    lalrpop_mod!(parser);

    pub use parser::*;
}

pub mod parser {
    pub use super::__parser::ExprParser;
}

#[cfg(test)]
mod tests {
    
}