#![no_std]

extern crate alloc;

use alloc::{borrow::Cow, vec::Vec};
use core::ops::Range;

pub trait IntoReport {
    type Arguments;

    fn into_report(self, arguments: Self::Arguments) -> Report;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Report {
    pub level: Level,
    pub span: Option<Range<usize>>,
    pub title: Cow<'static, str>,
    pub description: Option<Cow<'static, str>>,
    pub infos: Vec<Info>,
    pub helps: Vec<Help>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Info {
    pub info: Cow<'static, str>,
    pub span: Option<Range<usize>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Help {
    pub info: Cow<'static, str>,
    pub span: Option<Range<usize>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Level {
    Error,
    Warn,
    Info,
}
