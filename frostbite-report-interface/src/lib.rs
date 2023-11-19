#![no_std]

extern crate alloc;

use alloc::{borrow::Cow, vec::Vec};
use core::ops::Range;

pub struct Report {
    pub level: Level,
    pub span: Option<Range<usize>>,
    pub title: Cow<'static, str>,
    pub description: Option<Cow<'static, str>>,
    pub infos: Vec<Info>,
    pub helps: Vec<Help>,
}

pub struct Info {
    pub info: Cow<'static, str>,
    pub span: Option<Range<usize>>,
}

pub struct Help {
    pub info: Cow<'static, str>,
    pub span: Option<Range<usize>>,
}

pub enum Level {
    Error,
    Warn,
    Info,
}
