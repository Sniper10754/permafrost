#![no_std]

extern crate alloc;

pub mod print;
pub mod print_backend;
pub mod utils;

use alloc::{borrow::Cow, vec::Vec};
use core::ops::Range;

use derive_more::*;

pub trait IntoReport {
    type Arguments;

    fn into_report(self, arguments: Self::Arguments) -> Report;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Report {
    pub level: Level,
    pub location: Option<Location>,
    pub title: Cow<'static, str>,
    pub description: Option<Cow<'static, str>>,
    pub infos: Vec<Info>,
    pub helps: Vec<Help>,
}

impl Report {
    pub fn new(
        level: Level,
        location: Option<impl Into<Location>>,
        title: impl Into<Cow<'static, str>>,
        description: Option<impl Into<Cow<'static, str>>>,
        infos: impl IntoIterator<Item = Info>,
        helps: impl IntoIterator<Item = Help>,
    ) -> Self {
        Self {
            level,
            location: location.map(Into::into),
            title: title.into(),
            description: description.map(Into::into),
            infos: infos.into_iter().collect(),
            helps: helps.into_iter().collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Info {
    pub info: Cow<'static, str>,
    pub location: Option<Location>,
}

impl Info {
    pub fn new(info: impl Into<Cow<'static, str>>, location: Option<impl Into<Location>>) -> Self {
        Self {
            info: info.into(),
            location: location.map(Into::into),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Help {
    pub info: Cow<'static, str>,
    pub location: Option<Location>,
}

impl Help {
    pub fn new(info: impl Into<Cow<'static, str>>, location: Option<impl Into<Location>>) -> Self {
        Self {
            info: info.into(),
            location: location.map(Into::into),
        }
    }
}

#[derive(Debug, Display, Clone, Copy, PartialEq)]
pub enum Level {
    Error,
    Warn,
    Info,
}

#[derive(Debug, Clone, From, PartialEq)]
pub enum Location {
    /// A column in the file, may be placed anywhere
    Column(usize),

    /// a span, consisting of a start column and an end column
    Span(Range<usize>),
}

impl Location {
    /// Returns
    pub fn start(&self) -> usize {
        match self {
            Location::Column(start) => *start,
            Location::Span(span) => span.start,
        }
    }
}
