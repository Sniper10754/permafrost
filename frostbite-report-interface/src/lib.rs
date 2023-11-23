#![no_std]

pub mod utils;

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
            location: location.map(|it| it.into()),
            title: title.into(),
            description: description.map(|it| it.into()),
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
            location: location.map(|it| it.into()),
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
            location: location.map(|it| it.into()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Level {
    Error,
    Warn,
    Info,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Location {
    /// Location in the file, not LINE
    Location(usize),
    Span(Range<usize>),
}

impl From<Range<usize>> for Location {
    fn from(span: Range<usize>) -> Self {
        Self::Span(span)
    }
}

impl From<usize> for Location {
    fn from(location: usize) -> Self {
        Self::Location(location)
    }
}
