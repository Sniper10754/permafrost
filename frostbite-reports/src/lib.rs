#![no_std]

extern crate alloc;

use alloc::{borrow::Cow, vec::Vec};
use core::ops::Range;

cfg_if! {
    if #[cfg(feature = "std")] {
        extern crate std;

        pub mod print;
    }
}

pub mod sourcemap;
pub mod utils;

use cfg_if::cfg_if;

use derive_more::*;

pub trait IntoReport {
    type Arguments;

    fn into_report(self, arguments: Self::Arguments) -> Report;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Report {
    pub level: Level,
    pub location: Range<usize>,
    pub title: Cow<'static, str>,
    pub description: Option<Cow<'static, str>>,
    pub infos: Vec<Label>,
    pub helps: Vec<Label>,
}

impl Report {
    pub fn new(
        level: Level,
        location: Range<usize>,
        title: impl Into<Cow<'static, str>>,
        description: Option<impl Into<Cow<'static, str>>>,
        infos: impl IntoIterator<Item = Label>,
        helps: impl IntoIterator<Item = Label>,
    ) -> Self {
        Self {
            level,
            location,
            title: title.into(),
            description: description.map(Into::into),
            infos: infos.into_iter().collect(),
            helps: helps.into_iter().collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Label {
    pub info: Cow<'static, str>,
    pub location: Option<Range<usize>>,
}

impl Label {
    pub fn new(
        info: impl Into<Cow<'static, str>>,
        location: impl Into<Option<Range<usize>>>,
    ) -> Self {
        Self {
            info: info.into(),
            location: location.into(),
        }
    }
}

#[derive(Debug, Display, Clone, Copy, PartialEq)]
pub enum Level {
    Error,
    Warn,
    Info,
}
