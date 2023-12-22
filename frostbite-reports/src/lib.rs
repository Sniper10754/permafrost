#![no_std]

extern crate alloc;

use alloc::{borrow::Cow, vec::Vec};
use core::ops::Range;
use sourcemap::SourceId;

cfg_if! {
    if #[cfg(feature = "std")] {
        extern crate std;

    }
}

pub mod backtrace_printer;
#[cfg(feature = "diagnostic_printer")]
pub mod diagnostic_printer;
pub mod sourcemap;
pub mod utils;

use cfg_if::cfg_if;

use derive_more::*;

pub trait IntoReport {
    type Arguments;

    fn into_report(self, arguments: Self::Arguments) -> Report;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Report {
    Diagnostic(Diagnostic),
    Backtrace(Backtrace),
}

impl Report {
    pub fn new_diagnostic(
        level: Level,
        location: Range<usize>,
        source_id: impl Into<SourceId>,
        title: impl Into<Cow<'static, str>>,
        description: Option<impl Into<Cow<'static, str>>>,
        infos: impl IntoIterator<Item = Label>,
        helps: impl IntoIterator<Item = Label>,
    ) -> Self {
        Self::Diagnostic(Diagnostic {
            level,
            span: location,
            source_id: source_id.into(),
            title: title.into(),
            description: description.map(Into::into),
            infos: infos.into_iter().collect(),
            helps: helps.into_iter().collect(),
        })
    }

    pub fn new_backtrace(
        reason: impl Into<Cow<'static, str>>,
        message: impl Into<Cow<'static, str>>,
        frames: Vec<Frame>,
    ) -> Self {
        Self::Backtrace(Backtrace {
            reason: reason.into(),
            message: message.into(),
            frames,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic {
    level: Level,
    span: Range<usize>,
    source_id: SourceId,
    title: Cow<'static, str>,
    description: Option<Cow<'static, str>>,
    infos: Vec<Label>,
    helps: Vec<Label>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Label {
    pub info: Cow<'static, str>,
    pub span: Option<Range<usize>>,
    pub src_id: SourceId,
}

impl Label {
    pub fn new(
        info: impl Into<Cow<'static, str>>,
        location: impl Into<Option<Range<usize>>>,
        src_id: impl Into<SourceId>,
    ) -> Self {
        Self {
            info: info.into(),
            span: location.into(),
            src_id: src_id.into(),
        }
    }
}

#[derive(Debug, Display, Clone, Copy, PartialEq)]
pub enum Level {
    Error,
    Warn,
    Info,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Backtrace {
    pub reason: Cow<'static, str>,
    pub message: Cow<'static, str>,
    pub frames: Vec<Frame>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Frame {
    pub source_id: SourceId,
    pub position: Option<Position>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, derive_more::From)]
pub enum Position {
    Span(Range<usize>),
    Line(usize),
}