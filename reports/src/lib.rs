#![no_std]

extern crate alloc;

use alloc::{borrow::Cow, vec::Vec};
use core::{convert::Infallible, ops::Range};
use sourcemap::SourceKey;

use derive_more::*;

cfg_if! {
    if #[cfg(feature = "std")] {
        extern crate std;
    }
}

#[cfg(feature = "diagnostic_printer")]
pub mod printer;
pub mod sourcemap;
pub mod utils;

pub use permafrost_ast::Span;

use cfg_if::cfg_if;

use derive_more::Display;

pub trait IntoReport
{
    fn into_report(self) -> Report;
}

impl IntoReport for Report
{
    fn into_report(self) -> Report
    {
        self
    }
}

impl IntoReport for Infallible
{
    fn into_report(self) -> Report
    {
        match self {}
    }
}

#[derive(Debug, Clone, PartialEq, From)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Location
{
    Span(Span),
    Column(usize),
}

impl Location
{
    pub fn start(&self) -> usize
    {
        match self {
            Location::Span(Range { start, end: _ }) => *start,
            Location::Column(start) => *start,
        }
    }

    pub fn end(&self) -> Option<usize>
    {
        match self {
            Location::Span(Range { start: _, end }) => Some(*end),
            Location::Column(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Report
{
    level: Level,
    location: Location,
    source_key: SourceKey,
    title: Cow<'static, str>,
    description: Option<Cow<'static, str>>,
    infos: Vec<Label>,
}

impl Report
{
    pub fn new(
        level: Level,
        location: impl Into<Location>,
        source_key: impl Into<SourceKey>,
        title: impl Into<Cow<'static, str>>,
        description: Option<impl Into<Cow<'static, str>>>,
    ) -> Self
    {
        Self {
            level,
            location: location.into(),
            source_key: source_key.into(),
            title: title.into(),
            description: description.map(Into::into),
            infos: Vec::new(),
        }
    }

    pub fn with_label(
        mut self,
        label: impl Into<Label>,
    ) -> Self
    {
        self.infos.push(label.into());

        self
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Label
{
    pub info: Cow<'static, str>,
    pub span: Option<Span>,
    pub src_key: SourceKey,
}

impl Label
{
    pub fn new(
        info: impl Into<Cow<'static, str>>,
        span: Option<Span>,
        src_key: SourceKey,
    ) -> Self
    {
        Self {
            info: info.into(),
            span,
            src_key,
        }
    }
}

#[derive(Debug, Display, Clone, Copy, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Level
{
    Error,
    Warn,
    Advice,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, derive_more::From)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Position
{
    Span(Span),
    Line(usize),
}

#[derive(Debug, Clone, Default, Into)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ReportContext
{
    reports: Vec<Report>,
}

impl ReportContext
{
    pub fn has_errors(&self) -> bool
    {
        self.reports
            .iter()
            .any(|report| matches!(report.level, Level::Error))
    }

    pub fn has_reports(&self) -> bool
    {
        !self.reports.is_empty()
    }
}

impl core::ops::DerefMut for ReportContext
{
    fn deref_mut(&mut self) -> &mut Self::Target
    {
        &mut self.reports
    }
}

impl core::ops::Deref for ReportContext
{
    type Target = Vec<Report>;

    fn deref(&self) -> &Self::Target
    {
        &self.reports
    }
}

impl Extend<Report> for ReportContext
{
    fn extend<T: IntoIterator<Item = Report>>(
        &mut self,
        iter: T,
    )
    {
        self.reports.extend(iter)
    }
}
