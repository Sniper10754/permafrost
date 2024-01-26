#![no_std]

extern crate alloc;

use alloc::{borrow::Cow, vec::Vec};
use core::{convert::Infallible, ops::Range};
use sourcemap::SourceKey;

cfg_if! {
    if #[cfg(feature = "std")] {
        extern crate std;
    }
}

#[cfg(feature = "diagnostic_printer")]
pub mod printer;
pub mod sourcemap;
pub mod utils;

use cfg_if::cfg_if;

use derive_more::Display;

pub trait IntoReport
{
    fn into_report(self) -> Report;
}

impl IntoReport for Infallible
{
    fn into_report(self) -> Report
    {
        match self {}
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Location
{
    span: Range<usize>,
    source_key: SourceKey,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Report
{
    level: Level,
    span: Range<usize>,
    source_key: SourceKey,
    title: Cow<'static, str>,
    description: Option<Cow<'static, str>>,
    infos: Vec<Label>,
    helps: Vec<Label>,
}

impl Report
{
    pub fn new(
        level: Level,
        location: Range<usize>,
        source_key: impl Into<SourceKey>,
        title: impl Into<Cow<'static, str>>,
        description: Option<impl Into<Cow<'static, str>>>,
        infos: impl IntoIterator<Item = Label>,
        helps: impl IntoIterator<Item = Label>,
    ) -> Self
    {
        Self {
            level,
            span: location,
            source_key: source_key.into(),
            title: title.into(),
            description: description.map(Into::into),
            infos: infos.into_iter().collect(),
            helps: helps.into_iter().collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Label
{
    pub info: Cow<'static, str>,
    pub span: Option<Range<usize>>,
    pub src_id: SourceKey,
}

impl Label
{
    pub fn new(
        info: impl Into<Cow<'static, str>>,
        location: impl Into<Option<Range<usize>>>,
        src_id: impl Into<SourceKey>,
    ) -> Self
    {
        Self {
            info: info.into(),
            span: location.into(),
            src_id: src_id.into(),
        }
    }
}

#[derive(Debug, Display, Clone, Copy, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Level
{
    Error,
    Warn,
    Info,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, derive_more::From)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Position
{
    Span(Range<usize>),
    Line(usize),
}

#[derive(Debug, Clone, Default)]
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
