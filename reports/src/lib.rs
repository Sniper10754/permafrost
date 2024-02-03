#![no_std]

extern crate alloc;

use alloc::{borrow::Cow, vec::Vec};
use core::convert::Infallible;
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

pub use frostbite_ast::Span;

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

#[derive(Debug, Clone, PartialEq)]
pub struct Location
{
    span: Span,
    source_key: SourceKey,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Report
{
    level: Level,
    span: Span,
    source_key: SourceKey,
    title: Cow<'static, str>,
    description: Option<Cow<'static, str>>,
    infos: Vec<Label>,
}

impl Report
{
    pub fn new(
        level: Level,
        location: Span,
        source_key: impl Into<SourceKey>,
        title: impl Into<Cow<'static, str>>,
        description: Option<impl Into<Cow<'static, str>>>,
    ) -> Self
    {
        Self {
            level,
            span: location,
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
        location: impl Into<Option<Span>>,
        src_key: impl Into<SourceKey>,
    ) -> Self
    {
        Self {
            info: info.into(),
            span: location.into(),
            src_key: src_key.into(),
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
