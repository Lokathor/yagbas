use chumsky::{
  extra::ParserExtra,
  input::{Input, MapExtra},
};

use crate::{file_span::FileSpan, src_file::SrcID};

#[derive(Clone, Copy)]
pub struct FileSpanned<T> {
  pub _payload: T,
  pub _span: FileSpan,
}
impl<T> FileSpanned<T> {
  #[inline]
  #[must_use]
  pub const fn new(t: T, span: FileSpan) -> Self {
    Self { _payload: t, _span: span }
  }
  #[inline]
  #[must_use]
  pub fn from_extras<'src, I, E>(
    t: T, extras: &mut MapExtra<'src, '_, I, E>,
  ) -> Self
  where
    I: Input<'src, Span = FileSpan>,
    E: ParserExtra<'src, I>,
  {
    Self::new(t, extras.span())
  }
}
impl<T> chumsky::span::Span for FileSpanned<T>
where
  T: Clone,
{
  type Offset = usize;
  type Context = (T, SrcID);
  #[inline]
  #[must_use]
  fn new((t, id): (T, SrcID), range: std::ops::Range<Self::Offset>) -> Self {
    Self { _payload: t, _span: FileSpan::new(id, range) }
  }
  #[inline]
  #[must_use]
  fn start(&self) -> Self::Offset {
    self._span.start()
  }
  #[inline]
  #[must_use]
  fn end(&self) -> Self::Offset {
    self._span.end()
  }
  #[inline]
  #[must_use]
  fn context(&self) -> Self::Context {
    (self._payload.clone(), self._span.id)
  }
}
impl<T> core::ops::Deref for FileSpanned<T> {
  type Target = T;
  #[inline]
  #[must_use]
  fn deref(&self) -> &Self::Target {
    &self._payload
  }
}
impl<T> core::ops::DerefMut for FileSpanned<T> {
  #[inline]
  #[must_use]
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self._payload
  }
}
impl<T> core::fmt::Debug for FileSpanned<T>
where
  T: core::fmt::Debug,
{
  #[inline]
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    core::fmt::Debug::fmt(&self._payload, f)
  }
}
impl<T> core::fmt::Display for FileSpanned<T>
where
  T: core::fmt::Display,
{
  #[inline]
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    core::fmt::Display::fmt(&self._payload, f)
  }
}
impl<T> core::cmp::PartialEq<Self> for FileSpanned<T>
where
  T: core::cmp::PartialEq,
{
  #[inline]
  #[must_use]
  fn eq(&self, other: &Self) -> bool {
    self._payload == other._payload
  }
}
impl<T> core::cmp::Eq for FileSpanned<T> where T: Eq {}
impl<T> core::cmp::PartialEq<T> for FileSpanned<T>
where
  T: core::cmp::PartialEq,
{
  #[inline]
  #[must_use]
  fn eq(&self, other: &T) -> bool {
    self._payload.eq(other)
  }
}
