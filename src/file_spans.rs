use crate::src_files::SrcFileInfoID;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileSpan {
  id: SrcFileInfoID,
  start: usize,
  end: usize,
}
impl chumsky::span::Span for FileSpan {
  type Offset = usize;
  type Context = SrcFileInfoID;
  #[inline]
  #[must_use]
  fn new(context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
    Self { id: context, start: range.start, end: range.end }
  }
  #[inline]
  #[must_use]
  fn start(&self) -> Self::Offset {
    self.start
  }
  #[inline]
  #[must_use]
  fn end(&self) -> Self::Offset {
    self.end
  }
  #[inline]
  #[must_use]
  fn context(&self) -> Self::Context {
    self.id
  }
}
impl core::fmt::Display for FileSpan {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let info = self.id.get_info();
    let path = info.path();
    let (line, col) = info.line_col(self.start);
    write!(f, "{path}:{line}:{col}", path = path.display())
  }
}

#[derive(Clone, Copy)]
pub struct FileSpanned<T> {
  pub _payload: T,
  pub _span: FileSpan,
}
impl<T> chumsky::span::Span for FileSpanned<T>
where
  T: Clone,
{
  type Offset = usize;
  type Context = (T, SrcFileInfoID);
  #[inline]
  #[must_use]
  fn new((t, id): Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
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
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    core::fmt::Debug::fmt(&self._payload, f)
  }
}
impl<T> core::fmt::Display for FileSpanned<T>
where
  T: core::fmt::Display,
{
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    core::fmt::Display::fmt(&self._payload, f)
  }
}
impl<T> core::cmp::PartialEq for FileSpanned<T>
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
