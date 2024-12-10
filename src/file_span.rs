use crate::src_file::SrcID;

#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileSpan {
  pub id: SrcID,
  pub start: usize,
  pub end: usize,
}
impl FileSpan {
  #[inline]
  #[must_use]
  pub const fn new(id: SrcID, range: core::ops::Range<usize>) -> Self {
    Self { id, start: range.start, end: range.end }
  }
  #[inline]
  #[must_use]
  pub fn join(self, other: Self) -> Self {
    Self {
      id: self.id,
      start: self.start.min(other.start),
      end: self.end.max(other.end),
    }
  }
}
impl chumsky::span::Span for FileSpan {
  type Offset = usize;
  type Context = SrcID;
  #[inline]
  #[must_use]
  fn new(
    context: Self::Context, range: core::ops::Range<Self::Offset>,
  ) -> Self {
    Self::new(context, range)
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
impl ariadne::Span for FileSpan {
  type SourceId = SrcID;

  #[inline]
  #[must_use]
  fn source(&self) -> &Self::SourceId {
    &self.id
  }

  #[inline]
  #[must_use]
  fn start(&self) -> usize {
    self.start
  }

  #[inline]
  #[must_use]
  fn end(&self) -> usize {
    self.end
  }
}
impl core::fmt::Debug for FileSpan {
  #[inline]
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    core::fmt::Display::fmt(self, f)
  }
}
impl core::fmt::Display for FileSpan {
  #[inline]
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let info = self.id.get_src_file();
    let path = info.path();
    let (line, col) = info.line_col(self.start);
    write!(f, "{path}:{line}:{col}", path = path.display())
  }
}
