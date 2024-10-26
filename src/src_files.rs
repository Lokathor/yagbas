use crate::token::Token;
use bimap::BiMap;
use chumsky::span::Span;
use std::{
  num::NonZeroUsize,
  path::{Path, PathBuf},
  sync::{
    atomic::{AtomicUsize, Ordering},
    OnceLock, PoisonError, RwLock,
  },
};

static INFO_CACHE: OnceLock<RwLock<BiMap<SrcID, &'static SrcFile>>> =
  OnceLock::new();

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SrcFile {
  path_buf: PathBuf,
  file_text: String,
  line_bytes: Vec<usize>,
}
impl SrcFile {
  pub fn read_from_path<P>(p: &P) -> Result<Self, std::io::Error>
  where
    P: AsRef<Path> + ?Sized,
  {
    let path_buf = p.as_ref().to_owned();
    let file_text = std::fs::read_to_string(&path_buf)?;
    let mut line_bytes = Vec::new();
    let mut total = 0;
    // Note(Lokathor): This works on both `\r\n` and `\n`, but will not generate
    // the correct totals if the file uses bare `\r` for line endings.
    for line in file_text.split_inclusive('\n') {
      line_bytes.push(total);
      total += line.len();
    }
    let output = Self { path_buf, file_text, line_bytes };
    let _ = SrcID::from(&output);
    Ok(output)
  }

  pub fn in_memory(s: &str) -> Self {
    let path_buf = PathBuf::from("<in_memory>");
    let file_text = s.to_string();
    let mut line_bytes = Vec::new();
    let mut total = 0;
    // Note(Lokathor): This works on both `\r\n` and `\n`, but will not generate
    // the correct totals if the file uses bare `\r` for line endings.
    for line in file_text.split_inclusive('\n') {
      line_bytes.push(total);
      total += line.len();
    }
    let output = Self { path_buf, file_text, line_bytes };
    let _ = SrcID::from(&output);
    output
  }

  #[inline]
  #[must_use]
  pub fn path(&self) -> &Path {
    &self.path_buf
  }

  #[inline]
  #[must_use]
  pub fn text(&self) -> &str {
    &self.file_text
  }

  #[inline]
  #[must_use]
  #[track_caller]
  pub fn get_id(&self) -> SrcID {
    let rw_lock = INFO_CACHE.get_or_init(|| RwLock::new(BiMap::new()));
    let read = rw_lock.read().unwrap_or_else(PoisonError::into_inner);
    // Note(Lokathor): This shouldn't ever panic because all ID values should
    // have been initialized by the SrcFile constructor functions.
    *read.get_by_right(&self).unwrap()
  }

  #[inline]
  #[must_use]
  pub fn line_col(&self, byte: usize) -> (usize, usize) {
    match self.line_bytes.binary_search(&byte) {
      Ok(line) => (1 + line, 1),
      Err(could_be) => {
        let line = could_be.wrapping_sub(1);
        let line_start = self.line_bytes.get(line).copied().unwrap_or(0);
        let col = byte.saturating_sub(line_start);
        (1 + line, 1 + col)
      }
    }
  }

  #[inline]
  pub fn iter_tokens(
    &self,
  ) -> impl Iterator<Item = FileSpanned<Token>> + Clone + '_ {
    let id = self.get_id();
    let mut lexer = Token::lexer(&self.file_text);
    core::iter::from_fn(move || {
      let token = lexer.next()?.unwrap_or(Token::TokenError);
      let span = FileSpan::new(id, lexer.span());
      Some(FileSpanned::new(token, span))
    })
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct SrcID(NonZeroUsize);
impl SrcID {
  #[inline]
  fn try_new() -> Option<Self> {
    static NEXT_INFO_ID: AtomicUsize = AtomicUsize::new(1);

    NonZeroUsize::new(NEXT_INFO_ID.fetch_add(1, Ordering::Relaxed)).map(Self)
  }
  #[inline]
  #[track_caller]
  fn new() -> Self {
    Self::try_new().expect("exhausted the available SrcFileInfoID values!")
  }
  #[inline]
  #[must_use]
  pub const fn as_usize(self) -> usize {
    self.0.get()
  }
  #[inline]
  #[must_use]
  #[track_caller]
  pub fn get_info(self) -> &'static SrcFile {
    let rw_lock = INFO_CACHE.get_or_init(|| RwLock::new(BiMap::new()));
    let read = rw_lock.read().unwrap_or_else(PoisonError::into_inner);
    // Note(Lokathor): This shouldn't ever panic because all ID values should
    // have been made when inserting the info into the cache.
    read.get_by_left(&self).unwrap()
  }
}
impl<'a> From<&'a SrcFile> for SrcID {
  /// Convert any `&SrcFileInfo` into its ID, automatically interning it if
  /// necessary.
  fn from(s: &'a SrcFile) -> Self {
    let rw_lock = INFO_CACHE.get_or_init(|| RwLock::new(BiMap::new()));
    let read = rw_lock.read().unwrap_or_else(PoisonError::into_inner);
    if let Some(id) = read.get_by_right(s) {
      *id
    } else {
      drop(read);
      let mut write = rw_lock.write().unwrap_or_else(PoisonError::into_inner);
      if let Some(id) = write.get_by_right(s) {
        *id
      } else {
        let id = Self::new();
        let leaked: &'static SrcFile = Box::leak(Box::new(s.clone()));
        write.insert(id, leaked);
        id
      }
    }
  }
}
impl From<SrcFile> for SrcID {
  /// Convert any `SrcFileInfo` into its ID, automatically interning it if
  /// necessary.
  ///
  /// Prefer this impl if you *expect* to need to intern the value, it will
  /// avoid a clone.
  fn from(s: SrcFile) -> Self {
    let rw_lock = INFO_CACHE.get_or_init(|| RwLock::new(BiMap::new()));
    let read = rw_lock.read().unwrap_or_else(PoisonError::into_inner);
    if let Some(id) = read.get_by_right(&s) {
      *id
    } else {
      drop(read);
      let mut write = rw_lock.write().unwrap_or_else(PoisonError::into_inner);
      if let Some(id) = write.get_by_right(&s) {
        *id
      } else {
        let id = Self::new();
        // clone avoided!
        let leaked: &'static SrcFile = Box::leak(Box::new(s));
        write.insert(id, leaked);
        id
      }
    }
  }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileSpan {
  pub id: SrcID,
  pub start: usize,
  pub end: usize,
}
impl chumsky::span::Span for FileSpan {
  type Offset = usize;
  type Context = SrcID;
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
impl core::fmt::Debug for FileSpan {
  #[inline]
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    core::fmt::Display::fmt(self, f)
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
impl<T> FileSpanned<T> {
  #[inline]
  #[must_use]
  pub const fn new(t: T, span: FileSpan) -> Self {
    Self { _payload: t, _span: span }
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
