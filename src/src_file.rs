use crate::{
  ast::data::{Token, TokenTree},
  errors::YagError,
  file_span::FileSpan,
  file_spanned::FileSpanned,
};
use bimap::BiMap;
use chumsky::{
  error::Rich,
  input::{BorrowInput, Input, ValueInput},
  prelude::*,
  span::Span,
};
use std::{
  num::NonZeroUsize,
  path::{Path, PathBuf},
  sync::{
    OnceLock, PoisonError, RwLock,
    atomic::{AtomicUsize, Ordering},
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
  pub fn read_from_path<P>(p: &P) -> Result<Self, YagError>
  where
    P: AsRef<Path> + ?Sized,
  {
    let path_buf = p.as_ref().to_owned();
    let file_text = match std::fs::read_to_string(&path_buf) {
      Ok(string) => string,
      Err(e) => {
        return Err(YagError::FileIO {
          filename: format!("{}", path_buf.display()),
          message: e.to_string(),
        });
      }
    };
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
  pub fn get_src_file(self) -> &'static SrcFile {
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
  #[inline]
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
  #[inline]
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
impl core::fmt::Display for SrcID {
  #[inline]
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    core::fmt::Display::fmt(&self.get_src_file().path().display(), f)
  }
}
impl Default for SrcID {
  #[inline]
  fn default() -> Self {
    SrcID::from(SrcFile::in_memory(""))
  }
}
