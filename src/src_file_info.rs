use std::{
  num::NonZeroUsize,
  path::{Path, PathBuf},
  sync::{
    atomic::{AtomicUsize, Ordering},
    OnceLock, PoisonError, RwLock,
  },
};

use bimap::BiMap;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SrcFileInfo {
  canonical_path: PathBuf,
  file_text: String,
  line_bytes: Vec<usize>,
}
impl SrcFileInfo {
  pub fn read_path<ARP>(arp: ARP) -> Result<Self, std::io::Error>
  where
    ARP: AsRef<Path>,
  {
    let path = arp.as_ref();
    let canonical_path = path.canonicalize()?;
    let file_text = std::fs::read_to_string(&canonical_path)?;
    let mut line_bytes = Vec::new();
    let mut total = 0;
    for line in file_text.lines() {
      line_bytes.push(total);
      total += line.len();
    }
    Ok(Self { canonical_path, file_text, line_bytes })
  }

  #[inline]
  #[must_use]
  pub fn line_col(&self, byte: usize) -> (usize, usize) {
    match self.line_bytes.binary_search(&byte) {
      Ok(line) => (line, 0),
      Err(could_be) => {
        let line = could_be.wrapping_sub(1);
        let line_start = self.line_bytes.get(line).copied().unwrap_or(0);
        let col = byte.saturating_sub(line_start);
        (line, col)
      }
    }
  }
}

static NEXT_INFO_ID: AtomicUsize = AtomicUsize::new(1);

static INFO_CACHE: OnceLock<RwLock<BiMap<SrcFileInfoID, &'static SrcFileInfo>>> =
  OnceLock::new();

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct SrcFileInfoID(NonZeroUsize);
impl SrcFileInfoID {
  #[inline]
  fn try_new() -> Option<Self> {
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
  pub fn as_info(self) -> &'static SrcFileInfo {
    let rw_lock = INFO_CACHE.get_or_init(|| RwLock::new(BiMap::new()));
    let read = rw_lock.read().unwrap_or_else(PoisonError::into_inner);
    read.get_by_left(&self).unwrap()
  }
}
impl<'a> From<&'a SrcFileInfo> for SrcFileInfoID {
  /// Convert any `&SrcFileInfo` into its ID, automatically interning it if
  /// necessary.
  fn from(s: &'a SrcFileInfo) -> Self {
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
        let leaked: &'static SrcFileInfo = Box::leak(Box::new(s.clone()));
        write.insert(id, leaked);
        id
      }
    }
  }
}
impl From<SrcFileInfo> for SrcFileInfoID {
  /// Convert any `SrcFileInfo` into its ID, automatically interning it if
  /// necessary.
  ///
  /// Prefer this if you *expect* to need to intern the value, it will avoid a
  /// clone.
  fn from(s: SrcFileInfo) -> Self {
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
        let leaked: &'static SrcFileInfo = Box::leak(Box::new(s));
        write.insert(id, leaked);
        id
      }
    }
  }
}
