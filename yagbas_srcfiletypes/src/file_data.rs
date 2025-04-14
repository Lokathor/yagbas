use std::{
  num::NonZeroUsize,
  path::{Path, PathBuf},
  sync::{
    OnceLock, PoisonError, RwLock,
    atomic::{AtomicUsize, Ordering},
  },
};

use ariadne::{Cache, sources};
use bimap::BiHashMap;

static NEXT_FILE_ID: AtomicUsize = AtomicUsize::new(1);

static FILE_INFO_CACHE: OnceLock<
  RwLock<
    BiHashMap<
      FileID,
      &'static FileData,
      fnv::FnvBuildHasher,
      fnv::FnvBuildHasher,
    >,
  >,
> = OnceLock::new();

/// Newtype over [NonZeroUsize] that can look up the correct info.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct FileID(NonZeroUsize);
impl core::fmt::Display for FileID {
  #[inline]
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    core::fmt::Debug::fmt(self, f)
  }
}
impl FileID {
  #[inline]
  fn try_new() -> Option<Self> {
    NonZeroUsize::new(NEXT_FILE_ID.fetch_add(1, Ordering::Relaxed)).map(Self)
  }

  #[inline]
  #[track_caller]
  fn new() -> Self {
    Self::try_new().expect("exhausted the available StrID values!")
  }

  /// Unwraps the value into a raw `usize`.
  #[inline]
  #[must_use]
  pub const fn as_usize(self) -> usize {
    self.0.get()
  }

  /// Gets the file info associated with this ID value.
  #[inline]
  #[must_use]
  pub fn get_data(self) -> &'static FileData {
    let rw_lock =
      FILE_INFO_CACHE.get_or_init(|| RwLock::new(BiHashMap::default()));
    let read = rw_lock.read().unwrap_or_else(PoisonError::into_inner);
    read.get_by_left(&self).expect("FileInfo was not in the cache!")
  }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileData {
  id: FileID,
  path_buf: PathBuf,
  content: String,
}
impl FileData {
  #[inline]
  pub fn load<P: AsRef<Path>>(p: P) -> Result<&'static Self, std::io::Error> {
    let path_buf = p.as_ref().to_path_buf();
    let content = std::fs::read_to_string(&path_buf)?;
    let id = FileID::new();
    let file_data_ref = Box::leak(Box::new(FileData { id, path_buf, content }));
    let rw_lock =
      FILE_INFO_CACHE.get_or_init(|| RwLock::new(BiHashMap::default()));
    let mut write = rw_lock.write().unwrap_or_else(PoisonError::into_inner);
    write.insert(id, file_data_ref);
    Ok(file_data_ref)
  }
}

/// Generates a [Cache] of all files currently in the [FileData] system.
pub fn file_data_cache_sources() -> impl Cache<FileID> {
  let rw_lock =
    FILE_INFO_CACHE.get_or_init(|| RwLock::new(BiHashMap::default()));
  let read = rw_lock.read().unwrap_or_else(PoisonError::into_inner);
  let vec = read
    .iter()
    .map(|(id, info_ref)| (*id, info_ref.content.as_str()))
    .collect::<Vec<_>>();
  drop(read);
  sources(vec)
}
