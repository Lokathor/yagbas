use std::{
  num::NonZeroU32,
  path::{Path, PathBuf},
  sync::{
    OnceLock, PoisonError, RwLock,
    atomic::{AtomicU32, Ordering},
  },
};

use bimap::BiHashMap;

type BiMapPath =
  BiHashMap<PathId, &'static Path, fnv::FnvBuildHasher, fnv::FnvBuildHasher>;

static NEXT_OS_STR_ID: AtomicU32 = AtomicU32::new(1);

static OS_STR_CACHE: OnceLock<RwLock<BiMapPath>> = OnceLock::new();

/// Works like [StrId], but for a [Path] value instead.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct PathId(NonZeroU32);
impl PathId {
  #[inline]
  fn try_new() -> Option<Self> {
    NonZeroU32::new(NEXT_OS_STR_ID.fetch_add(1, Ordering::Relaxed)).map(Self)
  }

  #[inline]
  #[track_caller]
  fn new() -> Self {
    Self::try_new().expect("exhausted the available id values!")
  }

  /// Unpack to a raw `u32` value.
  #[inline]
  #[must_use]
  pub const fn as_u32(self) -> u32 {
    self.0.get()
  }

  /// Gets the [Path] for this Id.
  #[inline]
  #[must_use]
  pub fn as_path(self) -> &'static Path {
    let rw_lock =
      OS_STR_CACHE.get_or_init(|| RwLock::new(BiMapPath::default()));
    let read = rw_lock.read().unwrap_or_else(PoisonError::into_inner);
    read.get_by_left(&self).unwrap_or(&Path::new(""))
  }
}

impl core::fmt::Debug for PathId {
  #[inline]
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> core::fmt::Result {
    core::fmt::Debug::fmt(&self.as_path(), f)
  }
}

impl core::fmt::Display for PathId {
  #[inline]
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> core::fmt::Result {
    core::fmt::Display::fmt(&self.as_path().display(), f)
  }
}

impl From<Box<Path>> for PathId {
  #[inline]
  fn from(value: Box<Path>) -> Self {
    let s: &Path = &value;
    let rw_lock =
      OS_STR_CACHE.get_or_init(|| RwLock::new(BiMapPath::default()));
    let read = rw_lock.read().unwrap_or_else(PoisonError::into_inner);
    if let Some(id) = read.get_by_right(s) {
      *id
    } else {
      drop(read);
      let mut write = rw_lock.write().unwrap_or_else(PoisonError::into_inner);
      if let Some(id) = write.get_by_right(s) {
        *id
      } else {
        let id: PathId = PathId::new();
        let leaked: &'static Path = Box::leak(value);
        write.insert(id, leaked);
        id
      }
    }
  }
}

impl<'a> From<&'a Path> for PathId {
  #[inline]
  fn from(s: &'a Path) -> Self {
    let rw_lock =
      OS_STR_CACHE.get_or_init(|| RwLock::new(BiMapPath::default()));
    let read = rw_lock.read().unwrap_or_else(PoisonError::into_inner);
    if let Some(id) = read.get_by_right(&s) {
      *id
    } else {
      drop(read);
      let mut write = rw_lock.write().unwrap_or_else(PoisonError::into_inner);
      if let Some(id) = write.get_by_right(s) {
        *id
      } else {
        let id: PathId = PathId::new();
        let leaked: &'static Path =
          Box::leak(s.to_path_buf().into_boxed_path());
        write.insert(id, leaked);
        id
      }
    }
  }
}

impl From<PathBuf> for PathId {
  #[inline]
  fn from(s: PathBuf) -> Self {
    let rw_lock =
      OS_STR_CACHE.get_or_init(|| RwLock::new(BiMapPath::default()));
    let read = rw_lock.read().unwrap_or_else(PoisonError::into_inner);
    if let Some(id) = read.get_by_right(s.as_path()) {
      *id
    } else {
      drop(read);
      let mut write = rw_lock.write().unwrap_or_else(PoisonError::into_inner);
      if let Some(id) = write.get_by_right(s.as_path()) {
        *id
      } else {
        let id: PathId = PathId::new();
        let leaked: &'static Path = Box::leak(s.into_boxed_path());
        write.insert(id, leaked);
        id
      }
    }
  }
}

impl AsRef<Path> for PathId {
  #[inline]
  fn as_ref(&self) -> &'static Path {
    self.as_path()
  }
}

impl Default for PathId {
  #[inline]
  fn default() -> Self {
    Self::from(Path::new(""))
  }
}
