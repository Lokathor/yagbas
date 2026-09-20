use std::{
  ffi::{OsStr, OsString},
  num::NonZeroU32,
  sync::{
    OnceLock, PoisonError, RwLock,
    atomic::{AtomicU32, Ordering},
  },
};

use bimap::BiHashMap;

type BiMapOsStr =
  BiHashMap<OsStrId, &'static OsStr, fnv::FnvBuildHasher, fnv::FnvBuildHasher>;

static NEXT_OS_STR_ID: AtomicU32 = AtomicU32::new(1);

static OS_STR_CACHE: OnceLock<RwLock<BiMapOsStr>> = OnceLock::new();

/// Works like [StrId], but for an [OsStr] value instead.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct OsStrId(NonZeroU32);
impl OsStrId {
  #[inline]
  fn try_new() -> Option<Self> {
    NonZeroU32::new(NEXT_OS_STR_ID.fetch_add(1, Ordering::Relaxed)).map(Self)
  }

  #[inline]
  #[track_caller]
  fn new() -> Self {
    Self::try_new().expect("exhausted the available id values!")
  }

  /// Unpack to a raw u32 value.
  #[inline]
  #[must_use]
  pub const fn as_u32(self) -> u32 {
    self.0.get()
  }

  /// Gets the [OsStr] for this Id.
  #[inline]
  #[must_use]
  pub fn as_os_str(self) -> &'static OsStr {
    let rw_lock =
      OS_STR_CACHE.get_or_init(|| RwLock::new(BiMapOsStr::default()));
    let read = rw_lock.read().unwrap_or_else(PoisonError::into_inner);
    read.get_by_left(&self).unwrap_or(&OsStr::new(""))
  }
}

impl core::fmt::Debug for OsStrId {
  #[inline]
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> core::fmt::Result {
    core::fmt::Debug::fmt(&self.as_os_str(), f)
  }
}

impl core::fmt::Display for OsStrId {
  #[inline]
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> core::fmt::Result {
    core::fmt::Display::fmt(&self.as_os_str().display(), f)
  }
}

impl From<Box<OsStr>> for OsStrId {
  #[inline]
  fn from(value: Box<OsStr>) -> Self {
    let s: &OsStr = &value;
    let rw_lock =
      OS_STR_CACHE.get_or_init(|| RwLock::new(BiMapOsStr::default()));
    let read = rw_lock.read().unwrap_or_else(PoisonError::into_inner);
    if let Some(id) = read.get_by_right(s) {
      *id
    } else {
      drop(read);
      let mut write = rw_lock.write().unwrap_or_else(PoisonError::into_inner);
      if let Some(id) = write.get_by_right(s) {
        *id
      } else {
        let id: OsStrId = OsStrId::new();
        let leaked: &'static OsStr = Box::leak(value);
        write.insert(id, leaked);
        id
      }
    }
  }
}

impl<'a> From<&'a OsStr> for OsStrId {
  #[inline]
  fn from(s: &'a OsStr) -> Self {
    let rw_lock =
      OS_STR_CACHE.get_or_init(|| RwLock::new(BiMapOsStr::default()));
    let read = rw_lock.read().unwrap_or_else(PoisonError::into_inner);
    if let Some(id) = read.get_by_right(&s) {
      *id
    } else {
      drop(read);
      let mut write = rw_lock.write().unwrap_or_else(PoisonError::into_inner);
      if let Some(id) = write.get_by_right(s) {
        *id
      } else {
        let id: OsStrId = OsStrId::new();
        let leaked: &'static OsStr =
          Box::leak(s.to_os_string().into_boxed_os_str());
        write.insert(id, leaked);
        id
      }
    }
  }
}

impl From<OsString> for OsStrId {
  #[inline]
  fn from(s: OsString) -> Self {
    let rw_lock =
      OS_STR_CACHE.get_or_init(|| RwLock::new(BiMapOsStr::default()));
    let read = rw_lock.read().unwrap_or_else(PoisonError::into_inner);
    if let Some(id) = read.get_by_right(s.as_os_str()) {
      *id
    } else {
      drop(read);
      let mut write = rw_lock.write().unwrap_or_else(PoisonError::into_inner);
      if let Some(id) = write.get_by_right(s.as_os_str()) {
        *id
      } else {
        let id: OsStrId = OsStrId::new();
        let leaked: &'static OsStr = Box::leak(s.into_boxed_os_str());
        write.insert(id, leaked);
        id
      }
    }
  }
}

impl AsRef<OsStr> for OsStrId {
  #[inline]
  fn as_ref(&self) -> &'static OsStr {
    self.as_os_str()
  }
}

impl Default for OsStrId {
  #[inline]
  fn default() -> Self {
    Self::from(<&OsStr>::default())
  }
}
