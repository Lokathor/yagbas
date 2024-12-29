use bimap::BiMap;
use core::{
  num::NonZeroUsize,
  sync::atomic::{AtomicUsize, Ordering},
};
use std::sync::{OnceLock, PoisonError, RwLock};

pub type StaticStr = &'static str;

static NEXT_STR_ID: AtomicUsize = AtomicUsize::new(1);

static STR_CACHE: OnceLock<RwLock<BiMap<StrID, StaticStr>>> = OnceLock::new();

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct StrID(NonZeroUsize);
impl StrID {
  #[inline]
  fn try_new() -> Option<Self> {
    NonZeroUsize::new(NEXT_STR_ID.fetch_add(1, Ordering::Relaxed)).map(Self)
  }
  #[inline]
  #[track_caller]
  fn new() -> Self {
    Self::try_new().expect("exhausted the available StrID values!")
  }
  #[inline]
  #[must_use]
  pub const fn as_usize(self) -> usize {
    self.0.get()
  }
  #[inline]
  #[must_use]
  pub fn as_str(self) -> StaticStr {
    let rw_lock = STR_CACHE.get_or_init(|| RwLock::new(BiMap::new()));
    let read = rw_lock.read().unwrap_or_else(PoisonError::into_inner);
    read.get_by_left(&self).unwrap_or(&"")
  }
}
impl core::fmt::Debug for StrID {
  #[inline]
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> core::fmt::Result {
    core::fmt::Debug::fmt(&self.as_str(), f)
  }
}
impl core::fmt::Display for StrID {
  #[inline]
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> core::fmt::Result {
    core::fmt::Display::fmt(&self.as_str(), f)
  }
}
impl<'a> From<&'a str> for StrID {
  /// Convert any `&str` into its ID, automatically interning it if necessary.
  #[inline]
  fn from(s: &'a str) -> Self {
    let rw_lock = STR_CACHE.get_or_init(|| RwLock::new(BiMap::new()));
    let read = rw_lock.read().unwrap_or_else(PoisonError::into_inner);
    if let Some(id) = read.get_by_right(&s) {
      *id
    } else {
      drop(read);
      let mut write = rw_lock.write().unwrap_or_else(PoisonError::into_inner);
      // It's *possible* that the string was inserted after we dropped the
      // reader before we acquired the writer, so we must check a second
      // time.
      if let Some(id) = write.get_by_right(s) {
        *id
      } else {
        let id: StrID = StrID::new();
        let leaked: StaticStr = Box::leak(s.to_string().into_boxed_str());
        write.insert(id, leaked);
        id
      }
    }
  }
}
impl From<String> for StrID {
  /// Convert any `&str` into its ID, automatically interning it if necessary.
  #[inline]
  fn from(s: String) -> Self {
    let rw_lock = STR_CACHE.get_or_init(|| RwLock::new(BiMap::new()));
    let read = rw_lock.read().unwrap_or_else(PoisonError::into_inner);
    if let Some(id) = read.get_by_right(s.as_str()) {
      *id
    } else {
      drop(read);
      let mut write = rw_lock.write().unwrap_or_else(PoisonError::into_inner);
      // It's *possible* that the string was inserted after we dropped the
      // reader before we acquired the writer, so we must check a second
      // time.
      if let Some(id) = write.get_by_right(s.as_str()) {
        *id
      } else {
        let id: StrID = StrID::new();
        let leaked: StaticStr = Box::leak(s.into_boxed_str());
        write.insert(id, leaked);
        id
      }
    }
  }
}
impl AsRef<str> for StrID {
  #[inline]
  #[must_use]
  fn as_ref(&self) -> &str {
    self.as_str()
  }
}
impl Default for StrID {
  #[inline]
  #[must_use]
  fn default() -> Self {
    Self::from(<&str>::default())
  }
}
