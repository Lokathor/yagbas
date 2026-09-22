use core::num::NonZeroU64;

/// Stores a non-maximum `u64` value, with a niche at `u64::MAX`.
///
/// Internally this stores a [NonZeroU64], with [try_new](NonMaxU64::try_new)
/// applying a +1 and [get](NonMaxU64::get) applying a -1.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct NonMaxU64(NonZeroU64);
impl NonMaxU64 {
  /// ### Failure
  /// * If the input is `u64::MAX`
  pub const fn try_new(x: u64) -> Option<Self> {
    match NonZeroU64::new(x + 1) {
      Some(nz) => Some(Self(nz)),
      None => None,
    }
  }
  pub const fn get(self) -> u64 {
    self.0.get() - 1
  }
}
impl Default for NonMaxU64 {
  fn default() -> Self {
    Self::try_new(0).unwrap()
  }
}
impl core::fmt::Debug for NonMaxU64 {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> core::fmt::Result {
    core::fmt::Debug::fmt(&self.get(), f)
  }
}
