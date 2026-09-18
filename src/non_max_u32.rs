use core::num::NonZeroU32;

/// Stores a non-maximum `u32` value, with a niche at `u32::MAX`.
///
/// Internally this stores a [NonZeroU32], with [try_new](NonMaxU32::try_new)
/// applying a +1 and [get](NonMaxU32::get) applying a -1.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct NonMaxU32(NonZeroU32);
impl NonMaxU32 {
  /// ### Failure
  /// * If the input is `u32::MAX`
  pub const fn try_new(x: u32) -> Option<Self> {
    match NonZeroU32::new(x + 1) {
      Some(nz) => Some(Self(nz)),
      None => None,
    }
  }
  pub const fn get(self) -> u32 {
    self.0.get() - 1
  }
}
impl Default for NonMaxU32 {
  fn default() -> Self {
    Self::try_new(0).unwrap()
  }
}
