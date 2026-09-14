use std::num::NonZeroU32;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct NonMaxU32(NonZeroU32);
impl NonMaxU32 {
  pub const fn try_new(x: u32) -> Option<Self> {
    match NonZeroU32::new(x ^ u32::MAX) {
      Some(nz) => Some(Self(nz)),
      None => None,
    }
  }
  pub const fn get(self) -> u32 {
    self.0.get() ^ u32::MAX
  }
}
impl Default for NonMaxU32 {
  fn default() -> Self {
    Self::try_new(0).unwrap()
  }
}
impl PartialOrd for NonMaxU32 {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}
impl Ord for NonMaxU32 {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.get().cmp(&other.get())
  }
}
impl core::hash::Hash for NonMaxU32 {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.get().hash(state);
  }
}
