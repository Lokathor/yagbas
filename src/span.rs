use crate::non_max_u64::NonMaxU64;

/// A span within a source file.
///
/// Because we use `u32` positions, Yagbas source files are limited in size to
/// 4GB, which is entirely reasonable.
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span(NonMaxU64);
impl Span {
  /// Makes the new span.
  pub fn new(start: u32, end: u32) -> Self {
    if start == u32::MAX && end == u32::MAX {
      panic!("Span overflow")
    } else {
      let total: u64 = bytemuck::cast([start, end]);
      Self(NonMaxU64::try_new(total).unwrap())
    }
  }
  /// Convert the span to a [Range], so you can index with it.
  pub fn as_range(self) -> core::ops::Range<usize> {
    let [start, end]: [u32; 2] = bytemuck::cast(self.0.get());
    (start as usize)..(end as usize)
  }
}
impl core::fmt::Debug for Span {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    core::fmt::Debug::fmt(&self.as_range(), f)
  }
}
