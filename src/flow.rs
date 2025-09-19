use super::*;
use core::sync::atomic::Ordering;
use core::sync::atomic::AtomicUsize;
use core::num::NonZeroUsize;
use str_id::StrID;

static NEXT_BLOCK_ID: AtomicUsize = AtomicUsize::new(1);

/// This is a newtype over a [NonZeroUsize].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct BlockID(NonZeroUsize);
impl BlockID {
  /// Makes a new ID.
  /// ## Failure
  /// If the next ID would be 0, this fails.
  #[inline]
  pub fn try_new() -> Option<Self> {
    NonZeroUsize::new(NEXT_BLOCK_ID.fetch_add(1, Ordering::Relaxed)).map(Self)
  }
  /// `BlockID::try_new().expect("...")`
  #[inline]
  #[track_caller]
  pub fn new() -> Self {
    Self::try_new().expect("exhausted the available BlockID values!")
  }

  /// Unwraps the value into a raw `usize`.
  #[inline]
  #[must_use]
  pub const fn as_usize(self) -> usize {
    self.0.get()
  }
}

#[derive(Debug, Clone)]
pub enum AstBBStep {
  Expr(Expr),
  Call(StrID),
  StatementError,
}

#[derive(Debug, Clone)]
pub enum Condition {
  Carry,
  Zero,
}

#[derive(Debug, Clone)]
pub enum AstBBFlow {
  Always(BlockID),
  Branch(Condition, BlockID, BlockID),
}

#[derive(Debug, Clone)]
pub struct AstBB {
  pub id: BlockID,
  pub steps: Vec<AstBBStep>,
  pub next: AstBBFlow,
}
