/// * `make_global_id!(StructName);`
#[macro_export]
macro_rules! make_global_id {
  (
    $(#[$meta:meta])*
    $name:ident
  ) => {
    $(#[$meta])*
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[repr(transparent)]
    pub struct $name(::core::num::NonZeroU32);
    impl $name {
      #[inline]
      pub fn try_new() -> Option<Self> {
        static NEXT_ID: ::core::sync::atomic::AtomicU32 =
          ::core::sync::atomic::AtomicU32::new(1);
        ::core::num::NonZeroU32::new(
          NEXT_ID.fetch_add(1, ::core::sync::atomic::Ordering::Relaxed),
        )
        .map(Self)
      }

      #[inline]
      #[track_caller]
      #[allow(clippy::new_without_default)]
      pub fn new() -> Self {
        Self::try_new().expect("exhausted the available id values!")
      }

      /// Unpack to a raw `u32` value.
      #[inline]
      #[must_use]
      pub const fn as_u32(self) -> u32 {
        self.0.get()
      }
    }
  };
}
