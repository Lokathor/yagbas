use super::*;

/// Generic typed value paired with a `SimpleSpan`
#[derive(Clone, Copy, Display)]
#[display("{_0}")]
pub struct S<T>(pub T, pub SimpleSpan);
impl<T> S<T> {
  pub fn from_extras<'src, 'b, I, E>(
    t: T, ex: &mut MapExtra<'src, 'b, I, E>,
  ) -> Self
  where
    I: Input<'src, Span = SimpleSpan>,
    E: ParserExtra<'src, I>,
  {
    Self(t, ex.span())
  }
}
impl<T> core::fmt::Debug for S<T>
where
  T: core::fmt::Debug,
{
  #[inline]
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    core::fmt::Debug::fmt(&self.0, f)
  }
}
