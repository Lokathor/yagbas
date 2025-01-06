use super::*;

pub trait InternalIteratorRef: InternalIterator<Item = Self::ItemRef> {
  type ItemRef;
  fn try_for_each_ref<R, F>(self, f: &mut F) -> ControlFlow<R>
  where
    F: FnMut(Self::Item) -> ControlFlow<R>;
}

#[macro_export]
macro_rules! internal_iterator_ref_guts {
  () => {
    type Item = <Self as InternalIteratorRef>::ItemRef;

    fn try_for_each<R, F>(self, f: F) -> ControlFlow<R>
    where
      F: FnMut(Self::Item) -> ControlFlow<R>,
    {
      self.try_for_each_ref(&mut { f })
    }
  };
}
