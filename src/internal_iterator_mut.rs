use super::*;

pub trait InternalIteratorMut: InternalIterator<Item = Self::ItemMut> {
  type ItemMut;
  fn try_for_each_mut<R, F>(self, f: &mut F) -> ControlFlow<R>
  where
    F: FnMut(Self::Item) -> ControlFlow<R>;
}

#[macro_export]
macro_rules! internal_iterator_mut_guts {
  () => {
    type Item = <Self as InternalIteratorMut>::ItemMut;

    fn try_for_each<R, F>(self, f: F) -> ControlFlow<R>
    where
      F: FnMut(Self::Item) -> ControlFlow<R>,
    {
      self.try_for_each_mut(&mut { f })
    }
  };
}
