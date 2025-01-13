use super::*;

/// A `Rec`ursive-`impl`-friendly extension trait of [`InternalIterator`].
///
/// Whenever `impl`ementing [`InternalIterator`] is to be done by recurse-delegating
/// to *multiple* sub-fields, then the `mut f: F = impl FnMut(…)` visitor cannot be used
/// by value, and a `&mut f` borrow of it needs to be used.
///
/// But, given the recursion, if the child field is of a type which may ultimately
/// contain the parent (as is often the case with mutually recursive AST definitions),
/// it means we will end up back at our original function call, but with `F` having
/// become `&mut F`.
///
/// And even if this recursion will terminate at runtime, the type system
/// *and codegen* / `<F>`-generic-code monorphization stages don't know of this
/// (similar problem to that of a recursive type).
///
/// This means that codegen will try to monomorphize the code for `<F>`, then for `<&mut F>`,
/// then for `<&mut &mut F>`, _etc._, _ad nauseam_, since there is no termination within the
/// type system.
///
/// This issue, called a "polymorphization error", causes compilation (codegen) to fail.
///
/// The solution, then, is to start off a reborrowable `&mut F` arg to begin with.
/// That way, the recursive stage with *multiple* accesses can keep using this arg
/// (reborrows of `&mut F`).
///
/// Granted, we are technically back at a similar situation (codegen for `&'r mut F`
/// ultimately requires codegen for `&'slightly_shorter mut F`, and so on), but this
/// is fine since lifetimes do not affect codegen).
///
/// But [`InternalIterator`] offers no such method.
///
/// Hence this helper "extension" trait, simply offering such a method, alongside a helper
/// macro to play the role of the blanket impl (which cannot be written due to orphan rules)
/// from which the proper [`InternalIterator`] `impl` can be derived (by delegating to our
/// well-defined `&mut F` case).
pub trait InternalIteratorRec: InternalIterator<Item = ItemRec<Self>> {
  type Item;
  fn try_for_each_rec<R, F>(self, f: &mut F) -> ControlFlow<R>
  where
    F: FnMut(ItemRec<Self>) -> ControlFlow<R>;
}

pub(crate) type ItemRec<T> = <T as InternalIteratorRec>::Item;

macro_rules! internal_iterator_rec_guts {
  () => {
    type Item = <Self as InternalIteratorRec>::Item;

    fn try_for_each<R, F>(self, f: F) -> ControlFlow<R>
    where
      F: FnMut(Self::Item) -> ControlFlow<R>,
    {
      self.try_for_each_rec(&mut { f })
    }
  };
}
pub(crate) use internal_iterator_rec_guts;

#[rustfmt::skip]
macro_rules! adhoc_internal_iterator_rec {(
  $lt:lifetime,
  $value:expr,
  |$($this:ident)+ : $T:ty, $yield:ident $(,)?| -> $ItemTy:ty
    $body:block
  $(,)?
) => ({
  struct AdhocInternalIteratorRec<$lt>($T);
  impl<$lt> $crate::InternalIteratorRec for AdhocInternalIteratorRec<$lt> {
    type Item = $ItemTy;

    fn try_for_each_rec<R, F>(self, $yield: &mut F) -> ControlFlow<R>
    where
      F: FnMut(ItemRec<Self>) -> ControlFlow<R>,
    {
      let Self($($this)+) = self;
      $body
      ControlFlow::Continue(())
    }
  }
  impl InternalIterator for AdhocInternalIteratorRec<'_> {
    $crate::internal_iterator_rec_guts!();
  }
  AdhocInternalIteratorRec($value)
})}
pub(crate) use adhoc_internal_iterator_rec;
