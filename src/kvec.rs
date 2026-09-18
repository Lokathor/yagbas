use std::marker::PhantomData;

/// A trait for KVec key types.
///
/// Declare new key types using the [make_key!] macro.
pub trait KVecKey {
  /// change the key into a raw value.
  fn to_u32(self) -> u32;

  /// make a key from a raw value.
  fn from_u32(u: u32) -> Self;
}

/// Makes a new type of key for use with a [KVec].
///
/// Just pass the name you'd like on your key struct:
/// ```
/// # use yagbas::make_key;
/// make_key!(ItemId);
/// ```
#[macro_export]
macro_rules! make_key {
  ($name:ident) => {
    #[derive(
      Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash,
    )]
    #[repr(transparent)]
    pub struct $name($crate::non_max_u32::NonMaxU32);
    impl $crate::kvec::KVecKey for $name {
      fn to_u32(self) -> u32 {
        self.0.get()
      }
      /// ## Panics
      /// * If you pass `u32::MAX`
      fn from_u32(u: u32) -> Self {
        $name(
          $crate::non_max_u32::NonMaxU32::try_new(u)
            .expect("u32::MAX can't be a key!"),
        )
      }
    }
  };
}

pub type KVecRefIter<'a, K, V> = core::iter::Map<
  core::iter::Enumerate<core::slice::Iter<'a, V>>,
  for<'b> fn((usize, &'a V)) -> (K, &'a V),
>;

pub type KVecMutIter<'a, K, V> = core::iter::Map<
  core::iter::Enumerate<core::slice::IterMut<'a, V>>,
  for<'b> fn((usize, &'a mut V)) -> (K, &'a mut V),
>;

pub type KVecIter<K, V> = core::iter::Map<
  core::iter::Enumerate<std::vec::IntoIter<V>>,
  for<'b> fn((usize, V)) -> (K, V),
>;

/// Works like a `Vec` with new-typed 32-bit keys instead of `usize` indexes.
///
/// Highly likely to perform better than a `HashMap`.
///
/// Make a key type using the [make_key!] macro.
#[derive(Default, Clone)]
pub struct KVec<K, V> {
  phantom: PhantomData<K>,
  data: Vec<V>,
}
impl<K, V> core::fmt::Debug for KVec<K, V>
where
  V: core::fmt::Debug,
{
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    core::fmt::Debug::fmt(&self.data, f)
  }
}
impl<K, V> KVec<K, V> {
  /// Gets the key that will result from the next [push](Self::push) operation.
  pub fn next_key(&self) -> K
  where
    K: KVecKey,
  {
    let u = u32::try_from(self.data.len()).expect("KVec overflow!");
    <K as KVecKey>::from_u32(u)
  }
  /// Push an item to the KVec and get back the key associated with it.
  pub fn push(&mut self, value: V) -> K
  where
    K: KVecKey,
  {
    let key = self.next_key();
    self.data.push(value);
    key
  }
  /// ## Failure
  /// * If the key is not in range.
  pub fn get(&self, key: K) -> Option<&V>
  where
    K: KVecKey,
  {
    let u = key.to_u32();
    self.data.get(u as usize)
  }
  /// ## Failure
  /// * If the key is not in range.
  pub fn get_mut(&mut self, key: K) -> Option<&mut V>
  where
    K: KVecKey,
  {
    let u = key.to_u32();
    self.data.get_mut(u as usize)
  }
  pub fn iter(&self) -> KVecRefIter<'_, K, V>
  where
    K: KVecKey,
  {
    self.data.iter().enumerate().map(mapper)
  }
  pub fn iter_mut(&mut self) -> KVecMutIter<'_, K, V>
  where
    K: KVecKey,
  {
    self.data.iter_mut().enumerate().map(mapper_mut)
  }
  pub fn get_disjoint_mut<const N: usize>(
    &mut self, indices: [K; N],
  ) -> Result<[&mut V; N], core::slice::GetDisjointMutError>
  where
    K: KVecKey,
  {
    self.data.get_disjoint_mut(indices.map(|k| k.to_u32() as usize))
  }
}
impl<K, V> core::ops::Index<K> for KVec<K, V>
where
  K: KVecKey,
{
  type Output = V;
  /// ## Panics
  /// * If the key is not in range
  fn index(&self, key: K) -> &Self::Output {
    let u = key.to_u32();
    debug_assert!(usize::try_from(u).is_ok());
    &self.data[u as usize]
  }
}
impl<K, V> core::ops::IndexMut<K> for KVec<K, V>
where
  K: KVecKey,
{
  /// ## Panics
  /// * If the key is not in range
  fn index_mut(&mut self, key: K) -> &mut Self::Output {
    let u = key.to_u32();
    debug_assert!(usize::try_from(u).is_ok());
    &mut self.data[u as usize]
  }
}

impl<K, V> IntoIterator for KVec<K, V>
where
  K: KVecKey,
{
  type Item = (K, V);
  type IntoIter = KVecIter<K, V>;
  /// ```
  /// # use yagbas::kvec::KVec;
  /// # use yagbas::make_key;
  /// make_key!(ItemId);
  /// let x: KVec<ItemId, u64> = KVec::default();
  /// for (k, v) in x {
  ///   let _k: ItemId = k;
  ///   let _v: u64 = v;
  /// }
  /// ```
  fn into_iter(self) -> Self::IntoIter {
    self.data.into_iter().enumerate().map(mapper_owned)
  }
}
fn mapper_owned<K, V>((u, v): (usize, V)) -> (K, V)
where
  K: KVecKey,
{
  (<K as KVecKey>::from_u32(u as u32), v)
}

impl<'a, K, V> IntoIterator for &'a KVec<K, V>
where
  K: KVecKey,
{
  type Item = (K, &'a V);
  type IntoIter = KVecRefIter<'a, K, V>;
  /// ```
  /// # use yagbas::kvec::KVec;
  /// # use yagbas::make_key;
  /// make_key!(ItemId);
  /// let x: KVec<ItemId, u64> = KVec::default();
  /// for (k, v) in &x {
  ///   let _k: ItemId = k;
  ///   let _v: &u64 = v;
  /// }
  /// ```
  fn into_iter(self) -> Self::IntoIter {
    self.data.iter().enumerate().map(mapper)
  }
}
fn mapper<K, V>((u, v): (usize, &V)) -> (K, &V)
where
  K: KVecKey,
{
  (<K as KVecKey>::from_u32(u as u32), v)
}

impl<'a, K, V> IntoIterator for &'a mut KVec<K, V>
where
  K: KVecKey,
{
  type Item = (K, &'a mut V);
  type IntoIter = KVecMutIter<'a, K, V>;
  /// ```
  /// # use yagbas::kvec::KVec;
  /// # use yagbas::make_key;
  /// make_key!(ItemId);
  /// let mut x: KVec<ItemId, u64> = KVec::default();
  /// for (k, v) in &mut x {
  ///   let _k: ItemId = k;
  ///   let _v: &mut u64 = v;
  /// }
  /// ```
  fn into_iter(self) -> Self::IntoIter {
    self.data.iter_mut().enumerate().map(mapper_mut)
  }
}
fn mapper_mut<K, V>((u, v): (usize, &mut V)) -> (K, &mut V)
where
  K: KVecKey,
{
  (<K as KVecKey>::from_u32(u as u32), v)
}
