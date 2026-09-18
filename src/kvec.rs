use std::marker::PhantomData;

/// # Safety
/// * You are not allowed to implement this trait.
/// * Declare new key types using the [make_key!] macro.
pub unsafe trait KVecKey {
  /// change the key into a raw value.
  fn to_u32(self) -> u32;

  /// # Safety
  /// * You cannot call this function.
  unsafe fn from_u32(u: u32) -> Self;
}

/// Makes a new type of key for use with a [KVec].
#[macro_export]
macro_rules! make_key {
  ($name:ident) => {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[repr(transparent)]
    pub struct $name(u32);
    unsafe impl KVecKey for $name {
      fn to_u32(self) -> u32 {
        self.0
      }
      unsafe fn from_u32(u: u32) -> Self {
        $name(u)
      }
    }
  };
}

#[derive(Default)]
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
  pub fn push(&mut self, value: V) -> K
  where
    K: KVecKey,
  {
    let u = u32::try_from(self.data.len()).expect("IndexList overflow!");
    let i = unsafe { <K as KVecKey>::from_u32(u) };
    self.data.push(value);
    i
  }
  pub fn get(&self, index: K) -> Option<&V>
  where
    K: KVecKey,
  {
    let u = index.to_u32();
    debug_assert!(usize::try_from(u).is_ok());
    self.data.get(u as usize)
  }
  pub fn get_mut(&mut self, index: K) -> Option<&mut V>
  where
    K: KVecKey,
  {
    let u = index.to_u32();
    debug_assert!(usize::try_from(u).is_ok());
    self.data.get_mut(u as usize)
  }
  pub fn iter(&self) -> impl Iterator<Item = (K, &V)>
  where
    K: KVecKey,
  {
    self
      .data
      .iter()
      .enumerate()
      .map(|(u, v)| (unsafe { <K as KVecKey>::from_u32(u as u32) }, v))
  }
  pub fn iter_mut(&mut self) -> impl Iterator<Item = (K, &mut V)>
  where
    K: KVecKey,
  {
    self
      .data
      .iter_mut()
      .enumerate()
      .map(|(u, v)| (unsafe { <K as KVecKey>::from_u32(u as u32) }, v))
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
  fn index(&self, index: K) -> &Self::Output {
    let u = index.to_u32();
    debug_assert!(usize::try_from(u).is_ok());
    &self.data[u as usize]
  }
}
impl<K, V> core::ops::IndexMut<K> for KVec<K, V>
where
  K: KVecKey,
{
  fn index_mut(&mut self, index: K) -> &mut Self::Output {
    let u = index.to_u32();
    debug_assert!(usize::try_from(u).is_ok());
    &mut self.data[u as usize]
  }
}
