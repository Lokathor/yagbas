use core::marker::PhantomData;

use str_id::StrId;

#[derive(Debug, Clone)]
pub struct AstParser<'a> {
  pub phantom: PhantomData<&'a str>,
  pub file_origin: StrId,
}
