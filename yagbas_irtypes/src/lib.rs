#![forbid(unsafe_code)]

use str_id::StrID;
use yagbas_asttypes::S;
use yagbas_srcfiletypes::{FileData, FileID};

#[derive(Debug, Clone)]
pub struct IrBitstruct {
  pub name: S<StrID>,
  pub file_id: FileID,
  pub fields: [Option<S<StrID>>; 8],
}

#[derive(Debug, Clone)]
pub struct IrStruct {
  pub name: S<StrID>,
  pub file_id: FileID,
  pub fields: Vec<(S<StrID>, S<StrID>)>,
}
