#![forbid(unsafe_code)]
#![allow(unused_mut)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

use str_id::StrID;
use yagbas_asttypes::S;
use yagbas_srcfiletypes::FileID;

mod ir_bitstruct;
pub use ir_bitstruct::*;

mod ir_struct;
pub use ir_struct::*;

#[derive(Debug, Clone, Copy)]
pub enum IrTranslateError {
  IllegalBitPosition { file_id: FileID, position: S<StrID> },
  BitPositionDuplicate { file_id: FileID, first: S<StrID>, duplicate: S<StrID> },
  FieldNameDuplicate { file_id: FileID, first: S<StrID>, duplicate: S<StrID> },
}
