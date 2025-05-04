#![forbid(unsafe_code)]
#![allow(unused_mut)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

use str_id::StrID;
use yagbas_asttypes::{AstBitStruct, AstStruct, S};
use yagbas_srcfiletypes::FileID;

#[derive(Debug, Clone, Copy)]
pub enum IrTranslateError {
  IllegalBitPosition { file_id: FileID, position: S<StrID> },
  BitPositionDuplicate { file_id: FileID, first: S<StrID>, duplicate: S<StrID> },
  FieldNameDuplicate { file_id: FileID, first: S<StrID>, duplicate: S<StrID> },
}

#[derive(Debug, Clone, Copy)]
pub struct IrBitStruct {
  pub name: S<StrID>,
  pub file_id: FileID,
  pub fields: [Option<S<StrID>>; 8],
}

impl TryFrom<&AstBitStruct> for IrBitStruct {
  // we could have more than one error in a single conversion so we probably
  // want to have the error be like a "list of problems" of some kind?
  type Error = Vec<IrTranslateError>;

  fn try_from(ast: &AstBitStruct) -> Result<Self, Self::Error> {
    let name = ast.name;
    let file_id = ast.file_id;
    let mut fields: [Option<S<StrID>>; 8] = [None; 8];
    let mut errors = Vec::new();
    for (field_name, bit) in ast.fields.iter() {
      if let Some(found_first) = fields
        .iter()
        .find(|bit_name| bit_name.map(|S(name, _)| name) == Some(field_name.0))
      {
        errors.push(IrTranslateError::FieldNameDuplicate {
          file_id,
          first: found_first.unwrap(),
          duplicate: *field_name,
        });
        continue;
      }
      // QUESTION: should we support non-decimal numeric literals here? eg: we
      // could support `$1` as an alternative to `1`
      let i = match bit.0.as_str() {
        "0" => 0,
        "1" => 1,
        "2" => 2,
        "3" => 3,
        "4" => 4,
        "5" => 5,
        "6" => 6,
        "7" => 7,
        _ => {
          errors.push(IrTranslateError::IllegalBitPosition {
            file_id,
            position: *bit,
          });
          continue;
        }
      };
      if let Some(first) = fields[i] {
        errors.push(IrTranslateError::BitPositionDuplicate {
          file_id,
          first,
          duplicate: *field_name,
        });
        continue;
      }
      fields[i] = Some(*field_name);
    }
    if errors.is_empty() {
      Ok(Self { name, file_id, fields })
    } else {
      Err(errors)
    }
  }
}

#[derive(Debug, Clone)]
pub struct IrStruct {
  pub name: S<StrID>,
  pub file_id: FileID,
  pub fields: Vec<(S<StrID>, S<StrID>)>,
}

impl TryFrom<&AstStruct> for IrStruct {
  type Error = Vec<IrTranslateError>;
  fn try_from(value: &AstStruct) -> Result<Self, Self::Error> {
    let name = value.name;
    let file_id = value.file_id;
    let mut fields: Vec<(S<StrID>, S<StrID>)> = Vec::new();
    let mut errors = Vec::new();
    for (field_name, field_ty) in value.fields.iter() {
      if let Some(found_first) =
        fields.iter().find(|(name, _)| name.0 == field_name.0)
      {
        errors.push(IrTranslateError::FieldNameDuplicate {
          file_id,
          first: found_first.0,
          duplicate: *field_name,
        });
        continue;
      } else {
        fields.push((*field_name, *field_ty));
      }
    }
    if errors.is_empty() {
      Ok(Self { name, file_id, fields })
    } else {
      Err(errors)
    }
  }
}
