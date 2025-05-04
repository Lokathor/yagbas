use str_id::StrID;
use yagbas_asttypes::{AstBitStruct, S};
use yagbas_srcfiletypes::FileID;

use crate::IrTranslateError;

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
