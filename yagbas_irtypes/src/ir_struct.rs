use str_id::StrID;
use yagbas_asttypes::{AstStruct, S};
use yagbas_srcfiletypes::FileID;

use crate::IrTranslateError;

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
