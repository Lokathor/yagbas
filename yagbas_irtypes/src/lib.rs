#![forbid(unsafe_code)]

use str_id::StrID;
use yagbas_asttypes::{AstBitStruct, S};
use yagbas_srcfiletypes::{FileData, FileID};

#[derive(Debug, Clone)]
pub struct IrBitStruct {
  pub name: S<StrID>,
  pub file_id: FileID,
  pub fields: [Option<S<StrID>>; 8],
}

impl TryFrom<&AstBitStruct> for IrBitStruct {
  // we could have more than one error in a single conversion so we probably
  // want to have the error be like a "list of problems" of some kind?
  type Error = impl IntoIter<Item = ()>;

  fn try_from(ast: &AstBitStruct) -> Result<Self, Self::Error> {
    let name = ast.name;
    let file_id = ast.file_id;
    let mut fields = [None; 8];
    for (field_name, bit) in ast.fields.iter() {
      // QUESTION: should we support non-decimal numeric literals here?
      let i = match bit.as_str() {
        "0" => 0,
        "1" => 1,
        "2" => 2,
        "3" => 3,
        "4" => 4,
        "5" => 5,
        "6" => 6,
        "7" => 7,
        other => todo!("error: illegal bit position"),
      };
      if fields[i] != None {
        todo!("error: bit position duplicate")
      } else {
        fields[i] = Some(field_name);
      }
    }
    Ok(Self { name, file_id, fields })
  }
}

#[derive(Debug, Clone)]
pub struct IrStruct {
  pub name: S<StrID>,
  pub file_id: FileID,
  pub fields: Vec<(S<StrID>, S<StrID>)>,
}
