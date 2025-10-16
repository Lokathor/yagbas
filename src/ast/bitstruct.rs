pub use super::*;

/// Bitpacked structured data.
///
/// An 8-bit value, where each bit position can have a field name.
#[derive(Debug, Clone)]
pub struct AstBitStruct {
  pub file_id: FileID,
  pub name: S<StrID>,
  /// `field_name: bit` list
  pub fields: Vec<(S<StrID>, S<StrID>)>,
}

#[derive(Debug, Clone)]
pub struct IrBitStruct {
  pub file_id: FileID,
  pub name: StrID,
  pub name_span: SimpleSpan,
  pub bit_names: [Option<StrID>; 8],
  pub name_spans: [Option<SimpleSpan>; 8],
  pub bit_spans: [Option<SimpleSpan>; 8],
}
impl IrBitStruct {
  pub fn try_from_ast_data(b: &AstBitStruct) -> Option<Self> {
    let mut out = Self {
      file_id: b.file_id,
      name: b.name.0,
      name_span: b.name.1,
      bit_names: [None; 8],
      name_spans: [None; 8],
      bit_spans: [None; 8],
    };

    let mut bad = false;
    for (s_f_name, s_f_bit) in b.fields.iter() {
      if let Some(i) = parse_num_lit(s_f_bit.0)
        && let Some(u) = usize::try_from(i).ok()
        && u < 8
      {
        if let Some(_cur) = out.bit_names[u] {
          log_error(YagError::DuplicateFieldName(b.file_id, s_f_name.1));
          bad = true;
        } else {
          out.bit_names[u] = Some(s_f_name.0);
          out.name_spans[u] = Some(s_f_name.1);
          out.bit_spans[u] = Some(s_f_bit.1);
        }
      }
    }
    if bad { None } else { Some(out) }
  }

  pub fn get_bit_of(&self, name: StrID) -> Option<usize> {
    self.bit_names.iter().position(|bit_name| *bit_name == Some(name))
  }
}

/// Parse one [BitStruct]
pub(crate) fn bitstruct_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, AstBitStruct, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
  M: Fn(&'src [(TokenTree, SimpleSpan)], SimpleSpan) -> I + Copy + 'src,
{
  let keyword = kw_bit_struct_p();
  let name = ident_p().map_with(S::from_extras);
  let fields = newline_p()
    .repeated()
    .ignore_then(ident_p().map_with(S::from_extras))
    .then_ignore(colon_p())
    .then(numlit_p().map_with(S::from_extras))
    .separated_by(comma_p().padded_by(newline_p().repeated()))
    .allow_trailing()
    .collect::<Vec<_>>()
    .nested_in(braces_content_p(make_input));

  keyword.ignore_then(name).then(fields).map_with(|(name, fields), ex| {
    let state: &mut SimpleState<&'static FileData> = ex.state();
    let file_id: FileID = state.id();
    AstBitStruct { file_id, name, fields }
  })
}
