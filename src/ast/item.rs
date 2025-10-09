use super::*;

/// An item is basically "a top level definition within a source file".
#[derive(Debug, Clone)]
pub enum Item {
  BitStruct(AstBitStruct),
  Const(AstConst),
  Func(AstFunc),
  Static(AstStatic),
  Struct(AstStruct),
  ItemError,
}
impl Item {
  pub fn get_name(&self) -> Option<StrID> {
    Some(match self {
      Self::BitStruct(AstBitStruct { name, .. }) => name.0,
      Self::Const(AstConst { name, .. }) => name.0,
      Self::Func(AstFunc { name, .. }) => name.0,
      Self::Static(AstStatic { name, .. }) => name.0,
      Self::Struct(AstStruct { name, .. }) => name.0,
      Self::ItemError => return None,
    })
  }
}

pub fn items_of(
  trees: &[(TokenTree, SimpleSpan)], file_data: &'static FileData,
  err_bucket: &mut Vec<YagError>,
) -> Vec<S<Item>> {
  let eoi: SimpleSpan = match trees.last() {
    Some(s) => s.1,
    None => return Vec::new(),
  };
  let recovery = via_parser(
    item_start_p()
      .then(any().and_is(item_start_p().not()).repeated())
      .to(Item::ItemError),
  );

  let module_parser = item_p(make_tt_input)
    .padded_by(newline_p().repeated())
    .recover_with(recovery)
    .map_with(S::from_extras)
    .repeated()
    .collect::<Vec<_>>();

  let (opt_out, item_errors) = module_parser
    .parse_with_state(make_tt_input(trees, eoi), &mut SimpleState(file_data))
    .into_output_errors();

  err_bucket.extend(
    item_errors.into_iter().map(|error| {
      YagError::ItemParseError(error.into_owned(), file_data.id())
    }),
  );

  opt_out.unwrap_or_default()
}

/// Parses any keyword that begins an item.
pub fn item_start_p<'src, I>()
-> impl Parser<'src, I, Token, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::KwBitStruct) => Token::KwBitStruct,
    TokenTree::Lone(Token::KwConst) => Token::KwConst,
    TokenTree::Lone(Token::KwFn) => Token::KwFn,
    TokenTree::Lone(Token::KwStatic) => Token::KwStatic,
    TokenTree::Lone(Token::KwStruct) => Token::KwStruct,
  }
}

/// Parse one [Item]
pub fn item_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, Item, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
  M: Fn(&'src [(TokenTree, SimpleSpan)], SimpleSpan) -> I + Copy + 'src,
{
  let bs = bitstruct_p(make_input).map(Item::BitStruct);
  let c = const_p(make_input).map(Item::Const);
  let f = func_p(make_input).map(Item::Func);
  let sta = static_p(make_input).map(Item::Static);
  let struct_ = struct_p(make_input).map(Item::Struct);

  choice((bs, c, f, sta, struct_))
}
