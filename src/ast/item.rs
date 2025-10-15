use super::*;
use internal_iterator_rec::InternalIteratorRec;
use internal_iterator_rec::adhoc_internal_iterator_rec;

/// An item is basically "a top level definition within a source file".
#[derive(Debug, Clone)]
pub enum Item {
  BitStruct(AstBitStruct),
  Const(AstConst),
  Func(AstFunc),
  Static(AstStatic),
  Struct(AstStruct),
  ItemError(FileID),
}
impl Item {
  pub fn get_name(&self) -> Option<StrID> {
    Some(match self {
      Self::BitStruct(AstBitStruct { name, .. }) => name.0,
      Self::Const(AstConst { name, .. }) => name.0,
      Self::Func(AstFunc { name, .. }) => name.0,
      Self::Static(AstStatic { name, .. }) => name.0,
      Self::Struct(AstStruct { name, .. }) => name.0,
      Self::ItemError(_) => return None,
    })
  }

  pub fn file_id(&self) -> FileID {
    match self {
      Self::BitStruct(AstBitStruct { file_id, .. }) => *file_id,
      Self::Const(AstConst { file_id, .. }) => *file_id,
      Self::Func(AstFunc { file_id, .. }) => *file_id,
      Self::Static(AstStatic { file_id, .. }) => *file_id,
      Self::Struct(AstStruct { file_id, .. }) => *file_id,
      Self::ItemError(file_id) => *file_id,
    }
  }

  pub fn iter_s_exprs_mut(
    &mut self,
  ) -> impl '_ + InternalIteratorRec<Item = &'_ mut S<Expr>> {
    adhoc_internal_iterator_rec!(
      'r, self, |this: &'r mut Item, yield_| -> &'r mut S<Expr> {
        match this {
          Item::Const(c) => yield_(&mut c.expr)?,
          Item::Static(s) => yield_(&mut s.expr)?,
          Item::Func(f) => for s_statement in f.iter_s_statements_mut() {
              s_statement.0.iter_s_exprs_mut().try_for_each_rec( yield_)?;
          }
          _ => (),
        }
      }
    )
  }
}

fn per_expr_expand_size_of_static(
  s_expr: &mut S<Expr>, static_sizes: &HashMap<StrID, i32>, file_id: FileID,
) {
  match s_expr {
    S(Expr::MacroUse(xs), _span) => {
      let (name_x, args) = xs.split_last_mut().expect("macro with no name!");
      let name: StrID = if let Expr::Ident(i) = &name_x.0 {
        *i
      } else {
        panic!("macro name not an ident!");
      };
      if name.as_str() == "size_of_static" {
        match args {
          [S(Expr::Ident(target), _)] => {
            if let Some(size) = static_sizes.get(target) {
              s_expr.0 = Expr::Val(*size);
            } else {
              log_error(YagError::MacroSizeOfStaticNoSize(file_id, name_x.1))
            }
          }
          _ => log_error(YagError::MacroSizeOfStaticBadArgs(file_id, name_x.1)),
        }
      } else {
        args.iter_mut().for_each(|s_expr| {
          per_expr_expand_size_of_static(s_expr, static_sizes, file_id)
        });
      }
    }
    S(other, _span) => other.inner_expr_mut().iter_mut().for_each(|s_expr| {
      per_expr_expand_size_of_static(s_expr, static_sizes, file_id)
    }),
  }
}

pub fn per_item_expand_size_of_static(
  s_item: &mut S<Item>, static_sizes: &HashMap<StrID, i32>,
) {
  let file_id = s_item.0.file_id();
  s_item.0.iter_s_exprs_mut().for_each(|s_expr| {
    per_expr_expand_size_of_static(s_expr, static_sizes, file_id);
  })
}

pub fn items_of(
  trees: &[(TokenTree, SimpleSpan)], file_data: &'static FileData,
) -> Vec<S<Item>> {
  let eoi: SimpleSpan = match trees.last() {
    Some(s) => s.1,
    None => return Vec::new(),
  };
  let recovery = via_parser(
    item_start_p()
      .then(any().and_is(item_start_p().not()).repeated())
      .to(Item::ItemError(file_data.id())),
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

  item_errors.into_iter().for_each(|error| {
    log_error(YagError::ItemParseError(file_data.id(), error.into_owned()))
  });

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
