use super::*;
use str_id::StrID;
use Token::*;
use TokenTree::*;

/// Parse all the token trees of a file into the items of the file
#[allow(clippy::type_complexity)]
pub fn parse_token_trees_to_items(
  token_trees: &[(TokenTree, FileSpan)],
) -> (Vec<(Item, FileSpan)>, Vec<Rich<'static, TokenTree, FileSpan>>) {
  let last_span = if let Some((_tt, file_span)) = token_trees.last() {
    let mut x = *file_span;
    x.start = x.end;
    x
  } else {
    return (Vec::new(), Vec::new());
  };
  let parser =
    Item::parser().map_with(|item, ex| (item, ex.span())).repeated().collect::<Vec<_>>();
  let input = token_trees.spanned(last_span);
  let (items, errors) = parser.parse(input).into_output_errors();
  (
    items.unwrap_or_default(),
    errors.into_iter().map(|r| r.into_owned()).collect::<Vec<_>>(),
  )
}

#[derive(Debug)]
pub enum Item {
  Fn(FnDecl),
  ItemError,
}
impl Item {
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSliceInput<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    let fn_decl = FnDecl::parser().map(Self::Fn);

    choice((fn_decl,))
  }
}

#[derive(Debug, Clone)]
pub struct FnDecl {
  pub name: StrID,
  pub args: Vec<(TokenTree, FileSpan)>,
  pub statements: Vec<(Statement, FileSpan)>,
}
impl FnDecl {
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSliceInput<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    let kw_fn = just(Lone(KwFn));
    let fn_name = select! {
      Lone(Ident(i)) => i
    };
    let arguments = select! {
      Parens(p) => p
    };
    let line_sep = select! {
      Lone(Newline) => (),
      Lone(Semicolon) => (),
    }
    .ignored();
    let statements = Statement::parser()
      .map_with(|item, ex| (item, ex.span()))
      .padded_by(line_sep)
      .repeated()
      .collect::<Vec<_>>()
      .nested_in(select_ref! {
        Braces(b) = ex => {
          b.spanned(ex.span())
        }
      });

    kw_fn
      .ignore_then(fn_name)
      .then(arguments)
      .then(statements)
      .map(|((name, args), statements)| Self { name, args, statements })
  }
}

#[derive(Debug, Clone)]
pub enum Statement {
  Call { target: StrID, args: Vec<(TokenTree, FileSpan)> },
}
impl Statement {
  pub fn s_call<'a>(
  ) -> impl Parser<'a, TokenTreeSliceInput<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    let call_target = select! {
      Lone(Ident(i)) => i
    };
    let arguments = select! {
      Parens(p) => p
    };
    call_target.then(arguments).map(|(target, args)| Statement::Call { target, args })
  }

  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSliceInput<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    let call = Self::s_call();

    choice((call,))
  }
}
