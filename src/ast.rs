use crate::{
  comment_filter::no_comment_tokens,
  id2,
  item_decl::ItemDecl,
  run_parser,
  token::Token,
  token_tree::{make_token_trees, TokenTree},
};
use chumsky::{span::SimpleSpan, IterParser, Parser};

pub struct Ast<T> {
  pub items: Vec<(T, SimpleSpan)>,
}

impl Ast<Token> {
  /// Refines module source code into a list of [Token].
  pub fn module_to_tokens(module: &str) -> Result<Self, String> {
    Ok(Self { items: no_comment_tokens(module).map_err(|e| format!("{e:?}"))? })
  }

  pub fn make_token_trees(&self) -> Result<Ast<TokenTree>, String> {
    Ok(Ast {
      items: make_token_trees(&self.items).into_result().map_err(|v| format!("{v:?}"))?,
    })
  }
}

impl Ast<TokenTree> {
  pub fn parse_declarations(&self) -> Result<Ast<ItemDecl>, String> {
    let item_parser =
      ItemDecl::parser().map_with_span(id2).repeated().collect::<Vec<_>>();
    Ok(Ast {
      items: run_parser(item_parser, &self.items)
        .into_result()
        .map_err(|v| format!("{v:?}"))?,
    })
  }
}
