use chumsky::{
  error::Rich,
  extra::Err,
  input::{BorrowInput, Input, ValueInput},
  prelude::*,
  Parser,
};

use super::*;

pub mod token_tree;

use token_tree::*;

pub type ErrRichToken<'src> = Err<Rich<'src, Token, FileSpan>>;
pub type ErrRichTokenTree<'src> = Err<Rich<'src, TokenTree, FileSpan>>;

#[allow(clippy::needless_lifetimes)]
pub fn make_input<'src>(
  tokens: &'src [FileSpanned<TokenTree>], eoi: FileSpan,
) -> impl BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>
{
  tokens.map(eoi, |fsd| (&fsd._payload, &fsd._span))
}

#[inline]
pub fn lex_module_text(
  text: &str, id: SrcID, err_bucket: &mut Vec<YagError>,
) -> Vec<FileSpanned<Token>> {
  let mut output = Vec::new();
  for (res_token, range_span) in Token::lexer(text).spanned() {
    let file_span = FileSpan::new(id, range_span);
    match res_token {
      Ok(token) => {
        output.push(FileSpanned::new(token, file_span));
      }
      Err(()) => {
        output.push(FileSpanned::new(Token::TokenError, file_span));
        err_bucket.push(YagError::Tokenization(file_span));
      }
    }
  }
  output
}

pub fn grow_token_trees(
  tokens: &[FileSpanned<Token>], err_bucket: &mut Vec<YagError>,
) -> Vec<FileSpanned<TokenTree>> {
  let end_span = match tokens.last() {
    None => return Vec::new(),
    Some(t) => FileSpan {
      id: t._span.id,
      // Note(Lokathor): Yes, we're using `end` twice deliberately. Chumsky is
      // a silly friend, this is just how it works.
      start: t._span.end,
      end: t._span.end,
    },
  };
  let recovery = via_parser(
    any()
      .repeated()
      .at_least(1)
      .map_with(|_, ex| FileSpanned::new(TokenTree::TreeError, ex.span())),
  );
  let tree_parser = token_tree_p().recover_with(recovery).repeated().collect();
  let (opt_output, errors) = tree_parser
    .parse(Input::map(tokens, end_span, |fs| (&fs._payload, &fs._span)))
    .into_output_errors();
  let trees: Vec<FileSpanned<TokenTree>> = opt_output.unwrap_or_default();
  err_bucket
    .extend(errors.into_iter().map(|e| YagError::TokenTree(e.into_owned())));
  trees
}
