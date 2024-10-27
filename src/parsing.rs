use crate::{
  src_files::{FileSpan, FileSpanned},
  token::Token,
  token_tree::TokenTree,
};
use chumsky::{
  extra::Err,
  input::{BorrowInput, ValueInput},
  prelude::*,
};

pub type ErrRichToken<'src> = Err<Rich<'src, Token, FileSpan>>;
pub type ErrRichTokenTree<'src> = Err<Rich<'src, TokenTree, FileSpan>>;

/// Parses [Token] into [TokenTree].
pub fn token_tree_p<'src, I, M>(
) -> impl Parser<'src, I, TokenTree, ErrRichToken<'src>> + Clone
where
  I: BorrowInput<'src, Token = Token, Span = FileSpan> + ValueInput<'src>,
{
  use crate::Token::*;

  recursive(|tt| {
    // Looks like `{ ... }`
    let braces = {
      let open_bracket = select! {
        OpBrace => (),
      };
      let close_bracket = select! {
        ClBrace => (),
      };
      tt.clone()
        .map_with(|token_tree, ex| FileSpanned::new(token_tree, ex.span()))
        .repeated()
        .collect()
        .delimited_by(open_bracket, close_bracket)
        .map(TokenTree::Braces)
    };

    // Looks like `[ ... ]`
    let brackets = {
      let open_bracket = select! {
        OpBracket => (),
      };
      let close_bracket = select! {
        ClBracket => (),
      };
      tt.clone()
        .map_with(|token_tree, ex| FileSpanned::new(token_tree, ex.span()))
        .repeated()
        .collect()
        .delimited_by(open_bracket, close_bracket)
        .map(TokenTree::Brackets)
    };

    // Looks like `( ... )`
    let parens = {
      let open_paren = select! {
        OpParen => (),
      };
      let close_paren = select! {
        ClParen => (),
      };
      tt.clone()
        .map_with(|token_tree, ex| FileSpanned::new(token_tree, ex.span()))
        .repeated()
        .collect()
        .delimited_by(open_paren, close_paren)
        .map(TokenTree::Parens)
    };

    // Looks like something that does *NOT* open or close one of the other
    // types.
    let single =
      none_of([OpBracket, ClBracket, OpBrace, ClBrace, OpParen, ClParen])
        .map(TokenTree::Lone);

    // comments get stripped from the output.
    let comment = {
      // Looks like `//`
      let single_comment = select! {
        CommentSingle => (),
      };
      // Looks like `/* ... */`
      let block_start = select! {
        CommentBlockStart => (),
      };
      let block_end = select! {
        CommentBlockEnd => (),
      };
      let block_comment = tt
        .clone()
        .map_with(|token_tree, ex| FileSpanned::new(token_tree, ex.span()))
        .repeated()
        .delimited_by(block_start, block_end)
        .ignored();

      single_comment.or(block_comment)
    };

    let x =
      choice((brackets, braces, parens, single)).padded_by(comment.repeated());

    x
  })
}
