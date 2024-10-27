use crate::{
  src_files::{FileSpan, FileSpanned},
  token::Token,
  token_tree::TokenTree,
};
use chumsky::{extra::Err, input::BorrowInput, prelude::*};

pub type ErrRichToken<'src> = Err<Rich<'src, Token, FileSpan>>;
pub type ErrRichTokenTree<'src> = Err<Rich<'src, TokenTree, FileSpan>>;

/// Parses [Token] into [TokenTree].
pub fn token_tree_p<'src, I, M>(
) -> impl Parser<'src, I, TokenTree, ErrRichToken<'src>> + Clone
where
  I: BorrowInput<'src, Token = Token, Span = FileSpan>,
{
  use crate::Token::*;

  recursive(|tt| {
    let token_list = tt
      .map_with(|token, ex| (token, ex.span()))
      .repeated()
      .collect::<Vec<FileSpanned<TokenTree>>>();

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
    let block_comment =
      token_list.clone().delimited_by(block_start, block_end).ignored();
    // Either type of comment
    let comment = single_comment.or(block_comment);

    let x = choice(()).padded_by(comment.repeated());

    x
  })
}
