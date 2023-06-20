use crate::{lexer::Token, StaticStr};
use logos::Span;

pub fn remove_comments(
  mut i: impl Iterator<Item = (Result<Token, StaticStr>, Span)>,
) -> impl Iterator<Item = (Result<Token, StaticStr>, Span)> {
  let mut comment_levels = 0_usize;
  core::iter::from_fn(move || loop {
    match i.next()? {
      (Ok(Token::CommentSingle), _) => continue,
      (Ok(Token::CommentMultiStart), _) => {
        comment_levels += 1;
        continue;
      }
      (Ok(Token::CommentMultiEnd), span) => {
        if comment_levels == 0 {
          return Some((Err("UnmatchedCommentMultiEnd"), span));
        } else {
          comment_levels -= 1;
          continue;
        }
      }
      (Ok(_), _) if comment_levels > 0 => continue,
      (Ok(token), span) => return Some((Ok(token), span)),
      (Err(e), span) => return Some((Err(e), span)),
    }
  })
}
