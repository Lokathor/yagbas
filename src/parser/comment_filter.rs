use chumsky::span::SimpleSpan;
use logos::Span;

use crate::lexer::Token;

#[derive(Debug, Clone)]
pub enum CommentFilterError {
  LexError(Span),
  UnmatchedMultiOpen(Span),
  UnmatchedMultiClose(Span),
}
impl CommentFilterError {
  pub fn get_span(&self) -> Span {
    match self {
      CommentFilterError::LexError(s) => s.clone(),
      CommentFilterError::UnmatchedMultiOpen(s) => s.clone(),
      CommentFilterError::UnmatchedMultiClose(s) => s.clone(),
    }
  }
}

pub fn filter_out_comments(
  i: impl Iterator<Item = (Result<Token, ()>, Span)>,
) -> Result<Vec<(Token, SimpleSpan)>, CommentFilterError> {
  let mut out: Vec<(Token, SimpleSpan)> = vec![];
  let mut comment_levels = 0;
  let mut last_zero_opener = 0..0;
  for (res, span) in i {
    let token = match res {
      Err(()) => return Err(CommentFilterError::LexError(span)),
      Ok(Token::CommentSingle) => continue,
      Ok(Token::CommentMultiStart) => {
        if comment_levels == 0 {
          last_zero_opener = span;
        }
        comment_levels += 1;
        continue;
      }
      Ok(Token::CommentMultiEnd) => {
        if comment_levels == 0 {
          return Err(CommentFilterError::UnmatchedMultiClose(span));
        }
        comment_levels -= 1;
        continue;
      }
      Ok(_) if comment_levels > 0 => continue,
      Ok(t) => t,
    };
    out.push((token, span.into()));
  }
  if comment_levels > 0 {
    Err(CommentFilterError::UnmatchedMultiOpen(last_zero_opener))
  } else {
    Ok(out)
  }
}

pub fn no_comment_tokens(s: &str) -> Result<Vec<(Token, SimpleSpan)>, CommentFilterError> {
  filter_out_comments(Token::lexer(s).spanned())
}
