use chumsky::{
  input::{Input, SpannedInput, ValueInput},
  prelude::*,
};

use crate::{
  token::{Token, Token::*},
  ErrRichToken,
};

#[inline]
#[must_use]
pub const fn id2<A, B>(a: A, b: B) -> (A, B) {
  (a, b)
}

pub type TokenSliceInput<'a> = SpannedInput<Token, SimpleSpan, &'a [(Token, SimpleSpan)]>;

/// A lone token or a list of token trees within one of three groupings.
///
/// Collecting a raw token list into token trees ensures that all the
/// opening/closing markers of all the groupings are balanced before trying to
/// do any more advanced parsing.
///
/// * See: [make_token_trees]
#[derive(Clone, PartialEq, Eq)]
pub enum TokenTree {
  Lone(Token),
  Parens(Vec<(Self, SimpleSpan)>),
  Brackets(Vec<(Self, SimpleSpan)>),
  Braces(Vec<(Self, SimpleSpan)>),
  TreeError,
}
use TokenTree::*;
impl core::fmt::Debug for TokenTree {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Lone(t) => core::fmt::Debug::fmt(&t, f),
      Parens(ts) => {
        if ts.len() > 10 {
          write!(f, "(...{} elements...)", ts.len())
        } else {
          write!(f, "(")?;
          for (i, (tt, _span)) in ts.iter().enumerate() {
            if i > 0 {
              write!(f, " ")?;
            }
            write!(f, "{tt:?}")?;
          }
          write!(f, ")")?;
          Ok(())
        }
      }
      Brackets(ts) => {
        if ts.len() > 10 {
          write!(f, "[...{} elements...]", ts.len())
        } else {
          write!(f, "[")?;
          for (i, (tt, _span)) in ts.iter().enumerate() {
            if i > 0 {
              write!(f, " ")?;
            }
            write!(f, "{tt:?}")?;
          }
          write!(f, "]")?;
          Ok(())
        }
      }
      Braces(ts) => {
        if ts.len() > 10 {
          write!(f, "{{...{} elements...}}", ts.len())
        } else {
          write!(f, "{{")?;
          for (i, (tt, _span)) in ts.iter().enumerate() {
            if i > 0 {
              write!(f, " ")?;
            }
            write!(f, "{tt:?}")?;
          }
          write!(f, "}}")?;
          Ok(())
        }
      }
      TreeError => write!(f, "TreeError"),
    }
  }
}
impl TokenTree {
  /// Parses for just one token tree.
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenSliceInput<'a>, Self, ErrRichToken<'a>> + Clone {
    recursive(|tt| {
      let token_list =
        tt.map_with_span(id2).repeated().collect::<Vec<(TokenTree, SimpleSpan)>>();

      // Looks like `[ ... ]`
      let brackets = token_list
        .clone()
        .delimited_by(just(Punct('[')), just(Punct(']')))
        .map(TokenTree::Brackets);

      // Looks like `{ ... }`
      let braces = token_list
        .clone()
        .delimited_by(just(Punct('{')), just(Punct('}')))
        .map(TokenTree::Braces);

      // Looks like `( ... )`
      let parens = token_list
        .clone()
        .delimited_by(just(Punct('(')), just(Punct(')')))
        .map(TokenTree::Parens);

      // Looks like something that does *NOT* open or close one of the other types.
      let single = none_of([
        Punct('['),
        Punct(']'),
        Punct('{'),
        Punct('}'),
        Punct('('),
        Punct(')'),
        CommentBlockStart,
        CommentBlockEnd,
      ])
      .map(TokenTree::Lone);

      // Looks like `//`
      let single_comment = just(CommentSingle).ignored();

      // Looks like `/* ... */`
      let block_comment = token_list
        .clone()
        .delimited_by(just(CommentBlockStart), just(CommentBlockEnd))
        .ignored();

      // Either type of comment
      let comment = single_comment.or(block_comment);

      choice((brackets, braces, parens, single)).padded_by(comment.repeated())
    })
  }
}

pub fn grow_token_trees(
  tokens: &[(Token, SimpleSpan)],
) -> (Vec<(TokenTree, SimpleSpan)>, Vec<Rich<'static, Token>>) {
  let len = tokens.last().map(|(_, s)| s.end).unwrap_or(0);
  let span: SimpleSpan = SimpleSpan::from(len..len);
  let parser = TokenTree::parser()
    .recover_with(via_parser(any().repeated().at_least(1).to(TokenTree::TreeError)))
    .map_with_span(id2)
    .repeated()
    .collect::<Vec<_>>();
  let input = tokens.spanned(span);
  let (trees, errors) = parser.parse(input).into_output_errors();
  (
    trees.unwrap_or_default(),
    errors.into_iter().map(|r| r.into_owned()).collect::<Vec<_>>(),
  )
}
