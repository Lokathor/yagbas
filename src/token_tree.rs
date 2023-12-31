use chumsky::{
  input::{Input, SpannedInput, ValueInput},
  prelude::*,
};

use crate::{
  src_files::FileSpan,
  token::{Token, Token::*},
  ErrRichToken,
};

#[inline]
#[must_use]
const fn id2<A, B>(a: A, b: B) -> (A, B) {
  (a, b)
}

pub type TokenSliceInput<'a> = SpannedInput<Token, FileSpan, &'a [(Token, FileSpan)]>;
pub type TokenTreeSliceInput<'a> =
  SpannedInput<TokenTree, FileSpan, &'a [(TokenTree, FileSpan)]>;

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
  Parens(Vec<(TokenTree, FileSpan)>),
  Brackets(Vec<(TokenTree, FileSpan)>),
  Braces(Vec<(TokenTree, FileSpan)>),
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
        tt.map_with_span(id2).repeated().collect::<Vec<(TokenTree, FileSpan)>>();

      // Looks like `[ ... ]`
      let open_bracket = select! {
        Punct('[') => (),
      };
      let close_bracket = select! {
        Punct(']') => (),
      };
      let brackets = token_list
        .clone()
        .delimited_by(open_bracket, close_bracket)
        .map(TokenTree::Brackets);

      // Looks like `{ ... }`
      let open_brace = select! {
        Punct('{') => (),
      };
      let close_brace = select! {
        Punct('}') => (),
      };
      let braces =
        token_list.clone().delimited_by(open_brace, close_brace).map(TokenTree::Braces);

      // Looks like `( ... )`
      let open_paren = select! {
        Punct('(') => (),
      };
      let close_paren = select! {
        Punct(')') => (),
      };
      let parens =
        token_list.clone().delimited_by(open_paren, close_paren).map(TokenTree::Parens);

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

      let x = choice((brackets, braces, parens, single)).padded_by(comment.repeated());

      x
    })
  }
}

#[allow(clippy::type_complexity)]
pub fn grow_token_trees(
  tokens: &[(Token, FileSpan)],
) -> (Vec<(TokenTree, FileSpan)>, Vec<Rich<'static, Token, FileSpan>>) {
  if tokens.is_empty() {
    return (Vec::new(), Vec::new());
  }
  let last_span = *tokens.last().map(|(_, s)| s).unwrap();
  let end_span = FileSpan { start: last_span.end, ..last_span };
  let recover_strategy =
    via_parser(any().repeated().at_least(1).to(TokenTree::TreeError));
  let parser = TokenTree::parser()
    .recover_with(recover_strategy)
    .map_with_span(id2)
    .repeated()
    .collect::<Vec<(TokenTree, FileSpan)>>();
  let input = tokens.spanned(end_span);
  let (trees, errors) = parser.parse(input).into_output_errors();
  (
    trees.unwrap_or_default(),
    errors.into_iter().map(|r| r.into_owned()).collect::<Vec<_>>(),
  )
}
