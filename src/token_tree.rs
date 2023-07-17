use chumsky::{
  input::{SpannedInput, ValueInput},
  prelude::*,
};

use crate::{
  id2,
  token::{Token, Token::*},
  ErrRichToken,
};

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
  CommentBlock,
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
      CommentBlock => write!(f, "/* */"),
      TreeError => write!(f, "TreeError"),
    }
  }
}
impl TokenTree {
  /// Parses for just one token tree.
  pub fn parser<'a>() -> impl Parser<
    'a,
    SpannedInput<Token, SimpleSpan, &'a [(Token, SimpleSpan)]>,
    Self,
    ErrRichToken<'a>,
  > + Clone {
    recursive(|tt| {
      let token_list = tt.map_with_span(id2).repeated().collect::<Vec<_>>();

      // Looks like `/* ... */`
      let comment = token_list
        .clone()
        .delimited_by(just(CommentBlockStart), just(CommentBlockEnd))
        .to(CommentBlock);

      // Looks like `[...]`
      let brackets = token_list
        .clone()
        .delimited_by(just(Punct('[')), just(Punct(']')))
        .map(|mut trees| {
          trees.retain(|(tree, _span)| !matches!(tree, TokenTree::CommentBlock));
          TokenTree::Brackets(trees)
        });

      // Looks like `{...}`
      let braces = token_list
        .clone()
        .delimited_by(just(Punct('{')), just(Punct('}')))
        .map(|mut trees| {
          trees.retain(|(tree, _span)| !matches!(tree, TokenTree::CommentBlock));
          TokenTree::Braces(trees)
        });

      // Looks like `(...)`
      let parens = token_list
        .clone()
        .delimited_by(just(Punct('(')), just(Punct(')')))
        .map(|mut trees| {
          trees.retain(|(tree, _span)| !matches!(tree, TokenTree::CommentBlock));
          TokenTree::Parens(trees)
        });

      // Looks like something that does *NOT* close one of the other types.
      let single = none_of([CommentBlockEnd, Punct(')'), Punct(']'), Punct('}')])
        .padded_by(just(CommentSingle).repeated())
        .map(TokenTree::Lone);

      choice((comment, brackets, braces, parens, single))
    })
  }
}

#[test]
#[cfg(FALSE)]
fn test_make_token_trees() {
  let checks: &[(&str, &[TokenTree])] = &[
    ("[hl]", &[Lone(AddrHL)]),
    ("[ hl]", &[Lone(AddrHL)]),
    ("[hl ]", &[Lone(AddrHL)]),
    ("[ hl ]", &[Lone(AddrHL)]),
    //
    ("[bc]", &[Lone(AddrBC)]),
    ("[ bc]", &[Lone(AddrBC)]),
    ("[bc ]", &[Lone(AddrBC)]),
    ("[ bc ]", &[Lone(AddrBC)]),
    //
    ("[hl+]", &[Lone(AddrHLInc)]),
    ("[hl++]", &[Lone(AddrHLInc)]),
    ("[hl +]", &[Lone(AddrHLInc)]),
    ("[hl + +]", &[Lone(AddrHLInc)]),
    //
    ("[hl-]", &[Lone(AddrHLDec)]),
    ("[hl--]", &[Lone(AddrHLDec)]),
    ("[hl -  ]", &[Lone(AddrHLDec)]),
    ("[ hl  - -]", &[Lone(AddrHLDec)]),
  ];

  for (prog, expected) in checks {
    let tokens = Ast::tokenize(prog.to_string());
    let token_trees = make_token_trees(&tokens.items);
    if token_trees.has_errors() {
      for err in token_trees.errors() {
        println!("ERROR: {err:?}");
      }
      panic!("One or more errors during token tree creation.");
    }
    for (ex, ac) in expected.iter().zip(token_trees.output().unwrap().iter()) {
      assert_eq!(ex, &ac.0);
    }
  }
}
