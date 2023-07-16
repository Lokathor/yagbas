use chumsky::{input::ValueInput, prelude::*};

use crate::{
  ast::Ast,
  id2, run_parser,
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
    }
  }
}
impl TokenTree {
  /// Parses for just one token tree.
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichToken<'a>>
  where
    I: ValueInput<'a, Token = crate::token::Token, Span = SimpleSpan>,
  {
    recursive(|tt| {
      let token_list = tt.map_with_span(id2).repeated().collect::<Vec<_>>();

      // Certain bracketed sequences that occur very frequently, and that have
      // only one form without extra info we need to carry, we collapse into a
      // single lone token during the tree creation. This reduces intermediate
      // allocation and simplifies the later parsing stages quite a bit.

      // Looks like `[ hl + + ]` or `[ hl + ]`
      let addr_hl_inc = just(Punct('['))
        .ignore_then(just(RegHL))
        .ignore_then(just(Punct('+')))
        .ignore_then(just(Punct('+')).or_not())
        .ignore_then(just(Punct(']')))
        .ignored()
        .to(Lone(AddrHLInc));

      // Looks like `[ hl - - ]` or `[ hl - ]`
      let addr_hl_dec = just(Punct('['))
        .ignore_then(just(RegHL))
        .ignore_then(just(Punct('-')))
        .ignore_then(just(Punct('-')).or_not())
        .ignore_then(just(Punct(']')))
        .ignored()
        .to(Lone(AddrHLDec));

      // Looks like `[ hl ]`
      let addr_hl = just(Punct('['))
        .ignore_then(just(RegHL))
        .ignore_then(just(Punct(']')))
        .ignored()
        .to(Lone(AddrHL));

      // Looks like `[ bc ]`
      let addr_bc = just(Punct('['))
        .ignore_then(just(RegBC))
        .ignore_then(just(Punct(']')))
        .ignored()
        .to(Lone(AddrBC));

      // Looks like `[ de ]`
      let addr_de = just(Punct('['))
        .ignore_then(just(RegDE))
        .ignore_then(just(Punct(']')))
        .ignored()
        .to(Lone(AddrDE));

      // Looks like `[...]`
      let brackets = token_list
        .clone()
        .delimited_by(just(Punct('[')), just(Punct(']')))
        .map(TokenTree::Brackets);

      // Looks like `{...}`
      let braces = token_list
        .clone()
        .delimited_by(just(Punct('{')), just(Punct('}')))
        .map(TokenTree::Braces);

      // Looks like `(...)`
      let parens = token_list
        .clone()
        .delimited_by(just(Punct('(')), just(Punct(')')))
        .map(TokenTree::Parens);

      // Looks like something that does *NOT* close one of the other types.
      let single = none_of([Punct(')'), Punct(']'), Punct('}')]).map(TokenTree::Lone);

      choice((
        addr_hl_inc,
        addr_hl_dec,
        addr_hl,
        addr_bc,
        addr_de,
        brackets,
        braces,
        parens,
        single,
      ))
    })
  }
}

#[inline]
pub fn make_token_trees(
  tokens: &[(Token, SimpleSpan)],
) -> ParseResult<Vec<(TokenTree, SimpleSpan)>, Rich<'_, Token>> {
  let parser = TokenTree::parser().map_with_span(id2).repeated().collect::<Vec<_>>();
  //
  run_parser(parser, tokens)
}

#[test]
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
