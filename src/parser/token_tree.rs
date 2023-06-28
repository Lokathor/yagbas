use chumsky::{input::ValueInput, prelude::*};

use crate::lexer::Token;

use super::MyParseErr;

#[derive(Debug, Clone)]
pub enum TokenTree {
  Lone(Token),
  Parens(Vec<(Self, SimpleSpan)>),
  Squares(Vec<(Self, SimpleSpan)>),
  Braces(Vec<(Self, SimpleSpan)>),
}
impl TokenTree {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, MyParseErr<'a>>
  where
    I: ValueInput<'a, Token = crate::lexer::Token, Span = SimpleSpan>,
  {
    recursive(|tt| {
      let token_list = tt.map_with_span(|i, s| (i, s)).repeated().collect::<Vec<_>>();

      // Looks like `(...)`
      let parens = token_list
        .clone()
        .map(TokenTree::Parens)
        .delimited_by(just(Token::Punct('(')), just(Token::Punct(')')));

      // Looks like `[...]`
      let squares = token_list
        .clone()
        .map(TokenTree::Squares)
        .delimited_by(just(Token::Punct('[')), just(Token::Punct(']')));

      // Looks like `{...}`
      let braces = token_list
        .clone()
        .map(TokenTree::Braces)
        .delimited_by(just(Token::Punct('{')), just(Token::Punct('}')));

      // Looks like `5` or `"hello"`
      let single =
        none_of([Token::Punct(')'), Token::Punct(']'), Token::Punct('}')]).map(TokenTree::Lone);

      parens.or(squares).or(braces).or(single)
    })
  }
}

pub fn make_token_trees(
  tokens: &[(Token, SimpleSpan)],
) -> ParseResult<Vec<TokenTree>, Rich<'_, Token>> {
  let span: SimpleSpan = if tokens.is_empty() {
    (0..0).into()
  } else {
    let start = tokens.first().unwrap().1.start;
    let end = tokens.last().unwrap().1.end;
    (start..end).into()
  };
  TokenTree::parser().repeated().collect::<Vec<_>>().parse(tokens.spanned(span))
}
