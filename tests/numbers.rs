use yagbas::{
  lexer::Token,
  parser::{interpret_num, strip_comments, InterpretNumErr},
};

fn num_test(s: &str, t: Token, r: Result<i32, InterpretNumErr>) {
  let tokens: Vec<Token> =
    strip_comments(Token::lexer(s).spanned()).unwrap().into_iter().map(|t| t.0).collect();
  assert_eq!(1, tokens.len(), "Too Many Tokens: {tokens:?}");
  assert_eq!(t, tokens[0]);
  assert_eq!(r, interpret_num(tokens[0].unwrap_num()));
}

#[test]
fn numbers() {
  let x = [
    ("0", Token::Num("0"), Ok(0)),
    ("1", Token::Num("1"), Ok(1)),
    ("-1", Token::Num("-1"), Ok(-1)),
    (" $FF_00", Token::Num("$FF_00"), Ok(0xFF00)),
  ];
  for (s, t, r) in x {
    num_test(s, t, r)
  }
}
