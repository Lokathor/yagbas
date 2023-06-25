use yagbas::{
  lexer::Token,
  parser::{interpret_num, strip_comments, InterpretNumErr},
};

fn num_test(s: &str, r: Result<i32, InterpretNumErr>) {
  let tokens: Vec<Token> =
    strip_comments(Token::lexer(s).spanned()).unwrap().into_iter().map(|t| t.0).collect();
  assert_eq!(1, tokens.len(), "Too Many Tokens: {tokens:?}");
  assert_eq!(r, interpret_num(tokens[0].unwrap_num()));
}

#[test]
fn number_parsing() {
  let x = [
    ("0", Ok(0)),
    ("0011", Ok(11)),
    ("1", Ok(1)),
    ("1234", Ok(1234)),
    ("-1", Ok(-1)),
    ("10", Ok(10)),
    ("$FF_00", Ok(0xFF00)),
    ("%1010", Ok(0b1010)),
    ("0xABCD", Ok(0xABCD)),
    ("0b1100", Ok(0b1100)),
  ];
  for (s, r) in x {
    num_test(s, r)
  }
}
