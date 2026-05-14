use yagbas::tokenizer::Token;
use yagbas::tokenizer::tokenize;

#[test]
fn test_the_tokenizer() {
  const TOKENS_ONE_PER_LINE: &str = include_str!("tokens_one_per_line.txt");
  for line in TOKENS_ONE_PER_LINE.lines() {
    assert_eq!(tokenize(line).count(), 1);
    assert!(tokenize(line).all(|(t, _span)| t != Token::UnknownToken));
  }
}
