use yagbas::tokenizer::TokenKind;
use yagbas::tokenizer::tokenize;

#[test]
fn test_the_tokenizer() {
  const TOKENS_ONE_PER_LINE: &str = include_str!("tokens_one_per_line.txt");
  for line in TOKENS_ONE_PER_LINE.lines() {
    assert_eq!(tokenize(line).count(), 1, "Failure Line: {line}");
    assert!(tokenize(line).all(|t| t.kind != TokenKind::LexerConfused));
  }
}
