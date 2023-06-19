#![allow(clippy::while_let_on_iterator)]

use yagbas::Token;

fn main() {
  let prog = r#"
  section "main" @[$0150,rom0] {
    const FOO = 1_2;
    // comment1
    label1:
      ld a FOO;
      ld b $4_6;
      ld c %10671_010;
      d = $FF;
    1: /* foo bar baz */
      inc c;
      zero_bytes!(3);
      jr 1b;
  }"#;
  let mut lex = Token::lexer(prog);
  while let Some(result) = lex.next() {
    match result {
      Ok(t) => print!("{t:?}, "),
      Err(e) => {
        println!();
        println!("ERROR: {e:?}");
      }
    }
  }
  println!();
}
