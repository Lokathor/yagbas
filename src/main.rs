#![allow(clippy::while_let_on_iterator)]

use yagbas::{lexer::Token, parser::remove_comments};

fn main() {
  let prog = r#"
  const FOO = 1_2;
  section "main" @[$0150,rom0] {
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
  let lex = Token::lexer(prog);
  for (token_res, _) in remove_comments(lex.spanned()) {
    match token_res {
      Ok(Token::Punct(';')) => println!(";"),
      Ok(Token::Punct('{')) => {
        println!("{{")
      }
      Ok(Token::Punct('}')) => {
        println!("}}")
      }
      Ok(token) => {
        print!("{token:?} ");
      }
      Err(msg) => {
        println!();
        println!("ERROR: {msg}");
      }
    }
  }
  println!();
}
