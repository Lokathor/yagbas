use yagbas_asmtypes::{Asm, BinOp};

#[test]
fn check_asm_display() {
  let expected = "adc a, $FF";
  let actual = format!("{}", Asm::BinOpImm8(BinOp::Adc, 0xFF));
  assert_eq!(expected, actual);

  let expected = "db $01, $02";
  let actual = format!("{}", Asm::DataBytes(Box::new(vec![1, 2])));
  assert_eq!(expected, actual);

  let expected = "dw `12301223";
  let actual = format!("{}", Asm::TileIndexes(Box::new(vec![12301223])));
  assert_eq!(expected, actual);
}
