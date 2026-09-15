use yagbas::cst::{Cst, CstKind, actions::do_module, parser::CstParser};

#[track_caller]
fn cst_no_errors(src: &str) -> Cst {
  let mut p = CstParser::new(src);
  let m = p.open();
  do_module(&mut p);
  p.close(m, CstKind::Module);
  let (cst, errors) = p.build_tree();
  assert!(errors.is_empty(), "Cst Parse Errors: {errors:?}");
  let exact_src = cst.to_source_code().unwrap();
  assert_eq!(src, exact_src);
  cst
}

#[test]
fn test_empty_module() {
  cst_no_errors("");
  cst_no_errors(" ");
  cst_no_errors("/**/");
}

#[test]
fn test_const() {
  cst_no_errors("const NAME: Type = expression;");
}

#[test]
fn test_static_mmio() {
  cst_no_errors("static mmio($FF40) LCDC: LcdControl;");
}

#[test]
fn test_static_ram() {
  cst_no_errors("static ram SCORE: u8 = 0;");
}

#[test]
fn test_static_rom() {
  cst_no_errors("static rom BASE_HP: [u8; 5] = [0, 3, 5, 12, 10];");
}

#[test]
fn test_function() {
  cst_no_errors("fn foo ( ) { }");
  cst_no_errors(
    "fn add_two(x: u8) -> u8 {
      x + 2
    }",
  );
}

#[test]
fn test_struct() {
  cst_no_errors(
    "struct OamData {
      y: u8,
      x: u8,
      tile_index: u8,
      attributes: OamAttrs,
    }",
  );
}

#[test]
fn test_bitbag() {
  cst_no_errors(
    "bitbag IrqFlags {
      vblank: 0,
      lcd: 1,
      timer: 2,
      serial: 3,
      joypad: 4,
    }",
  );
}

#[test]
fn test_use() {
  cst_no_errors("use core::memcpy;");
}

#[test]
fn test_impl() {
  cst_no_errors(
    "impl OamData {
      fn hide(&mut self) {
        self.y = 0;
      }
    }",
  );
}
