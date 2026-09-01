use yagbas::cst::Cst;

#[track_caller]
fn cst_no_errors(src: &str) -> Cst {
  let cst = Cst::from_module_src(src);
  cst.assert_no_errors();
  cst
}

#[test]
fn test_empty_module() {
  cst_no_errors("");
}

#[test]
fn test_trivia_module() {
  cst_no_errors(
    "
    // just a comment
    ",
  );
}

#[test]
fn test_empty_function() {
  cst_no_errors("fn foo() {}");
}

#[test]
fn test_empty_function2() {
  cst_no_errors(
    "fn foo() {
    }",
  );
}

#[test]
fn test_empty_function3() {
  cst_no_errors(
    "fn foo() {
      // comment
    }",
  );
}

#[test]
fn test_empty_let_lit_num() {
  cst_no_errors(
    "fn foo() {
      let _ = 1;
    }",
  );
}

#[test]
fn test_empty_let_lit_true() {
  cst_no_errors(
    "fn foo() {
      let _ = true;
    }",
  );
}

#[test]
fn test_empty_let_lit_false() {
  cst_no_errors(
    "fn foo() {
      let _ = false;
    }",
  );
}

#[test]
fn test_empty_let_lit_str() {
  cst_no_errors(
    "fn foo() {
      let _ = \"abc\";
    }",
  );
}

#[test]
fn test_empty_let_unit() {
  cst_no_errors(
    "fn foo() {
      let _ = ();
    }",
  );
}

#[test]
fn test_assign_static() {
  cst_no_errors(
    "fn foo() {
      *AUDIO_MAIN = 0;
    }",
  );
}

#[test]
fn test_stmt_loop() {
  cst_no_errors(
    "fn foo() {
      loop { }
    }",
  );
}

#[test]
fn test_stmt_for() {
  let _x = cst_no_errors(
    "fn foo() {
      for _ in 0..12 { }
    }",
  );
}

#[test]
fn test_stmt_for2() {
  let _x = cst_no_errors(
    "fn foo() {
      let tile0 = &VRAM_BLOCK_8000[0][0];
      let pattern = $AA;
      for _ in 0..BYTES_PER_TILE {
        *tile0 = pattern;
        *tile0 += 1;
      }
      pattern = $FF;
      for _ in 0..BYTES_PER_TILE {
        *tile1 = pattern;
        *tile1 += 1;
      }
    }",
  );
  //panic!("{_x}");
}

#[test]
fn test_stmt_if() {
  cst_no_errors(
    "fn foo() {
      if condition { x = 5; }
    }",
  );
}

#[test]
fn test_const_basic() {
  cst_no_errors("const VBLANK_START: u8 = 144;");
}

#[test]
fn test_const_with_sub_expression() {
  cst_no_errors("const EIGHT_ROWS_OF_TILES: u16 = TILES_PER_ROW * 8;");
}

#[test]
fn test_static_mmio_basic() {
  cst_no_errors("static mmio($FF44) LY: u8;");
}

#[test]
fn test_static_mmio_array_typed() {
  cst_no_errors("static mmio($9800) TILEMAP_9800: [u8; 32*32];");
}
