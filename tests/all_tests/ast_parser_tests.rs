use yagbas::ast::AstModule;

#[track_caller]
fn module_no_errors(src: &str) -> AstModule {
  let ast = AstModule::from_source(src);
  assert!(!ast.has_errors(), "===Ast Has Errors: {ast:?}");
  ast
}

#[test]
fn test_empty_module() {
  module_no_errors("");
  module_no_errors(" ");
  module_no_errors(
    "
    // comment
  ",
  );
}

#[test]
fn test_const() {
  module_no_errors("const VBLANK_START: u8 = 144;");
  module_no_errors("const EIGHT_ROWS_OF_TILES: u16 = TILES_PER_ROW * 8;");
}

#[test]
fn test_static_mmio() {
  module_no_errors("static mmio($FF44) LY: u8;");
  module_no_errors("static mmio($9800) TILEMAP_9800: [u8; 32*32];");
  module_no_errors(
    "static mmio($8800) VRAM_BLOCK_8000: [[u8;BYTES_PER_TILE]; TILES_PER_BLOCK];",
  );
}

#[test]
fn test_function_empty() {
  module_no_errors("fn foo() {}");
  module_no_errors(
    "fn foo() {
      // comment
    }",
  );
}

#[test]
fn test_function_assign() {
  module_no_errors(
    "fn foo() {
      *AUDIO_MAIN = 0;
    }",
  );
}

#[test]
fn test_function_let() {
  module_no_errors(
    "fn foo() {
      let tile0 = &VRAM_BLOCK_8000[0][0];
    }",
  );
  module_no_errors(
    "fn foo() {
      let pattern = $AA;
    }",
  );
}

#[test]
fn test_function_loop() {
  let m = module_no_errors(
    "fn foo() {
      loop {
        if *LY == VBLANK_START {
          break
        }
      }
      *LCDC = 0;
    }",
  );
  let item = &m.items[0];
  match &item.kind {
    yagbas::ast::AstItemKind::Function(f) => {
      // check that the loop gets properly made a separate statement from the
      // line after it, because the brace gets an implicit semicolon separator.
      assert_eq!(f.body.statements.len(), 2)
    }
    _ => panic!(),
  }
}

#[test]
fn test_function_for() {
  let m = module_no_errors(
    "fn foo() {
      for _ in 0..BYTES_PER_TILE {
        *tile1 = pattern;
        *tile1 += 1;
      }
      *LCDC = 0;
    }",
  );
  let item = &m.items[0];
  match &item.kind {
    yagbas::ast::AstItemKind::Function(f) => {
      // check that the loop gets properly made a separate statement from the
      // line after it, because the brace gets an implicit semicolon separator.
      assert_eq!(f.body.statements.len(), 2)
    }
    _ => panic!(),
  }
}

#[test]
fn test_function_if() {
  let m = module_no_errors(
    "fn foo() {
      if *LY == VBLANK_START {
        break
      }
      *LCDC = 0;
    }",
  );
  let item = &m.items[0];
  match &item.kind {
    yagbas::ast::AstItemKind::Function(f) => {
      // check that the loop gets properly made a separate statement from the
      // line after it, because the brace gets an implicit semicolon separator.
      assert_eq!(f.body.statements.len(), 2)
    }
    _ => panic!(),
  }
}
