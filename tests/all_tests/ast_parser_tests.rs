use std::path::PathBuf;

use yagbas::{
  ast::{Ast, actions::parse_ast_module, parser::AstParser},
  cst::{
    actions::gather_module,
    parser::{BuildTreeArgs, CstParser},
  },
};

#[track_caller]
fn ast_no_errors(src: &str) -> Ast {
  let file_origin = PathBuf::from("InMemoryData");
  let mut p = CstParser::new(&src);
  gather_module(&mut p);
  let cst = p.build_tree(BuildTreeArgs { skip_trivial: true });
  let mut ast_parser =
    AstParser { file_origin: file_origin.clone(), errors: Vec::new() };
  let module = parse_ast_module(&mut ast_parser, file_origin, &cst);
  let mut ast = Ast::default();
  ast.modules.push(module);
  ast.errors.extend(ast_parser.errors);
  assert!(ast.errors.is_empty(), "{:?}", ast.errors);
  ast
}

#[test]
fn test_empty_module() {
  ast_no_errors("");
  ast_no_errors(" ");
  ast_no_errors(
    "
    // comment
  ",
  );
}

#[test]
fn test_const() {
  ast_no_errors("const VBLANK_START: u8 = 144;");
  ast_no_errors("const EIGHT_ROWS_OF_TILES: u16 = TILES_PER_ROW * 8;");
}

#[test]
fn test_static_mmio() {
  ast_no_errors("static mmio($FF44) LY: u8;");
  ast_no_errors("static mmio($9800) TILEMAP_9800: [u8; 32*32];");
  ast_no_errors(
    "static mmio($8800) VRAM_BLOCK_8000: [[u8;BYTES_PER_TILE]; TILES_PER_BLOCK];",
  );
}

#[test]
fn test_function_empty() {
  ast_no_errors("fn foo() {}");
  ast_no_errors(
    "fn foo() {
      // comment
    }",
  );
}

#[test]
fn test_function_assign() {
  ast_no_errors(
    "fn foo() {
      *AUDIO_MAIN = 0;
    }",
  );
}

#[test]
fn test_function_let() {
  ast_no_errors(
    "fn foo() {
      let tile0 = &VRAM_BLOCK_8000[0][0];
    }",
  );
  ast_no_errors(
    "fn foo() {
      let pattern = $AA;
    }",
  );
}

#[test]
fn test_function_loop() {
  ast_no_errors(
    "fn foo() {
      loop {
        if *LY == VBLANK_START {
          break
        }
      }
      *LCDC = 0;
    }",
  );
}

#[test]
fn test_function_for() {
  ast_no_errors(
    "fn foo() {
      for _ in 0..BYTES_PER_TILE {
        *tile1 = pattern;
        *tile1 += 1;
      }
      *LCDC = 0;
    }",
  );
}

#[test]
fn test_function_if() {
  ast_no_errors(
    "fn foo() {
      if *LY == VBLANK_START {
        break
      }
      *LCDC = 0;
    }",
  );
}
