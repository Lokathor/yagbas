
fn main() {
  'wait_for_vblank: loop {
    if *LY == VBLANK_START {
      break
    }
  }
  *LCDC = LcdCtrl {};

  memcpy(VRAM, TileData, size_of!(TileData));
  memcpy(TILEMAP0, TilemapData, size_of!(TilemapData));
  *TILEMAP0 = 1;
  memclr(TILEMAP0 + 1, (TILEMAP1-TILEMAP0-1));
  *BGP = STANDARD_PALETTE;
  *SCX = 0;
  *SCY = 0;
  *LCDC = LcdCtrl {
    lcd_enabled, bg_win_enabled, bg_tilemap_1
  };
}

static rom TileData: [Tile; _] = include_tiles!("abandonauts-tilemap.2bpp");

static rom TilemapData: [u8; _] = include_tilemap!("abandonauts-tilemap.tilemap");
