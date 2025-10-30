; Sprite Objects Library - by Eievui
;
; This is a small, lightweight library meant to facilitate the rendering of
; sprite objects, including Shadow OAM and OAM DMA, single-entry "simple" sprite
; objects, and Q12.4 fixed-point position metasprite rendering.
;
; The library is only 127 bytes of ROM0, 160 bytes of WRAM0 for Shadow OAM, and a
; single HRAM byte for tracking the current position in OAM.
; (not counting the 8 required for OAM DMA)
;
; The library is relatively simple to use, with 4 steps to rendering:
; 1. Call InitSprObjLib during initilizations - This copies the OAMDMA function to
;    HRAM.
; 2. Call ResetShadowOAM at the beginning of each frame - This hides all sprites
;    and resets hOAMIndex, allowing you to render a new frame of sprites.
; 3. Call rendering functions - Push simple sprites or metasprites to Shadow OAM.
; 4. Wait for VBlank and call hOAMDMA - Copies wShadowOAM to the Game Boy's OAM in
;    just 160 M-cycles. Make sure to pass HIGH(wShadowOAM) in the a register.
;
; Copyright 2021, Eievui
;
; This software is provided 'as-is', without any express or implied
; warranty.  In no event will the authors be held liable for any damages
; arising from the use of this software.
; 
; Permission is granted to anyone to use this software for any purpose,
; including commercial applications, and to alter it and redistribute it
; freely, subject to the following restrictions:
; 
; 1. The origin of this software must not be misrepresented; you must not
;    claim that you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation would be
;    appreciated but is not required.
; 2. Altered source versions must be plainly marked as such, and must not be
;    misrepresented as being the original software.
; 3. This notice may not be removed or altered from any source distribution.
;

INCLUDE "hardware.inc"

SECTION "OAM DMA Code", ROM0
OAMDMACode::
LOAD "OAM DMA", HRAM
; Begin an OAM DMA, waiting 160 cycles for the DMA to finish.
; This quickly copies Shadow OAM to the Game Boy's OAM, allowing the PPU to draw
; the objects. hOAMDMA should be called once per frame near the end of your
; VBlank interrupt. While an OAM DMA is running no sprites objects can be drawn
; by the PPU, which makes it preferrable to run within the VBlank interrupt, but
; it can be run at any point if more than 40 sprite objects are needed.
; @param a: High byte of active Shadow OAM. Shadow OAM must be aligned to start
;           at the beginning of a page (low byte == $00).
hOAMDMA::
  ldh [rDMA], a
  ld a, 40
.wait
  dec a
  jr nz, .wait
  ret
ENDL
OAMDMACodeEnd::

SECTION "Initialize Sprite Object Library", ROM0
; Initializes the sprite object library, copying things such as the hOAMDMA
; function and reseting hOAMIndex
; @clobbers: a, bc, hl
InitSprObjLib::
  ; Copy OAM DMA.
  ld b, OAMDMACodeEnd - OAMDMACode
  ld c, LOW(hOAMDMA)
  ld hl, OAMDMACode
.memcpy
  ld a, [hli]
  ldh [c], a
  inc c
  dec b
  jr nz, .memcpy
  xor a, a
  ldh [hOAMIndex], a ; hOAMIndex must be reset before running ResetShadowOAM.
  ret

SECTION "Reset Shadow OAM", ROM0
; Reset the Y positions of every sprite object that was used in the last frame, 
; effectily hiding them, and reset hOAMIndex. Run this function each frame
; before rendering sprite objects.
; @clobbers: a, c, hl
ResetShadowOAM::
  xor a, a ; clear carry
  ldh a, [hOAMIndex]
  rra
  rra ; a / 4
  and a, a
  jr z, .skip
  ld c, a 
  ld hl, wShadowOAM
  xor a, a
.clearOAM
  ld [hli], a 
  inc l 
  inc l
  inc l 
  dec c
  jr nz, .clearOAM
  ldh [hOAMIndex], a
.skip
  ret

SECTION "Render Simple Sprite", ROM0
; Render a single object, or sprite, to OAM.
; @param b: Y position
; @param c: X position
; @param d: Tile ID
; @param e: Tile Attribute
; @clobbers: hl
RenderSimpleSprite::
  ld h, HIGH(wShadowOAM)
  ldh a, [hOAMIndex]
  ld l, a
  ld a, b
  add a, 16
  ld [hli], a
  ld a, c
  add a, 8
  ld [hli], a
  ld a, d
  ld [hli], a
  ld a, e
  ld [hli], a
  ld a, l
  ldh [hOAMIndex], a
  ret

SECTION "Render Metasprite", ROM0
; Render a metasprite to OAM.
; @param bc: Q12.4 fixed-point Y position.
; @param de: Q12.4 fixed-point X position.
; @param hl: Pointer to current metasprite.
RenderMetasprite::
  ; Adjust Y and store in b.
  ld a, c
  rrc b
  rra
  rrc b
  rra
  rrc b
  rra
  rrc b
  rra
  ld b, a
  ; Adjust X and store in c.
  ld a, e
  rrc d
  rra
  rrc d
  rra
  rrc d
  rra
  rrc d
  rra
  ld c, a
  ; Load Shadow OAM pointer.
  ld d, HIGH(wShadowOAM)
  ldh a, [hOAMIndex]
  ld e, a
  ; Now:
  ; bc - Y, X
  ; de - Shadow OAM
  ; hl - Metasprite
  ; Time to render!
.loop
  ; Load Y.
  ld a, [hli]
  add a, b
  ld [de], a
  inc e
  ; Load X.
  ld a, [hli]
  add a, c
  ld [de], a
  inc e
  ; Load Tile.
  ld a, [hli]
  ld [de], a
  inc e
  ; Load Attribute.
  ld a, [hli]
  ld [de], a
  inc e
  ; Check for null end byte.
  ld a, [hl]
  cp a, 128
  jr nz, .loop
  ld a, e
  ldh [hOAMIndex], a
  ret

SECTION "Shadow OAM", WRAM0, ALIGN[8]
wShadowOAM::
  ds 160

SECTION "Shadow OAM Index", HRAM
; The current low byte of shadow OAM.
hOAMIndex::
  db

; Sprite Objects Library example.
INCLUDE "hardware.inc"

SECTION "VBlank Vector", ROM0[$40]
  push af
  push bc
  push de
  push hl
  jp VBlankHandler

SECTION "Entry", ROM0[$100]
  jp Init
  ds $150 - @

SECTION "Main", ROM0
Init:
  ; Wait for VBlank
  ld a, [rLY]
  cp a, 144
  jr c, Init
  
  ; Disable screen
  xor a, a
  ld [rLCDC], a
  
  ; Set palettes
  ld a, %11100100
  ldh [rBGP], a
  ldh [rOBP0], a
  ldh [rOBP1], a
  
  ; Initilize Sprite Object Library.
  call InitSprObjLib
  
  ; Reset hardware OAM
  xor a, a
  ld b, 160
  ld hl, _OAMRAM
.resetOAM
  ld [hli], a
  dec b
  jr nz, .resetOAM
  
  ; Copy Graphics
  ld bc, GfxCat.end - GfxCat
  ld de, GfxCat
  ld hl, $8000
  call MemCopy
  
  ; Reset Positions
  ld c, 4
  ld hl, wSimplePosition
  xor a, a
: ld [hli], a
  dec c
  jr nz, :-
  
  ; Enable VBlank interrupt
  ld a, IEF_VBLANK
  ldh [rIE], a
  
  ; Clear pending interrupts
  xor a, a
  ldh [rIF], a
  
  ; Enable screen
  ld a, LCDCF_BGON | LCDCF_OBJON | LCDCF_OBJ8 | LCDCF_ON
  ldh [rLCDC], a
  ei
Main:
  call ResetShadowOAM
  
  ld de, $0000
  ld a, [wSimplePosition]
  ld c, a
  ld b, 0
  call RenderSimpleSprite
  sla c
  ld b, 16
  call RenderSimpleSprite
  sla c
  ld b, 32
  call RenderSimpleSprite
  sla c
  ld b, 48
  call RenderSimpleSprite
  ld bc, (96.0 >> 12) & $FFFF
  ld a, [wMetaspritePosition]
  ld e, a
  ld a, [wMetaspritePosition + 1]
  ld d, a
  ld hl, CatMetasprite
  call RenderMetasprite
  
  ld hl, wSimplePosition
  inc [hl]
  
  ld hl, wMetaspriteVelocity
  inc [hl]
  ld a, (2.0 >> 12) & $FF
  cp a, [hl]
  jr nz, .skip
  ld [hl], 0
.skip
  ld a, [wMetaspritePosition]
  add a, [hl]
  ld [wMetaspritePosition], a
  ld a, [wMetaspritePosition + 1]
  adc a, 0
  ld [wMetaspritePosition + 1], a
  
  halt
  jr Main

MemCopy::
  dec bc
  inc b
  inc c
.loop:
  ld a, [de]
  ld [hli], a
  inc de
  dec c
  jr nz, .loop
  dec b
  jr nz, .loop
  ret

SECTION "VBlank Handler", ROM0
VBlankHandler:
  ; Push sprites to OAM
  ld a, HIGH(wShadowOAM)
  call hOAMDMA
  
  pop hl
  pop de
  pop bc
  pop af
  reti

SECTION "Graphics", ROM0
GfxCat:
  INCBIN "cat.2bpp"
.end::

CatMetasprite:
  db 16, 8, 0, 0
  db 12, 16, 0, 0
  db 20, 20, 0, 0
  db 24, 12, 0, 0
  db 128

SECTION "Position Vars", WRAM0
; 8-bit X position
wSimplePosition:
  ds 1

; Q12.4 fixed-point X posiition
wMetaspritePosition:
  dw

; Q4.4 fixed-point velocity
wMetaspriteVelocity::
  db
