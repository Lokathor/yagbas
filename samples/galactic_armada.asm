; ANCHOR: entry-point
INCLUDE "src/main/utils/hardware.inc"

SECTION "GameVariables", WRAM0

wLastKeys:: db
wCurKeys:: db
wNewKeys:: db
wGameState::db

SECTION "Header", ROM0[$100]

	jp EntryPoint

	ds $150 - @, 0 ; Make room for the header

EntryPoint:
; ANCHOR_END: entry-point
	
; ANCHOR: entry-point-end
	; Shut down audio circuitry
	xor a
	ld [rNR52], a
	; We don't actually need another xor a here, because the value of A doesn't change between these two instructions
	ld [wGameState], a

	; Wait for the vertical blank phase before initiating the library
    call WaitForOneVBlank

	; from: https://github.com/eievui5/gb-sprobj-lib
	; The library is relatively simple to get set up. First, put the following in your initialization code:
	; Initilize Sprite Object Library.
	call InitSprObjLibWrapper

	; Turn the LCD off
	xor a
	ld [rLCDC], a

	; Load our common text font into VRAM
	call LoadTextFontIntoVRAM

	; Turn the LCD on
	ld a, LCDCF_ON  | LCDCF_BGON|LCDCF_OBJON | LCDCF_OBJ16 | LCDCF_WINON | LCDCF_WIN9C00
	ld [rLCDC], a

	; During the first (blank) frame, initialize display registers
	ld a, %11100100
	ld [rBGP], a
	ld [rOBP0], a

; ANCHOR_END: entry-point-end
; ANCHOR: next-game-state

NextGameState::

	; Do not turn the LCD off outside of VBlank
    call WaitForOneVBlank

	call ClearBackground


	; Turn the LCD off
	xor a
	ld [rLCDC], a

	ld [rSCX], a
	ld [rSCY], a
	ld [rWX], a
	ld [rWY], a
	; disable interrupts
	call DisableInterrupts
	
	; Clear all sprites
	call ClearAllSprites

	; Initiate the next state
	ld a, [wGameState]
	cp 2 ; 2 = Gameplay
	call z, InitGameplayState
	ld a, [wGameState]
	cp 1 ; 1 = Story
	call z, InitStoryState
	ld a, [wGameState]
	and a ; 0 = Menu
	call z, InitTitleScreenState

	; Update the next state
	ld a, [wGameState]
	cp 2 ; 2 = Gameplay
	jp z, UpdateGameplayState
	cp 1 ; 1 = Story
	jp z, UpdateStoryState
	jp UpdateTitleScreenState

; ANCHOR_END: next-game-state

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; It's straight from: https://gbdev.io/gb-asm-tutorial/part2/input.html
; In their words (paraphrased): reading player input for gameboy is NOT a trivial task
; So it's best to use some tested code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

include "src/main/utils/hardware.inc"

 SECTION "Input", ROM0

Input::
  ; Poll half the controller
  ld a, P1F_GET_BTN
  call .onenibble
  ld b, a ; B7-4 = 1; B3-0 = unpressed buttons

  ; Poll the other half
  ld a, P1F_GET_DPAD
  call .onenibble
  swap a ; A3-0 = unpressed directions; A7-4 = 1
  xor a, b ; A = pressed buttons + directions
  ld b, a ; B = pressed buttons + directions

  ; And release the controller
  ld a, P1F_GET_NONE
  ldh [rP1], a

  ; Combine with previous wCurKeys to make wNewKeys
  ld a, [wCurKeys]
  xor a, b ; A = keys that changed state
  and a, b ; A = keys that changed to pressed
  ld [wNewKeys], a
  ld a, b
  ld [wCurKeys], a
  ret

.onenibble
  ldh [rP1], a ; switch the key matrix
  call .knownret ; burn 10 cycles calling a known ret
  ldh a, [rP1] ; ignore value while waiting for the key matrix to settle
  ldh a, [rP1]
  ldh a, [rP1] ; this read counts
  or a, $F0 ; A7-4 = 1; A3-0 = unpressed keys
.knownret
  ret

; Sprite Objects Library - by Eievui
;
; This is a small, lightweight library meant to facilitate the rendering of
; sprite objects, including Shadow OAM and OAM DMA, single-entry "simple" sprite
; objects, and Q12.4 fixed-point position metasprite rendering.
;
; The library is only 127 bytes of ROM0, 160 bytes of WRAM0 for Shadow OAM, and a
; single HRAM byte for tracking the current position in OAM.
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

INCLUDE "src/main/utils/hardware.inc"

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

; A wrapper or the InitSprObjLib code
; from: https://github.com/eievui5/gb-sprobj-lib
; The library is relatively simple to get set up. First, put the following in your initialization code:
; Initilize Sprite Object Library.
InitSprObjLibWrapper::

  call InitSprObjLib
	; Reset hardware OAM
	xor a, a
	ld b, 160
	ld hl, _OAMRAM
	
.resetOAM
	ld [hli], a
	dec b
	jr nz, .resetOAM
  
  ret

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


; ANCHOR: enemy-bullet-collision-start
include "src/main/utils/hardware.inc"
include "src/main/utils/constants.inc"
include "src/main/utils/hardware.inc"

SECTION "EnemyBulletCollisionVariables", WRAM0

wEnemyBulletCollisionCounter: db
wBulletAddresses: dw

SECTION "EnemyBulletCollision", ROM0

; called from enemies.asm
CheckCurrentEnemyAgainstBullets::


    ld a, l
    ld [wUpdateEnemiesCurrentEnemyAddress], a
    ld a, h
    ld [wUpdateEnemiesCurrentEnemyAddress+1], a

    xor a
    ld [wEnemyBulletCollisionCounter], a
    
    ; Copy our bullets address into wBulletAddress
    ld a, LOW(wBullets)
    ld l, a
    ld a, HIGH(wBullets)
    ld h, a

    jp CheckCurrentEnemyAgainstBullets_PerBullet
; ANCHOR_END: enemy-bullet-collision-start

; ANCHOR: enemy-bullet-collision-loop
CheckCurrentEnemyAgainstBullets_Loop:

    ; increase our counter
    ld a, [wEnemyBulletCollisionCounter]
    inc a
    ld [wEnemyBulletCollisionCounter], a

    ; Stop if we've checked all bullets
    cp MAX_BULLET_COUNT
    ret nc

    ; Increase the  data our address is pointing to
    ld a, l
    add PER_BULLET_BYTES_COUNT
    ld l, a
    ld a, h
    adc 0
    ld h, a
; ANCHOR_END: enemy-bullet-collision-loop


; ANCHOR: enemy-bullet-collision-per-bullet-start
CheckCurrentEnemyAgainstBullets_PerBullet:

    ld a, [hl]
    cp 1
    jp nz, CheckCurrentEnemyAgainstBullets_Loop
; ANCHOR_END: enemy-bullet-collision-per-bullet-start

; ANCHOR: enemy-bullet-collision-per-bullet-x-overlap
CheckCurrentEnemyAgainstBullets_Check_X_Overlap:

    ; Save our first byte address
    push hl

    inc hl

    ; Get our x position
    ld a, [hli]
    add 4
    ld b, a

    push hl

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Start: Checking the absolute difference
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ; The first value
    ld a, b
    ld [wObject1Value], a

    ; The second value
    ld a, [wCurrentEnemyX]
    add 8
    ld [wObject2Value], a

    ; Save if the minimum distance
    ld a, 12
    ld [wSize], a

    call CheckObjectPositionDifference

    
    ld a, [wResult]
    and a
    jp z, CheckCurrentEnemyAgainstBullets_Check_X_Overlap_Fail

    
    pop hl

    jp CheckCurrentEnemyAgainstBullets_PerBullet_Y_Overlap

CheckCurrentEnemyAgainstBullets_Check_X_Overlap_Fail:

    pop hl
    pop hl

    jp CheckCurrentEnemyAgainstBullets_Loop
; ANCHOR_END: enemy-bullet-collision-per-bullet-x-overlap

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; End: Checking the absolute difference
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ANCHOR: enemy-bullet-collision-per-bullet-y-overlap
    
CheckCurrentEnemyAgainstBullets_PerBullet_Y_Overlap:

    ; get our bullet 16-bit y position
    ld a, [hli]
    ld b, a

    ld a, [hli]
    ld c, a

    ; Descale our 16 bit y position
    srl c
    rr b
    srl c
    rr b
    srl c
    rr b
    srl c
    rr b

    ; preserve our first byte addresss
    pop hl
    push hl

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Start: Checking the absolute difference
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ; The first value
    ld a, b
    ld [wObject1Value], a

    ; The second value
    ld a, [wCurrentEnemyY]
    ld [wObject2Value], a

    ; Save if the minimum distance
    ld a, 16
    ld [wSize], a

    call CheckObjectPositionDifference

    pop hl
    
    ld a, [wResult]
    and a
    jp z, CheckCurrentEnemyAgainstBullets_Loop
    jp CheckCurrentEnemyAgainstBullets_PerBullet_Collision

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; End: Checking the absolute difference
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
; ANCHOR_END: enemy-bullet-collision-per-bullet-y-overlap


; ANCHOR: enemy-bullet-collision-per-bullet-collision
CheckCurrentEnemyAgainstBullets_PerBullet_Collision:

    ; set the active byte  and x value to 0 for bullets
    xor a
    ld [hli], a
    ld [hl], a

    ld a, [wUpdateEnemiesCurrentEnemyAddress+0]
    ld l, a
    ld a, [wUpdateEnemiesCurrentEnemyAddress+1]
    ld h, a

    ; set the active byte  and x value to 0 for enemies
    xor a
    ld [hli], a
    ld [hl], a
    
    call IncreaseScore
    call DrawScore

    ; Decrease how many active enemies their are
    ld a, [wActiveEnemyCounter]
    dec a
    ld [wActiveEnemyCounter], a

    ; Decrease how many active bullets their are
    ld a, [wActiveBulletCounter]
    dec a
    ld [wActiveBulletCounter], a

    ret
; ANCHOR_END: enemy-bullet-collision-per-bullet-collision

; ANCHOR: enemies-start
include "src/main/utils/hardware.inc"
include "src/main/utils/constants.inc"

SECTION "EnemiesPlayerCollision", ROM0

; ANCHOR: get-player-x
CheckEnemyPlayerCollision::

    ; Get our player's unscaled x position in d
    ld a, [wPlayerPositionX]
    ld d, a

    ld a, [wPlayerPositionX+1]
    ld e, a

    srl e
    rr d
    srl e
    rr d
    srl e
    rr d
    srl e
    rr d
    
; ANCHOR_END: get-player-x

; ANCHOR: check-x-overlap

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Check the x distances. Jump to 'NoCollisionWithPlayer' on failure
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ld a, [wCurrentEnemyX]
    ld [wObject1Value], a

    ld a, d
    ld [wObject2Value], a

    ; Save if the minimum distance
    ld a, 16
    ld [wSize], a

    call CheckObjectPositionDifference

    ld a, [wResult]
    and a
    jp z, NoCollisionWithPlayer
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    
; ANCHOR_END: check-x-overlap

; ANCHOR: get-y
    ; Get our player's unscaled y position in d
    ld a, [wPlayerPositionY+0]
    ld d, a

    ld a, [wPlayerPositionY+1]
    ld e, a

    srl e
    rr d
    srl e
    rr d
    srl e
    rr d
    srl e
    rr d

; ANCHOR_END: get-y


; ANCHOR: check-y-overlap

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Check the y distances. Jump to 'NoCollisionWithPlayer' on failure
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    ld a, [wCurrentEnemyY]
    ld [wObject1Value], a

    ld a, d
    ld [wObject2Value], a

    ; Save if the minimum distance
    ld a, 16
    ld [wSize], a

    call CheckObjectPositionDifference

    ld a, [wResult]
    and a
    jp z, NoCollisionWithPlayer
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ANCHOR_END: check-y-overlap

; ANCHOR: result

    ld a, 1
    ld [wResult], a

    ret
    
NoCollisionWithPlayer::

    xor a
    ld [wResult], a

    ret

; ANCHOR_END: result


; ANCHOR: bullets-top
include "src/main/utils/hardware.inc"
include "src/main/utils/constants.inc"

SECTION "BulletVariables", WRAM0

wSpawnBullet: db

; how many bullets are currently active
wActiveBulletCounter:: db

; how many bullet's we've updated
wUpdateBulletsCounter: db 

; ANCHOR: w-bullets

; Bytes: active, x , y (low), y (high)
wBullets:: ds MAX_BULLET_COUNT*PER_BULLET_BYTES_COUNT

; ANCHOR_END: w-bullets

SECTION "Bullets", ROM0

bulletMetasprite::
    .metasprite1    db 0,0,8,0
    .metaspriteEnd  db 128

; ANCHOR: bullets-tile-data
bulletTileData:: INCBIN "src/generated/sprites/bullet.2bpp"
bulletTileDataEnd::
; ANCHOR_END: bullets-tile-data


; ANCHOR_END: bullets-top

; ANCHOR: bullets-initialize
InitializeBullets::

    xor a
    ld [wSpawnBullet], a

    ; Copy the bullet tile data intto vram
	ld de, bulletTileData
	ld hl, BULLET_TILES_START
	ld bc, bulletTileDataEnd - bulletTileData
    call CopyDEintoMemoryAtHL

    ; Reset how many bullets are active to 0
    xor a
    ld [wActiveBulletCounter],a

    ld b, a
    ld hl, wBullets
    ld [hl], a

InitializeBullets_Loop:

    ; Increase the address
    ld a, l
    add PER_BULLET_BYTES_COUNT
    ld l, a
    ld a, h
    adc 0
    ld h, a

    ; Increase how many bullets we have initailized
    ld a, b
    inc a
    ld b, a

    cp MAX_BULLET_COUNT
    ret z

    jp InitializeBullets_Loop
; ANCHOR_END: bullets-initialize

; ANCHOR: bullets-update-start
UpdateBullets::

    ; Make sure we have SOME active enemies
    ld a, [wSpawnBullet]
    ld b, a
    ld a, [wActiveBulletCounter]
    or b
    cp 0
    ret z
    
    ; Reset our counter for how many bullets we have checked
    xor a
    ld [wUpdateBulletsCounter], a

    ; Get the address of the first bullet in hl
    ld a, LOW(wBullets)
    ld l, a
    ld a, HIGH(wBullets)
    ld h, a

    jp UpdateBullets_PerBullet
; ANCHOR_END: bullets-update-start

; ANCHOR: bullets-update-loop
UpdateBullets_Loop:

    ; Check our counter, if it's zero
    ; Stop the function
    ld a, [wUpdateBulletsCounter]
    inc a
    ld [wUpdateBulletsCounter], a

    ; Check if we've already
    ld a, [wUpdateBulletsCounter]
    cp MAX_BULLET_COUNT
    ret nc

    ; Increase the bullet data our address is pointingtwo
    ld a, l
    add PER_BULLET_BYTES_COUNT
    ld l, a
    ld a, h
    adc 0
    ld h, a
; ANCHOR_END: bullets-update-loop

; ANCHOR: bullets-update-per
UpdateBullets_PerBullet:

    ; The first byte is if the bullet is active
    ; If it's NOT  zero, it's active, go to the normal update section
    ld a, [hl]
    and a
    jp nz, UpdateBullets_PerBullet_Normal

    ; Do we need to spawn a bullet?
    ; If we dont, loop to the next enemy
    ld a, [wSpawnBullet]
    and a
    jp z, UpdateBullets_Loop
    
UpdateBullets_PerBullet_SpawnDeactivatedBullet:

    ; reset this variable so we don't spawn anymore
    xor a
    ld [wSpawnBullet], a
    
    ; Increase how many bullets are active
    ld a, [wActiveBulletCounter]
    inc a
    ld [wActiveBulletCounter], a

    push hl

    ; Set the current bullet as  active
    ld a, 1
    ld [hli], a

    ; Get the unscaled player x position in b
    ld a, [wPlayerPositionX]
    ld b, a
    ld a, [wPlayerPositionX+1]
    ld d, a
    
    ; Descale the player's x position
    ; the result will only be in the low byt
    srl d
    rr b
    srl d
    rr b
    srl d
    rr b
    srl d
    rr b
    
    ; Set the x position to equal the player's x position
    ld a, b
    ld [hli], a

    ; Set the y position (low)
    ld a, [wPlayerPositionY]
    ld [hli], a

    ; Set the y position (high)
    ld a, [wPlayerPositionY+1]
    ld [hli], a

    pop hl

UpdateBullets_PerBullet_Normal:

    ; Save our active byte
    push hl

    inc hl

    ; Get our x position
    ld a, [hli]
    ld b, a

    ; get our 16-bit y position
    ld a, [hl]
    sub BULLET_MOVE_SPEED
    ld [hli], a
    ld c, a
    ld a, [hl] 
    sbc 0
    ld [hl], a
    ld d, a

    pop hl; go to the active byte

    ; Descale our y position
    srl d
    rr c
    srl d
    rr c
    srl d
    rr c
    srl d
    rr c

    ; See if our non scaled low byte is above 160
    ld a, c
    cp 178
    ; If it's below 160, deactivate
    jp nc, UpdateBullets_DeActivateIfOutOfBounds
    
; ANCHOR_END: bullets-update-per
; ANCHOR: draw-bullets

    push hl

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Drawing a metasprite
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     ; Save the address of the metasprite into the 'wMetaspriteAddress' variable
    ; Our DrawMetasprites functoin uses that variable
    ld a, LOW(bulletMetasprite)
    ld [wMetaspriteAddress], a
    ld a, HIGH(bulletMetasprite)
    ld [wMetaspriteAddress+1], a

    ; Save the x position
    ld a, b
    ld [wMetaspriteX], a

    ; Save the y position
    ld a, c
    ld [wMetaspriteY], a

    ; Actually call the 'DrawMetasprites function
    call DrawMetasprites
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    

    pop hl
    
    jp UpdateBullets_Loop
; ANCHOR_END: draw-bullets

; ANCHOR: deactivate-bullets
UpdateBullets_DeActivateIfOutOfBounds:

    ; if it's y value is grater than 160
    ; Set as inactive
    xor a
    ld [hl], a

    ; Decrease counter
    ld a,[wActiveBulletCounter]
    dec a
    ld [wActiveBulletCounter], a

    jp UpdateBullets_Loop
; ANCHOR_END: deactivate-bullets
    
; ANCHOR: fire-bullets
FireNextBullet::

    ; Make sure we don't have the max amount of enmies
    ld a, [wActiveBulletCounter]
    cp MAX_BULLET_COUNT
    ret nc

    ; Set our spawn bullet variable to true
    ld a, 1
    ld [wSpawnBullet], a

    ret
; ANCHOR_END: fire-bullets

; ANCHOR: enemies-start
include "src/main/utils/hardware.inc"
include "src/main/utils/constants.inc"

SECTION "EnemyVariables", WRAM0

wCurrentEnemyX:: db  
wCurrentEnemyY:: db  

wSpawnCounter: db  
wNextEnemyXPosition: db
wActiveEnemyCounter::db
wUpdateEnemiesCounter:db
wUpdateEnemiesCurrentEnemyAddress::dw

; ANCHOR: w-enemies
; Bytes: active, x , y (low), y (high), speed, health
wEnemies:: ds MAX_ENEMY_COUNT*PER_ENEMY_BYTES_COUNT

; ANCHOR_END: w-enemies

; ANCHOR_END: enemies-start
; ANCHOR: enemies-section-header
SECTION "Enemies", ROM0
; ANCHOR_END: enemies-section-header

; ANCHOR: enemies-tile-data
enemyShipTileData:: INCBIN "src/generated/sprites/enemy-ship.2bpp"
enemyShipTileDataEnd::
; ANCHOR_END: enemies-tile-data

; ANCHOR: enemy-metasprites
enemyShipMetasprite::
    .metasprite1    db 0,0,4,0
    .metasprite2    db 0,8,6,0
    .metaspriteEnd  db 128
; ANCHOR_END: enemy-metasprites

; ANCHOR: enemies-initialize
InitializeEnemies::

	ld de, enemyShipTileData
	ld hl, ENEMY_TILES_START
	ld bc, enemyShipTileDataEnd - enemyShipTileData
    call CopyDEintoMemoryAtHL

    xor a
    ld [wSpawnCounter], a
    ld [wActiveEnemyCounter], a
    ld [wNextEnemyXPosition], a

    ld b, a

    ld hl, wEnemies

InitializeEnemies_Loop:

    ; Set as inactive
    ld [hl], 0
    
    ; Increase the address
    ld a, l
    add PER_ENEMY_BYTES_COUNT
    ld l, a
    ld a, h
    adc 0
    ld h, a

    inc b
    ld a, b

    cp MAX_ENEMY_COUNT
    ret z

    jp InitializeEnemies_Loop
; ANCHOR_END: enemies-initialize

; ANCHOR: enemies-update-start
UpdateEnemies::

	call TryToSpawnEnemies

    ; Make sure we have active enemies
    ; or we want to spawn a new enemy
    ld a, [wNextEnemyXPosition]
    ld b, a
    ld a, [wActiveEnemyCounter]
    or b
    and a
    ret z
    
    xor a
    ld [wUpdateEnemiesCounter], a

    ld a, LOW(wEnemies)
    ld l, a
    ld a, HIGH(wEnemies)
    ld h, a

    jp UpdateEnemies_PerEnemy
; ANCHOR_END: enemies-update-start
; ANCHOR: enemies-update-loop
UpdateEnemies_Loop:

    ; Check our coutner, if it's zero
    ; Stop the function
    ld a, [wUpdateEnemiesCounter]
    inc a
    ld [wUpdateEnemiesCounter], a

    ; Compare against the active count
    cp MAX_ENEMY_COUNT
    ret nc

    ; Increase the enemy data our address is pointingtwo
    ld a, l
    add PER_ENEMY_BYTES_COUNT
    ld l, a
    ld a, h
    adc 0
    ld h, a
; ANCHOR_END: enemies-update-loop


; ANCHOR: enemies-update-per-enemy
UpdateEnemies_PerEnemy:

    ; The first byte is if the current object is active
    ; If it's not zero, it's active, go to the normal update section
    ld a, [hl]
    and a
    jp nz, UpdateEnemies_PerEnemy_Update

UpdateEnemies_SpawnNewEnemy:

    ; If this enemy is NOT active
    ; Check If we want to spawn a new enemy
    ld a, [wNextEnemyXPosition]
    and a

    ; If we don't want to spawn a new enemy, we'll skip this (deactivated) enemy
    jp z, UpdateEnemies_Loop

    push hl

    ; If they are deactivated, and we want to spawn an enemy
    ; activate the enemy
    ld a, 1
    ld [hli], a

    ; Put the value for our enemies x position
    ld a, [wNextEnemyXPosition]
    ld [hli], a

    ; Put the value for our enemies y position to equal 0
    xor a
    ld [hli], a
    ld [hld], a
    ld [wNextEnemyXPosition], a

    pop hl
    
    ; Increase counter
    ld a, [wActiveEnemyCounter]
    inc a
    ld [wActiveEnemyCounter], a

; ANCHOR_END: enemies-update-per-enemy

; ANCHOR: enemies-update-per-enemy2
UpdateEnemies_PerEnemy_Update:

    ; Save our first bytye
    push hl

    ; Get our move speed in e
    ld bc, enemy_speedByte
    add hl, bc
    ld a, [hl]
    ld e, a

    ; Go back to the first byte
    ; put the address toe the first byte back on the stack for later
    pop hl
    push hl

    inc hl

    ; Get our x position
    ld a, [hli]
    ld b, a
    ld [wCurrentEnemyX], a

    ; get our 16-bit y position
    ; increase it (by e), but also save it 
    ld a, [hl]
    add 10
    ld [hli], a
    ld c, a
    ld a, [hl]
    adc 0
    ld [hl], a
    ld d, a

    pop hl

    ; Descale the y psoition
    srl d
    rr c
    srl d
    rr c
    srl d
    rr c
    srl d
    rr c

    ld a, c
    ld [wCurrentEnemyY], a

; ANCHOR_END: enemies-update-per-enemy2
    

; ANCHOR: enemies-update-check-collision
UpdateEnemies_PerEnemy_CheckPlayerCollision:

    push hl

    call CheckCurrentEnemyAgainstBullets
    call CheckEnemyPlayerCollision

    pop hl

    ld a, [wResult]
    and a
    jp z, UpdateEnemies_NoCollisionWithPlayer 
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    push hl

    call DamagePlayer
    call DrawLives

    pop hl
    
    jp UpdateEnemies_DeActivateEnemy
; ANCHOR_END: enemies-update-check-collision


; ANCHOR: enemies-update-deactivate
UpdateEnemies_DeActivateEnemy:

    ; Set as inactive
    xor a
    ld [hl], a

    ; Decrease counter
    ld a, [wActiveEnemyCounter]
    dec a
    ld [wActiveEnemyCounter], a

    jp UpdateEnemies_Loop

; ANCHOR_END: enemies-update-deactivate

; ANCHOR: enemies-update-nocollision
UpdateEnemies_NoCollisionWithPlayer::

    ; See if our non scaled low byte is above 160
    ld a, [wCurrentEnemyY]
    cp 160
    jp nc, UpdateEnemies_DeActivateEnemy

    push hl


; ANCHOR: draw-enemy-metasprites

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; call the 'DrawMetasprites function. setup variables and call
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ; Save the address of the metasprite into the 'wMetaspriteAddress' variable
    ; Our DrawMetasprites functoin uses that variable
    ld a, LOW(enemyShipMetasprite)
    ld [wMetaspriteAddress+0], a
    ld a, HIGH(enemyShipMetasprite)
    ld [wMetaspriteAddress+1], a

    ; Save the x position
    ld a, [wCurrentEnemyX]
    ld [wMetaspriteX], a

    ; Save the y position
    ld a, [wCurrentEnemyY]
    ld [wMetaspriteY], a

    ; Actually call the 'DrawMetasprites function
    call DrawMetasprites

; ANCHOR_END: draw-enemy-metasprites

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    pop hl
    
    jp UpdateEnemies_Loop
; ANCHOR_END: enemies-update-nocollision



; ANCHOR: enemies-spawn
TryToSpawnEnemies::

    ; Increase our spwncounter
    ld a, [wSpawnCounter]
    inc a
    ld [wSpawnCounter], a

    ; Check our spawn acounter
    ; Stop if it's below a given value
    ld a, [wSpawnCounter]
    cp ENEMY_SPAWN_DELAY_MAX
    ret c

    ; Check our next enemy x position variable
    ; Stop if it's non zero
    ld a, [wNextEnemyXPosition]
    cp 0
    ret nz

    ; Make sure we don't have the max amount of enmies
    ld a, [wActiveEnemyCounter]
    cp MAX_ENEMY_COUNT
    ret nc

GetSpawnPosition:

    ; Generate a semi random value
    call rand
    
    ; make sure it's not above 150
    ld a, b
    cp 150
    ret nc

    ; make sure it's not below 24
    ld a, b
    cp 24
    ret c

    ; reset our spawn counter
    xor a
    ld [wSpawnCounter], a
    
    ld a, b
    ld [wNextEnemyXPosition], a


    ret
; ANCHOR_END: enemies-spawn

; ANCHOR: enemies-start
include "src/main/utils/hardware.inc"
include "src/main/utils/constants.inc"

SECTION "EnemyVariables", WRAM0

wCurrentEnemyX:: db  
wCurrentEnemyY:: db  

wSpawnCounter: db  
wNextEnemyXPosition: db
wActiveEnemyCounter::db
wUpdateEnemiesCounter:db
wUpdateEnemiesCurrentEnemyAddress::dw

; ANCHOR: w-enemies
; Bytes: active, x , y (low), y (high), speed, health
wEnemies:: ds MAX_ENEMY_COUNT*PER_ENEMY_BYTES_COUNT

; ANCHOR_END: w-enemies

; ANCHOR_END: enemies-start
; ANCHOR: enemies-section-header
SECTION "Enemies", ROM0
; ANCHOR_END: enemies-section-header

; ANCHOR: enemies-tile-data
enemyShipTileData:: INCBIN "src/generated/sprites/enemy-ship.2bpp"
enemyShipTileDataEnd::
; ANCHOR_END: enemies-tile-data

; ANCHOR: enemy-metasprites
enemyShipMetasprite::
    .metasprite1    db 0,0,4,0
    .metasprite2    db 0,8,6,0
    .metaspriteEnd  db 128
; ANCHOR_END: enemy-metasprites

; ANCHOR: enemies-initialize
InitializeEnemies::

	ld de, enemyShipTileData
	ld hl, ENEMY_TILES_START
	ld bc, enemyShipTileDataEnd - enemyShipTileData
    call CopyDEintoMemoryAtHL

    xor a
    ld [wSpawnCounter], a
    ld [wActiveEnemyCounter], a
    ld [wNextEnemyXPosition], a

    ld b, a

    ld hl, wEnemies

InitializeEnemies_Loop:

    ; Set as inactive
    ld [hl], 0
    
    ; Increase the address
    ld a, l
    add PER_ENEMY_BYTES_COUNT
    ld l, a
    ld a, h
    adc 0
    ld h, a

    inc b
    ld a, b

    cp MAX_ENEMY_COUNT
    ret z

    jp InitializeEnemies_Loop
; ANCHOR_END: enemies-initialize

; ANCHOR: enemies-update-start
UpdateEnemies::

	call TryToSpawnEnemies

    ; Make sure we have active enemies
    ; or we want to spawn a new enemy
    ld a, [wNextEnemyXPosition]
    ld b, a
    ld a, [wActiveEnemyCounter]
    or b
    and a
    ret z
    
    xor a
    ld [wUpdateEnemiesCounter], a

    ld a, LOW(wEnemies)
    ld l, a
    ld a, HIGH(wEnemies)
    ld h, a

    jp UpdateEnemies_PerEnemy
; ANCHOR_END: enemies-update-start
; ANCHOR: enemies-update-loop
UpdateEnemies_Loop:

    ; Check our coutner, if it's zero
    ; Stop the function
    ld a, [wUpdateEnemiesCounter]
    inc a
    ld [wUpdateEnemiesCounter], a

    ; Compare against the active count
    cp MAX_ENEMY_COUNT
    ret nc

    ; Increase the enemy data our address is pointingtwo
    ld a, l
    add PER_ENEMY_BYTES_COUNT
    ld l, a
    ld a, h
    adc 0
    ld h, a
; ANCHOR_END: enemies-update-loop


; ANCHOR: enemies-update-per-enemy
UpdateEnemies_PerEnemy:

    ; The first byte is if the current object is active
    ; If it's not zero, it's active, go to the normal update section
    ld a, [hl]
    and a
    jp nz, UpdateEnemies_PerEnemy_Update

UpdateEnemies_SpawnNewEnemy:

    ; If this enemy is NOT active
    ; Check If we want to spawn a new enemy
    ld a, [wNextEnemyXPosition]
    and a

    ; If we don't want to spawn a new enemy, we'll skip this (deactivated) enemy
    jp z, UpdateEnemies_Loop

    push hl

    ; If they are deactivated, and we want to spawn an enemy
    ; activate the enemy
    ld a, 1
    ld [hli], a

    ; Put the value for our enemies x position
    ld a, [wNextEnemyXPosition]
    ld [hli], a

    ; Put the value for our enemies y position to equal 0
    xor a
    ld [hli], a
    ld [hld], a
    ld [wNextEnemyXPosition], a

    pop hl
    
    ; Increase counter
    ld a, [wActiveEnemyCounter]
    inc a
    ld [wActiveEnemyCounter], a

; ANCHOR_END: enemies-update-per-enemy

; ANCHOR: enemies-update-per-enemy2
UpdateEnemies_PerEnemy_Update:

    ; Save our first bytye
    push hl

    ; Get our move speed in e
    ld bc, enemy_speedByte
    add hl, bc
    ld a, [hl]
    ld e, a

    ; Go back to the first byte
    ; put the address toe the first byte back on the stack for later
    pop hl
    push hl

    inc hl

    ; Get our x position
    ld a, [hli]
    ld b, a
    ld [wCurrentEnemyX], a

    ; get our 16-bit y position
    ; increase it (by e), but also save it 
    ld a, [hl]
    add 10
    ld [hli], a
    ld c, a
    ld a, [hl]
    adc 0
    ld [hl], a
    ld d, a

    pop hl

    ; Descale the y psoition
    srl d
    rr c
    srl d
    rr c
    srl d
    rr c
    srl d
    rr c

    ld a, c
    ld [wCurrentEnemyY], a

; ANCHOR_END: enemies-update-per-enemy2
    

; ANCHOR: enemies-update-check-collision
UpdateEnemies_PerEnemy_CheckPlayerCollision:

    push hl

    call CheckCurrentEnemyAgainstBullets
    call CheckEnemyPlayerCollision

    pop hl

    ld a, [wResult]
    and a
    jp z, UpdateEnemies_NoCollisionWithPlayer 
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    push hl

    call DamagePlayer
    call DrawLives

    pop hl
    
    jp UpdateEnemies_DeActivateEnemy
; ANCHOR_END: enemies-update-check-collision


; ANCHOR: enemies-update-deactivate
UpdateEnemies_DeActivateEnemy:

    ; Set as inactive
    xor a
    ld [hl], a

    ; Decrease counter
    ld a, [wActiveEnemyCounter]
    dec a
    ld [wActiveEnemyCounter], a

    jp UpdateEnemies_Loop

; ANCHOR_END: enemies-update-deactivate

; ANCHOR: enemies-update-nocollision
UpdateEnemies_NoCollisionWithPlayer::

    ; See if our non scaled low byte is above 160
    ld a, [wCurrentEnemyY]
    cp 160
    jp nc, UpdateEnemies_DeActivateEnemy

    push hl


; ANCHOR: draw-enemy-metasprites

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; call the 'DrawMetasprites function. setup variables and call
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ; Save the address of the metasprite into the 'wMetaspriteAddress' variable
    ; Our DrawMetasprites functoin uses that variable
    ld a, LOW(enemyShipMetasprite)
    ld [wMetaspriteAddress+0], a
    ld a, HIGH(enemyShipMetasprite)
    ld [wMetaspriteAddress+1], a

    ; Save the x position
    ld a, [wCurrentEnemyX]
    ld [wMetaspriteX], a

    ; Save the y position
    ld a, [wCurrentEnemyY]
    ld [wMetaspriteY], a

    ; Actually call the 'DrawMetasprites function
    call DrawMetasprites

; ANCHOR_END: draw-enemy-metasprites

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    pop hl
    
    jp UpdateEnemies_Loop
; ANCHOR_END: enemies-update-nocollision



; ANCHOR: enemies-spawn
TryToSpawnEnemies::

    ; Increase our spwncounter
    ld a, [wSpawnCounter]
    inc a
    ld [wSpawnCounter], a

    ; Check our spawn acounter
    ; Stop if it's below a given value
    ld a, [wSpawnCounter]
    cp ENEMY_SPAWN_DELAY_MAX
    ret c

    ; Check our next enemy x position variable
    ; Stop if it's non zero
    ld a, [wNextEnemyXPosition]
    cp 0
    ret nz

    ; Make sure we don't have the max amount of enmies
    ld a, [wActiveEnemyCounter]
    cp MAX_ENEMY_COUNT
    ret nc

GetSpawnPosition:

    ; Generate a semi random value
    call rand
    
    ; make sure it's not above 150
    ld a, b
    cp 150
    ret nc

    ; make sure it's not below 24
    ld a, b
    cp 24
    ret c

    ; reset our spawn counter
    xor a
    ld [wSpawnCounter], a
    
    ld a, b
    ld [wNextEnemyXPosition], a


    ret
; ANCHOR_END: enemies-spawn

; ANCHOR: gameplay-background-initialize
INCLUDE "src/main/utils/hardware.inc"
INCLUDE "src/main/utils/macros/text-macros.inc"

SECTION "BackgroundVariables", WRAM0

mBackgroundScroll:: dw

SECTION "GameplayBackgroundSection", ROM0

starFieldMap: INCBIN "src/generated/backgrounds/star-field.tilemap"
starFieldMapEnd:
 
starFieldTileData: INCBIN "src/generated/backgrounds/star-field.2bpp"
starFieldTileDataEnd:

InitializeBackground::

	; Copy the tile data
	ld de, starFieldTileData ; de contains the address where data will be copied from;
	ld hl, $9340 ; hl contains the address where data will be copied to;
	ld bc, starFieldTileDataEnd - starFieldTileData ; bc contains how many bytes we have to copy.
    call CopyDEintoMemoryAtHL

	; Copy the tilemap
	ld de, starFieldMap
	ld hl, $9800
	ld bc, starFieldMapEnd - starFieldMap
    call CopyDEintoMemoryAtHL_With52Offset

	xor a
	ld [mBackgroundScroll], a
	ld [mBackgroundScroll+1], a
	ret
; ANCHOR_END: gameplay-background-initialize

; ANCHOR: gameplay-background-update-start
; This is called during gameplay state on every frame
UpdateBackground::

	; Increase our scaled integer by 5
	; Get our true (non-scaled) value, and save it for later usage in bc
	ld a, [mBackgroundScroll]
	add a, 5
    ld b, a
	ld [mBackgroundScroll], a
	ld a, [mBackgroundScroll+1]
	adc 0
    ld c, a
	ld [mBackgroundScroll+1], a
; ANCHOR_END: gameplay-background-update-start

; ANCHOR: gameplay-background-update-end
    ; Descale our scaled integer 
    ; shift bits to the right 4 spaces
    srl c
    rr b
    srl c
    rr b
    srl c
    rr b
    srl c
    rr b

    ; Use the de-scaled low byte as the backgrounds position
    ld a, b
	ld [rSCY], a
	ret
; ANCHOR_END: gameplay-background-update-end

; ANCHOR: gameplay-data-variables
INCLUDE "src/main/utils/hardware.inc"
INCLUDE "src/main/utils/macros/text-macros.inc"

SECTION "GameplayVariables", WRAM0

wScore:: ds 6
wLives:: db

SECTION "GameplayState", ROM0

wScoreText::  db "score", 255
wLivesText::  db "lives", 255
; ANCHOR_END: gameplay-data-variables

; ANCHOR: init-gameplay-state
InitGameplayState::

	ld a, 3
	ld [wLives], a

	xor a
	ld [wScore], a
	ld [wScore+1], a
	ld [wScore+2], a
	ld [wScore+3], a
	ld [wScore+4], a
	ld [wScore+5], a

	call InitializeBackground
	call InitializePlayer
	call InitializeBullets
	call InitializeEnemies

	; Initiate STAT interrupts
	call InitStatInterrupts

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	; Call Our function that draws text onto background/window tiles
    ld de, $9c00
    ld hl, wScoreText
    call DrawTextTilesLoop

	; Call Our function that draws text onto background/window tiles
    ld de, $9c0d
    ld hl, wLivesText
    call DrawTextTilesLoop
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	call DrawScore
	call DrawLives

	ld a, 0
	ld [rWY], a

	ld a, 7
	ld [rWX], a

	; Turn the LCD on
	ld a, LCDCF_ON  | LCDCF_BGON|LCDCF_OBJON | LCDCF_OBJ16 | LCDCF_WINON | LCDCF_WIN9C00|LCDCF_BG9800
	ld [rLCDC], a

    ret
; ANCHOR_END: init-gameplay-state
	
; ANCHOR: update-gameplay-state-start
UpdateGameplayState::

	; save the keys last frame
	ld a, [wCurKeys]
	ld [wLastKeys], a

	; This is in input.asm
	; It's straight from: https://gbdev.io/gb-asm-tutorial/part2/input.html
	; In their words (paraphrased): reading player input for gameboy is NOT a trivial task
	; So it's best to use some tested code
    call Input
; ANCHOR_END: update-gameplay-state-start

; ANCHOR: update-gameplay-oam
	; from: https://github.com/eievui5/gb-sprobj-lib
	; hen put a call to ResetShadowOAM at the beginning of your main loop.
	call ResetShadowOAM
	call ResetOAMSpriteAddress
; ANCHOR_END: update-gameplay-oam
	
; ANCHOR: update-gameplay-elements
	call UpdatePlayer
	call UpdateEnemies
	call UpdateBullets
	call UpdateBackground
; ANCHOR_END: update-gameplay-elements
	
; ANCHOR: update-gameplay-clear-sprites
	; Clear remaining sprites to avoid lingering rogue sprites
	call ClearRemainingSprites
; ANCHOR_END: update-gameplay-clear-sprites

; ANCHOR: update-gameplay-end-update
	ld a, [wLives]
	cp 250
	jp nc, EndGameplay

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Call our function that performs the code
    call WaitForOneVBlank
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	; from: https://github.com/eievui5/gb-sprobj-lib
	; Finally, run the following code during VBlank:
	ld a, HIGH(wShadowOAM)
	call hOAMDMA

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Call our function that performs the code
    call WaitForOneVBlank
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	jp UpdateGameplayState

EndGameplay:
	
    ld a, 0
    ld [wGameState],a
    jp NextGameState
; ANCHOR_END: update-gameplay-end-update

INCLUDE "src/main/utils/hardware.inc"


SECTION "GameplayHUD", ROM0

; ANCHOR: hud-increase-score
IncreaseScore::

    ; We have 6 digits, start with the right-most digit (the last byte)
    ld c, 0
    ld hl, wScore+5

IncreaseScore_Loop:

    ; Increase the digit 
    ld a, [hl]
    inc a
    ld [hl], a

    ; Stop if it hasn't gone past 0
    cp 9
    ret c

; If it HAS gone past 9
IncreaseScore_Next:

    ; Increase a counter so we can not go out of our scores bounds
    inc c
    ld a, c

    ; Check if we've gone over our scores bounds
    cp 6
    ret z

    ; Reset the current digit to zero
    ; Then go to the previous byte (visually: to the left)
    ld a, 0
    ld [hl], a
    ld [hld], a

    jp IncreaseScore_Loop
; ANCHOR_END: hud-increase-score

    
; ANCHOR: hud-draw-lives
DrawLives::

    ld hl, wLives
    ld de, $9C13 ; The window tilemap starts at $9C00

    ld a, [hl]
    add 10 ; our numeric tiles start at tile 10, so add 10 to each bytes value
    ld [de], a

    ret
; ANCHOR_END: hud-draw-lives

; ANCHOR: hud-draw-score
DrawScore::

    ; Our score has max 6 digits
    ; We'll start with the left-most digit (visually) which is also the first byte
    ld c, 6
    ld hl, wScore
    ld de, $9C06 ; The window tilemap starts at $9C00

DrawScore_Loop:

    ld a, [hli]
    add 10 ; our numeric tiles start at tile 10, so add to 10 to each bytes value
    ld [de], a

    ; Decrease how many numbers we have drawn
    dec c
		
    ; Stop when we've drawn all the numbers
    ret z

    ; Increase which tile we are drawing to
    inc de

    jp DrawScore_Loop
; ANCHOR_END: hud-draw-score


; ANCHOR: interrupts-start
INCLUDE "src/main/utils/hardware.inc"

 SECTION "Interrupts", ROM0

 DisableInterrupts::
	xor a
	ldh [rSTAT], a
	di
	ret

InitStatInterrupts::

    ld a, IEF_STAT
	ldh [rIE], a
	xor a
	ldh [rIF], a
	ei

	; This makes our stat interrupts occur when the current scanline is equal to the rLYC register
	ld a, STATF_LYC
	ldh [rSTAT], a

	; We'll start with the first scanline
	; The first stat interrupt will call the next time rLY = 0
	xor a
	ldh [rLYC], a

    ret
; ANCHOR_END: interrupts-start

; ANCHOR: interrupts-section
; Define a new section and hard-code it to be at $0048.
SECTION "Stat Interrupt", ROM0[$0048]
StatInterrupt:

	push af

	; Check if we are on the first scanline
	ldh a, [rLYC]
	and a
	jp z, LYCEqualsZero

LYCEquals8:

	; Don't call the next stat interrupt until scanline 8
	xor a
	ldh [rLYC], a

	; Turn the LCD on including sprites. But no window
	ld a, LCDCF_ON | LCDCF_BGON | LCDCF_OBJON | LCDCF_OBJ16 | LCDCF_WINOFF | LCDCF_WIN9C00
	ldh [rLCDC], a

	jp EndStatInterrupts

LYCEqualsZero:

	; Don't call the next stat interrupt until scanline 8
	ld a, 8
	ldh [rLYC], a

	; Turn the LCD on including the window. But no sprites
	ld a, LCDCF_ON | LCDCF_BGON | LCDCF_OBJOFF | LCDCF_OBJ16| LCDCF_WINON | LCDCF_WIN9C00
	ldh [rLCDC], a


EndStatInterrupts:

	pop af

	reti;
; ANCHOR_END: interrupts-section

; ANCHOR: init-story-state
INCLUDE "src/main/utils/hardware.inc"
INCLUDE "src/main/utils/macros/text-macros.inc"

SECTION "StoryStateASM", ROM0

InitStoryState::

	; Turn the LCD on
	ld a, LCDCF_ON  | LCDCF_BGON|LCDCF_OBJON | LCDCF_OBJ16
	ld [rLCDC], a

    ret
; ANCHOR_END: init-story-state

; ANCHOR: story-screen-data
Story: 
    .Line1 db "the galatic empire", 255
    .Line2 db "rules the galaxy", 255
    .Line3 db "with an iron", 255
    .Line4 db "fist.", 255
    .Line5 db "the rebel force", 255
    .Line6 db "remain hopeful of", 255
    .Line7 db "freedoms light", 255
	
; ANCHOR_END: story-screen-data
; ANCHOR: story-screen-page1
UpdateStoryState::

    ; Call Our function that typewrites text onto background/window tiles
    ld de, $9821
    ld hl, Story.Line1
    call DrawText_WithTypewriterEffect


    ; Call Our function that typewrites text onto background/window tiles
    ld de, $9861
    ld hl, Story.Line2
    call DrawText_WithTypewriterEffect


    ; Call Our function that typewrites text onto background/window tiles
    ld de, $98A1
    ld hl, Story.Line3
    call DrawText_WithTypewriterEffect


    ; Call Our function that typewrites text onto background/window tiles
    ld de, $98E1
    ld hl, Story.Line4
    call DrawText_WithTypewriterEffect

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Wait for A
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ; Save the passed value into the variable: mWaitKey
    ; The WaitForKeyFunction always checks against this vriable
    ld a, PADF_A
    ld [mWaitKey], a

    call WaitForKeyFunction
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ANCHOR_END: story-screen-page1


    call ClearBackground


; ANCHOR: story-screen-page2
    ; Call Our function that typewrites text onto background/window tiles
    ld de, $9821
    ld hl, Story.Line5
    call DrawText_WithTypewriterEffect


    ; Call Our function that typewrites text onto background/window tiles
    ld de, $9861
    ld hl, Story.Line6
    call DrawText_WithTypewriterEffect


    ; Call Our function that typewrites text onto background/window tiles
    ld de, $98A1
    ld hl, Story.Line7
    call DrawText_WithTypewriterEffect


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Wait for A
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ; Save the passed value into the variable: mWaitKey
    ; The WaitForKeyFunction always checks against this vriable
    ld a, PADF_A
    ld [mWaitKey], a

    call WaitForKeyFunction
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    
; ANCHOR_END: story-screen-page2

; ANCHOR: story-screen-end
    ld a, 2
    ld [wGameState],a
    jp NextGameState
; ANCHOR_END: story-screen-end

; ANCHOR: title-screen-start
INCLUDE "src/main/utils/hardware.inc"
INCLUDE "src/main/utils/macros/text-macros.inc"

SECTION "TitleScreenState", ROM0

PressPlayText::  db "press a to play", 255
 
titleScreenTileData: INCBIN "src/generated/backgrounds/title-screen.2bpp"
titleScreenTileDataEnd:
 
titleScreenTileMap: INCBIN "src/generated/backgrounds/title-screen.tilemap"
titleScreenTileMapEnd:
; ANCHOR_END: title-screen-start
; ANCHOR: title-screen-init
InitTitleScreenState::

	call DrawTitleScreen
	
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Draw the press play text
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	; Call Our function that draws text onto background/window tiles
    ld de, $99C3
    ld hl, PressPlayText
    call DrawTextTilesLoop

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	; Turn the LCD on
	ld a, LCDCF_ON  | LCDCF_BGON|LCDCF_OBJON | LCDCF_OBJ16
	ld [rLCDC], a

    ret
; ANCHOR_END: title-screen-init
	
; ANCHOR: draw-title-screen
DrawTitleScreen::
	
	; Copy the tile data
	ld de, titleScreenTileData ; de contains the address where data will be copied from;
	ld hl, $9340 ; hl contains the address where data will be copied to;
	ld bc, titleScreenTileDataEnd - titleScreenTileData ; bc contains how many bytes we have to copy.
	call CopyDEintoMemoryAtHL
	
	; Copy the tilemap
	ld de, titleScreenTileMap
	ld hl, $9800
	ld bc, titleScreenTileMapEnd - titleScreenTileMap
	jp CopyDEintoMemoryAtHL_With52Offset

; ANCHOR_END: draw-title-screen
	
; ANCHOR: update-title-screen
UpdateTitleScreenState::

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Wait for A
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ; Save the passed value into the variable: mWaitKey
    ; The WaitForKeyFunction always checks against this vriable
    ld a, PADF_A
    ld [mWaitKey], a

    call WaitForKeyFunction

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ld a, 1
    ld [wGameState],a
    jp NextGameState
; ANCHOR_END: update-title-screen

; ANCHOR: charmap
; The character map for the text-font 
CHARMAP " ", 0
CHARMAP ".", 24
CHARMAP "-", 25
CHARMAP "a", 26
CHARMAP "b", 27
CHARMAP "c", 28
CHARMAP "d", 29
CHARMAP "e", 30
CHARMAP "f", 31
CHARMAP "g", 32
CHARMAP "h", 33
CHARMAP "i", 34
CHARMAP "j", 35
CHARMAP "k", 36
CHARMAP "l", 37
CHARMAP "m", 38
CHARMAP "n", 39
CHARMAP "o", 40
CHARMAP "p", 41
CHARMAP "q", 42
CHARMAP "r", 43
CHARMAP "s", 44
CHARMAP "t", 45
CHARMAP "u", 46
CHARMAP "v", 47
CHARMAP "w", 48
CHARMAP "x", 49
CHARMAP "y", 50
CHARMAP "z", 51
; ANCHOR_END: charmap

; ANCHOR: background-utils
include "src/main/utils/hardware.inc"

SECTION "Background", ROM0

ClearBackground::

	; Turn the LCD off
	xor a
	ld [rLCDC], a

	ld bc, 1024
	ld hl, $9800

ClearBackgroundLoop:

	xor a
	ld [hli], a

	
	dec bc
	ld a, b
	or c

	jp nz, ClearBackgroundLoop


	; Turn the LCD on
	ld a, LCDCF_ON  | LCDCF_BGON|LCDCF_OBJON | LCDCF_OBJ16
	ld [rLCDC], a


	ret
; ANCHOR_END: background-utils

; ANCHOR: collision-utils
include "src/main/utils/constants.inc"
include "src/main/utils/hardware.inc"

SECTION "CollisionUtilsVariables", WRAM0

wResult::       db
wSize::         db
wObject1Value:: db
wObject2Value:: db

SECTION "CollisionUtils", ROM0

CheckObjectPositionDifference::

    ; at this point in time; e = enemy.y, b =bullet.y

    ld a, [wObject1Value]
    ld e, a
    ld a, [wObject2Value]
    ld b, a

    ld a, [wSize]
    ld d, a

    ; subtract  bullet.y, (aka b) - (enemy.y+8, aka e)
    ; carry means e<b, means enemy.bottom is visually above bullet.y (no collision)

    ld a, e
    add d
    cp b

    ;  carry means  no collision
    jp c, CheckObjectPositionDifference_Failure

    ; subtract  enemy.y-8 (aka e) - bullet.y (aka b)
    ; no carry means e>b, means enemy.top is visually below bullet.y (no collision)
    ld a, e
    sub d
    cp b

    ; no carry means no collision
    jp nc, CheckObjectPositionDifference_Failure

    ld a, 1
    ld [wResult], a
    ret

    
CheckObjectPositionDifference_Failure:

    ld a,0
    ld [wResult], a
    ret;

; ANCHOR_END: collision-utils

include "src/main/utils/hardware.inc"

DEF MAX_ENEMY_COUNT EQU 10
DEF MAX_BULLET_COUNT EQU 5

DEF ENEMY_SPAWN_DELAY_MAX EQU 70

; from https://rgbds.gbdev.io/docs/v0.6.1/rgbasm.5#EXPRESSIONS
; The RS group of commands is a handy way of defining structure offsets:
RSRESET
DEF metasprite_y                RB   1
DEF metasprite_x                RB   1
DEF metasprite_tile             RB   1
DEF metasprite_flag             RB   1
DEF METASPRITE_BYTES_COUNT      RB   0

; from https://rgbds.gbdev.io/docs/v0.6.1/rgbasm.5#EXPRESSIONS
; The RS group of commands is a handy way of defining structure offsets:
RSRESET
DEF enemy_activeByte            RB   1
DEF enemy_xByte                 RB   1
DEF enemy_yLowByte              RB   1
DEF enemy_yHighByte             RB   1
DEF enemy_speedByte             RB   1
DEF enemy_healthByte            RB   1
DEF PER_ENEMY_BYTES_COUNT       RB   0

; ANCHOR: bullet-offset-constants
; from https://rgbds.gbdev.io/docs/v0.6.1/rgbasm.5#EXPRESSIONS
; The RS group of commands is a handy way of defining structure offsets:
RSRESET
DEF bullet_activeByte            RB   1
DEF bullet_xByte                 RB   1
DEF bullet_yLowByte              RB   1
DEF bullet_yHighByte             RB   1
DEF PER_BULLET_BYTES_COUNT       RB   0
; ANCHOR_END: bullet-offset-constants

; ANCHOR: sprite-vram-constants
RSRESET
DEF spriteTilesStart            RB _VRAM
DEF PLAYER_TILES_START          RB 4*16
DEF ENEMY_TILES_START           RB 4*16
DEF BULLET_TILES_START          RB 0
; ANCHOR_END: sprite-vram-constants


DEF ENEMY_MOVE_SPEED EQU 11
DEF BULLET_MOVE_SPEED EQU 20

DEF PLAYER_MOVE_SPEED EQU 15
DEF PADDLE_Y_POSITION EQU 136

; ANCHOR: input-utils
SECTION "InputUtilsVariables", WRAM0

mWaitKey:: db

SECTION "InputUtils", ROM0

WaitForKeyFunction::

    ; Save our original value
    push bc

	
WaitForKeyFunction_Loop:

	; save the keys last frame
	ld a, [wCurKeys]
	ld [wLastKeys], a
    
	; This is in input.asm
	; It's straight from: https://gbdev.io/gb-asm-tutorial/part2/input.html
	; In their words (paraphrased): reading player input for gameboy is NOT a trivial task
	; So it's best to use some tested code
    call Input

    
	ld a, [mWaitKey]
    ld b, a
	ld a, [wCurKeys]
    and b
    jp z, WaitForKeyFunction_NotPressed
    
	ld a, [wLastKeys]
    and b
    jp nz, WaitForKeyFunction_NotPressed

	; restore our original value
	pop bc

    ret


WaitForKeyFunction_NotPressed:

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Wait a small amount of time
    ; Save our count in this variable
    ld a, 1
    ld [wVBlankCount], a

    ; Call our function that performs the code
    call WaitForVBlankFunction
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    jp WaitForKeyFunction_Loop
; ANCHOR_END: input-utils


SECTION "MathVariables", WRAM0
randstate:: ds 4

SECTION "Math", ROM0


;; From: https://github.com/pinobatch/libbet/blob/master/src/rand.z80#L34-L54
; Generates a pseudorandom 16-bit integer in BC
; using the LCG formula from cc65 rand():
; x[i + 1] = x[i] * 0x01010101 + 0xB3B3B3B3
; @return A=B=state bits 31-24 (which have the best entropy),
; C=state bits 23-16, HL trashed
rand::
  ; Add 0xB3 then multiply by 0x01010101
  ld hl, randstate
  ld a, [hl]
  add $B3
  ld [hl+], a
  adc [hl]
  ld [hl+], a
  adc [hl]
  ld [hl+], a
  ld c, a
  adc [hl]
  ld [hl], a
  ld b, a
  ret

; ANCHOR: memory-utils
SECTION "MemoryUtilsSection", ROM0

CopyDEintoMemoryAtHL::
	ld a, [de]
	ld [hli], a
	inc de
	dec bc
	ld a, b
	or c
	jp nz, CopyDEintoMemoryAtHL ; Jump to CopyTiles if the last operation had a non zero result.
	ret

CopyDEintoMemoryAtHL_With52Offset::
	ld a, [de]
    add a, 52 
	ld [hli], a
	inc de
	dec bc
	ld a, b
	or c
	jp nz, CopyDEintoMemoryAtHL_With52Offset ; Jump to COpyTiles, if the z flag is not set. (the last operation had a non zero result)
	ret
; ANCHOR_END: memory-utils


include "src/main/utils/constants.inc"
SECTION "MetaSpriteVariables", WRAM0

wMetaspriteAddress:: dw
wMetaspriteX:: db
wMetaspriteY::db

SECTION "MetaSprites", ROM0

DrawMetasprites::


    ; get the metasprite address
    ld a, [wMetaspriteAddress+0]
    ld l, a
    ld a, [wMetaspriteAddress+1]
    ld h, a

    ; Get the y position
    ld a, [hli]
    ld b, a

    ; stop if the y position is 128 
    ld a, b
    cp 128
    ret z

    ld a, [wMetaspriteY]
    add b
    ld [wMetaspriteY], a

    ; Get the x position
    ld a, [hli]
    ld c, a

    ld a, [wMetaspriteX]
    add c
    ld [wMetaspriteX], a

    ; Get the tile position
    ld a, [hli]
    ld d, a

    ; Get the flag position
    ld a, [hli]
    ld e, a
    

    ; Get our offset address in hl
	ld a,[wLastOAMAddress+0]
    ld l, a
	ld a, HIGH(wShadowOAM)
    ld h, a

    ld a, [wMetaspriteY]
    ld [hli], a

    ld a, [wMetaspriteX]
    ld [hli], a

    ld a, d
    ld [hli], a

    ld a, e
    ld [hli], a

    call NextOAMSprite

     ; increase the wMetaspriteAddress
    ld a, [wMetaspriteAddress]
    add a, METASPRITE_BYTES_COUNT
    ld [wMetaspriteAddress], a
    ld a, [wMetaspriteAddress+1]
    adc 0
    ld [wMetaspriteAddress+1], a


    jp DrawMetasprites


include "src/main/utils/hardware.inc"

SECTION "SpriteVariables", WRAM0

wLastOAMAddress:: dw
wSpritesUsed:: db
wHelperValue::db

SECTION "Sprites", ROM0

ClearAllSprites::
	 
	; Start clearing oam
	xor a
    ld b, OAM_COUNT*sizeof_OAM_ATTRS ; 40 sprites times 4 bytes per sprite
    ld hl, wShadowOAM ; The start of our oam sprites in RAM

ClearOamLoop::
    ld [hli], a
    dec b
    jp nz, ClearOamLoop
    xor a
    ld [wSpritesUsed], a
    
    
	; from: https://github.com/eievui5/gb-sprobj-lib
	; Finally, run the following code during VBlank:
	ld a, HIGH(wShadowOAM)
	jp hOAMDMA

ClearRemainingSprites::

ClearRemainingSprites_Loop::

    ;Get our offset address in hl
	ld a,[wLastOAMAddress]
    ld l, a
	ld a, HIGH(wShadowOAM)
    ld h, a

    ld a, l
    cp 160
    ret nc

    ; Set the y and x to be 0
    xor a
    ld [hli], a
    ld [hld], a

    ; Move up 4 bytes
    ld a, l
    add 4
    ld l, a

    call NextOAMSprite


    jp ClearRemainingSprites_Loop

; ANCHOR: reset-oam-sprite-address
ResetOAMSpriteAddress::
    
    xor a
    ld [wSpritesUsed], a

	ld a, LOW(wShadowOAM)
	ld [wLastOAMAddress], a
	ld a, HIGH(wShadowOAM)
	ld [wLastOAMAddress+1], a

    ret
; ANCHOR_END: reset-oam-sprite-address

; ANCHOR: next-oam-sprite
NextOAMSprite::

    ld a, [wSpritesUsed]
    inc a
    ld [wSpritesUsed], a

	ld a,[wLastOAMAddress]
    add sizeof_OAM_ATTRS
	ld [wLastOAMAddress], a
	ld a, HIGH(wShadowOAM)
	ld [wLastOAMAddress+1], a


    ret
; ANCHOR_END: next-oam-sprite


SECTION "Text", ROM0

textFontTileData: INCBIN "src/generated/backgrounds/text-font.2bpp"
textFontTileDataEnd:
; ANCHOR: load-text-font

LoadTextFontIntoVRAM::
	; Copy the tile data
	ld de, textFontTileData ; de contains the address where data will be copied from;
	ld hl, $9000 ; hl contains the address where data will be copied to;
	ld bc, textFontTileDataEnd - textFontTileData ; bc contains how many bytes we have to copy.
    jp CopyDEintoMemoryAtHL
    
; ANCHOR_END: load-text-font


; ANCHOR: draw-text-tiles
DrawTextTilesLoop::

    ; Check for the end of string character 255
    ld a, [hl]
    cp 255
    ret z

    ; Write the current character (in hl) to the address
    ; on the tilemap (in de)
    ld a, [hl]
    ld [de], a

    inc hl
    inc de

    ; move to the next character and next background tile
    jp DrawTextTilesLoop
; ANCHOR_END: draw-text-tiles

; ANCHOR: typewriter-effect
DrawText_WithTypewriterEffect::

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Wait a small amount of time
    ; Save our count in this variable
    ld a, 3
    ld [wVBlankCount], a

    ; Call our function that performs the code
    call WaitForVBlankFunction
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    
    ; Check for the end of string character 255
    ld a, [hl]
    cp 255
    ret z

    ; Write the current character (in hl) to the address
    ; on the tilemap (in de)
    ld a, [hl]
    ld [de], a

    ; move to the next character and next background tile
    inc hl
    inc de

    jp DrawText_WithTypewriterEffect
; ANCHOR_END: typewriter-effect

; ANCHOR: vblank-utils
INCLUDE "src/main/utils/hardware.inc"

SECTION "VBlankVariables", WRAM0

wVBlankCount:: db 

SECTION "VBlankFunctions", ROM0

WaitForOneVBlank::

    ; Wait a small amount of time
    ; Save our count in this variable
    ld a, 1
    ld [wVBlankCount], a

WaitForVBlankFunction::

WaitForVBlankFunction_Loop::

	ld a, [rLY] ; Copy the vertical line to a
	cp 144 ; Check if the vertical line (in a) is 0
	jp c, WaitForVBlankFunction_Loop ; A conditional jump. The condition is that 'c' is set, the last operation overflowed

    ld a, [wVBlankCount]
    sub 1
    ld [wVBlankCount], a
    ret z

WaitForVBlankFunction_Loop2::

	ld a, [rLY] ; Copy the vertical line to a
	cp 144 ; Check if the vertical line (in a) is 0
	jp nc, WaitForVBlankFunction_Loop2 ; A conditional jump. The condition is that 'c' is set, the last operation overflowed

    jp WaitForVBlankFunction_Loop

; ANCHOR_END: vblank-utils
