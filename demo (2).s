; Authors Jasey J. Acevedo Saldaña, Derel W. Rivera Guzman
.segment "HEADER"
  ; .byte "NES", $1A      ; iNES header identifier
  .byte $4E, $45, $53, $1A
  .byte 2               ; 2x 16KB PRG code
  .byte 1               ; 1x  8KB CHR data
  .byte $01, $00        ; mapper 0, vertical mirroring

.segment "VECTORS"
  ;; When an NMI happens (once per frame if enabled) the label nmi:
  .addr nmi
  ;; When the processor first turns on or is reset, it will jump to the label reset:
  .addr reset
  ;; External interrupt IRQ (unused)
  .addr 0

; "nes" linker config requires a STARTUP section, even if it's empty
.segment "STARTUP"

; Main code segment for the program
.segment "CODE"

reset:
  sei		; disable IRQs
  cld		; disable decimal mode
  ldx #$40
  stx $4017	; disable APU frame IRQ
  ldx #$ff 	; Set up stack
  txs		;  .
  inx		; now X = 0
  stx $2000	; disable NMI
  stx $2001 	; disable rendering
  stx $4010 	; disable DMC IRQs

;; first wait for vblank to make sure PPU is ready
vblankwait1:
  bit $2002
  bpl vblankwait1

clear_memory:
  lda #$00
  sta $0000, x
  sta $0100, x
  sta $0200, x
  sta $0300, x
  sta $0400, x
  sta $0500, x
  sta $0600, x
  sta $0700, x
  inx
  bne clear_memory

;; second wait for vblank, PPU is ready after this
vblankwait2:
  bit $2002
  bpl vblankwait2

constants:
  ticks = $0099 

  groundLevel = $0098

  renderCount = $0000

  controller1 = $0005
  controller2 = $0006

  animation_count = $0018
  animation_count2 = $0028
  gravity = $0019
  gravity2 = $0029

  button_A = %10000000
  button_B = %01000000
  button_Select = %00100000
  button_Up = %00001000
  button_down = %00000100
  button_left = %00000010
  button_right = %00000001

  player_facing = %00000001
  player_moving = %00000010
  player_jumping = %00000100
  player_Shooting = %00001000
  player_hit = %00010000
  player_dead = %00100000
  player_won = %01000000

  player1_x = $0010
  player1_y = $0011
  player1_State = $0012
  player1_pallete = $0013
  player1_tile = $0014
  player1_tile2 = $0015
  player1_tile3 = $0016
  player1_tile4 = $0017
  player1_health = $001a

  player2_x = $0020
  player2_y = $0021
  player2_State = $0022
  player2_pallete = $0023
  player2_tile = $0024
  player2_tile2 = $0025
  player2_tile3 = $0026
  player2_tile4 = $0027
  player2_health = $002a


main:
load_palettes:
  lda $2002
  lda #$3f
  sta $2006
  lda #$00
  sta $2006
  ldx #$00
@loop:
  lda palettes, x
  sta $2007
  inx
  cpx #$20
  bne @loop
  
LoadBackground:
  LDA $2002
  LDA #$20
  STA $2006
  LDA #$00
  STA $2006
  ldx #$00
  
Loop:	
  lda background, x 	; Load the hello message into SPR-RAM
  sta $2007
  inx
  cpx #$00 
  bne Loop
  ldx $00
secondloop:
  lda secondbackground, x	; Load the hello message into SPR-RAM
  sta $2007
  inx
  cpx #$00
  bne secondloop   
  ldx $00
thirdloop:
  lda thirdbackground, x	; Load the hello message into SPR-RAM
  sta $2007
  inx
  cpx #$00
  bne thirdloop 
  ldx $00
fourthloop:
  lda fourthbackground, x	; Load the hello message into SPR-RAM
  sta $2007
  inx
  cpx #$00
  bne fourthloop

enable_rendering:
  lda #%10001000	; Enable NMI
  sta $2000
  lda #%00011110	; Enable Sprites
  sta $2001

initializeCharacter: 
  lda #$bd
  sta player1_y
  sta groundLevel
  lda #$40
  sta player1_x
  lda #$40
  sta player1_pallete
  lda #$01
  sta player1_State
  sta player1_tile
  lda #$02
  sta player1_tile2
  lda #$11
  sta player1_tile3
  lda #$12
  sta player1_tile4
  lda #$03
  sta player1_health
  sta player2_health

  lda #$bd
  sta player2_y
  lda #$d0
  sta player2_x
  lda #$01
  sta player2_pallete
  lda #$00
  sta player2_State
  lda #$01
  sta player2_tile
  lda #$02
  sta player2_tile2
  sta player2_health
  lda #$11
  sta player2_tile3
  lda #$12
  sta player2_tile4

forever:
  jmp forever

nmi:
  ldx #$00 	; Set SPR-RAM address to 0
  stx $2003
  
  jsr Tick ;timer ticks every frame, the counts reset to 0 once it reaches 20

  jsr readcontroller1 ;reads both controllers

  ldx #$00
  jsr checkGravity ;checks if the player 1 is in the air and applies gravity to them
  jsr checkDead ;check if player 1 has died
  ldx #$10
  jsr checkGravity ;checks if the player 2 is in the air and applies gravity to them
  jsr checkDead ;checks if player 2 has died
  
  lda player1_State
  and #player_dead
  cmp #player_dead
  beq playerdead
  lda player2_State
  and #player_dead
  cmp #player_dead
  beq playerdead

  jsr manageinput1  ;decifers controller 1 inputs
  jsr manageinput2  ;decifers controller 2 inputs

  playerdead:

  ldx #$00
  jsr renderCharacter ;renders player 1
  ldx #$10
  jsr renderCharacter ;renders player 2

  lda ticks ;decides when the animation changes to its next stage
  cmp #00
  bne skip

  ldx #$00
  jsr animation_handler ;changes the animation of player 1 depending on the player state
  ldx #$10
  jsr animation_handler ;changes the animation of player 2 depending on the player state
  skip:  

  rti  

Tick:
  ldx ticks
  inx
  cpx #15
  bne count
  ldx #$00
  count:
  stx ticks
  rts

readcontroller1:
  lda #1
  sta controller1

  sta $4016
  lda #0
  sta $4016

  read_loop:
    lda $4017
    lsr a
    rol controller2

    lda $4016
    lsr a
    rol controller1
    bcc read_loop

rts


manageinput1:
  lda player1_State
  and #player_jumping
  cmp #player_jumping
  beq notA

  lda controller1
  cmp #$00
  beq noinput

  lda controller1
  and #button_right
  cmp #button_right
  bne notRight
  
  
  lda #3
  sta player1_State

  lda player1_pallete
  ora #$40
  sta player1_pallete
  

  notRight:
  lda controller1
  and #button_left
  cmp #button_left
  bne notLeft

  lda #2
  sta player1_State
  lda player1_pallete
  and #1
  sta player1_pallete


  notLeft:

  lda controller1
  and #button_A
  cmp #button_A
  bne notA

  lda player1_State
  and #3
  ora #4
  sta player1_State

  lda #23
  sta gravity
  dec player1_y

  notA:

  lda controller1
  and #button_Select
  cmp #button_Select
  bne notSelect

  lda player1_State
  ora #player_hit
  sta player1_State


  notSelect:

  lda controller1
  and #button_B
  cmp #button_B
  bne notB

  lda player1_State
  ora #8
  sta player1_State

  notB:

  ldx #$00
  jsr updateX

  rts
  noinput:
  lda player1_State
  and #1
  sta player1_State

rts

manageinput2:
  lda player2_State
  and #player_jumping
  cmp #player_jumping
  beq notA2

  lda controller2
  cmp #$00
  beq noinput2

  lda controller2
  and #button_right
  cmp #button_right
  bne notRight2
  
  lda player2_State
  ora #3
  sta player2_State

  lda player2_pallete
  ora #$40
  sta player2_pallete

  notRight2:
  lda controller2
  and #button_left
  cmp #button_left
  bne notLeft2

  lda #2
  sta player2_State
  lda player2_pallete
  and #1
  sta player2_pallete

  notLeft2:

  lda controller2
  and #button_A
  cmp #button_A
  bne notA2

  lda player2_State
  and #3
  ora #4
  sta player2_State

  lda #23
  sta gravity2
  dec player2_y 

  notA2:

  lda controller2
  and #button_Select
  cmp #button_Select
  bne notSelect2

  lda player2_State
  ora #player_hit
  sta player2_State

  
  notSelect2:

  lda controller2
  and #button_B
  cmp #button_B
  bne notB2

  lda player2_State
  ora #8
  sta player2_State

  notB2:

  ldx #$10
  jsr updateX

  rts
  noinput2:
  lda player2_State
  and #1
  sta player2_State

rts

animation_handler:
  lda player1_State, x
  and #player_hit
  cmp #player_hit
  bne nothit

  jsr Hit

  rts
  nothit:

  lda player1_State, x
  and #player_dead
  cmp #player_dead
  bne notdead

  jsr Dead

  rts
  notdead:
  
  lda player1_State, x
  and #6
  cmp #2
  bmi idle_update
  beq walking_update

jumping:
  lda #$08
  sta player1_tile, x
  lda #$07
  sta player1_tile2, x
  lda #$18
  sta player1_tile3, x
  lda #$17
  sta player1_tile4, x

  lda player1_State, x
  and #8
  cmp #8
  bne shoot

  lda #$03
  sta player1_tile, x
  lda #$16
  sta player1_tile3, x

  shoot:

  rts
idle_update:
  jsr tickAnimation
  lda animation_count, x
  cmp #0
  bne idle2nd_frame

  lda #$01
  sta player1_tile, x
  lda #$02
  sta player1_tile2, x
  lda #$11
  sta player1_tile3, x
  lda #$12
  sta player1_tile4, x

  lda player1_State, x
  and #8
  cmp #8
  bne shoot

  lda #$05
  sta player1_tile3, x

  rts

  idle2nd_frame:
  lda #$03
  sta player1_tile, x
  lda #$04
  sta player1_tile2, x
  lda #$13
  sta player1_tile3, x
  lda #$14
  sta player1_tile4, x

  lda player1_State, x
  and #8
  cmp #8
  bne shoot

  lda #$05
  sta player1_tile3, x

rts

walking_update:
  jsr tickAnimation
  lda animation_count, x
  cmp #0
  bne walking2nd_frame

  lda #$1b
  sta player1_tile, x
  lda #$04
  sta player1_tile2, x
  lda #$0d
  sta player1_tile3, x
  lda #$0c
  sta player1_tile4, x

  lda player1_State, x
  and #8
  cmp #8
  bne shoot

  lda #$1c
  sta player1_tile3, x

  rts

  walking2nd_frame:
  lda #$09
  sta player1_tile, x
  lda #$02
  sta player1_tile2, x
  lda #$0a
  sta player1_tile3, x
  lda #$1a
  sta player1_tile4, x

  lda player1_State, x
  and #8
  cmp #8
  bne @shoot

  lda #$19
  sta player1_tile3, x
  @shoot:

rts

Dead:
  lda #$21
  sta player1_tile, x
  lda #$22
  sta player1_tile2, x
  lda #$31
  sta player1_tile3, x
  lda #$32
  sta player1_tile4, x

  lda player1_State, x
  and #%00100001
  sta player1_State, x

  rts

Hit:
  lda #$00
  sta player1_tile, x
  lda #$00
  sta player1_tile2, x
  lda #$00
  sta player1_tile3, x
  lda #$00
  sta player1_tile4, x

  lda player1_State, x
  and #%11101111
  sta player1_State, x

  dec player1_health, x

  rts

tickAnimation:
  lda animation_count, x
  cmp #0
  bne decrease
  inc animation_count, x
  jmp increase
  decrease:
  dec animation_count, x
  increase:
  rts

renderCharacter:
  
  lda player1_State, x
  and #1
  cmp #0
  bne rightrender

  lda player1_y, x
  sta $2004
  lda player1_tile, x
  sta $2004
  lda player1_pallete, x
  sta $2004
  lda player1_x, x
  sta $2004

  lda player1_y, x
  sta $2004
  lda player1_tile2, x
  sta $2004
  lda player1_pallete, x
  sta $2004
  lda player1_x, x
  adc #7
  sta $2004

  lda player1_y, x
  adc #8
  sta $2004
  lda player1_tile3, x
  sta $2004
  lda player1_pallete, x
  sta $2004
  lda player1_x, x
  sta $2004

  lda player1_y, x
  adc #8
  sta $2004
  lda player1_tile4, x
  sta $2004
  lda player1_pallete, x
  sta $2004
  lda player1_x, x
  adc #8
  sta $2004
  
rts

  rightrender:
  lda player1_y, x
  sta $2004
  lda player1_tile2, x
  sta $2004
  lda player1_pallete, x
  sta $2004
  lda player1_x, x
  sta $2004

  lda player1_y, x
  sta $2004
  lda player1_tile, x
  sta $2004
  lda player1_pallete, x
  sta $2004
  lda player1_x, x
  adc #7
  sta $2004

  lda player1_y, x
  adc #8
  sta $2004
  lda player1_tile4, x
  sta $2004
  lda player1_pallete, x
  sta $2004
  lda player1_x, x
  sta $2004

  lda player1_y, x
  adc #8
  sta $2004
  lda player1_tile3, x
  sta $2004
  lda player1_pallete, x
  sta $2004
  lda player1_x, x
  adc #8
  sta $2004

rts

checkGravity:
  lda player1_State, x
  ora #4
  sta player1_State, x

  lda player1_x, x
  cmp #$60
  bmi notPlatform
  cmp #$a8
  bpl notPlatform

  lda player1_y, x
  cmp #$97
  beq grounded

  notPlatform:
  lda player1_y, x
  cmp groundLevel
  beq grounded

  jsr updateY

  rts

  grounded:
  
  lda player1_State, x
  and #%11111011
  sta player1_State, x

  rts
updateX: ;function checks if the player is moving if he is it moves the player in the direction they are facing
  
  lda player1_State, x ;checks the player
  and #player_moving
  cmp #player_moving
  bne notmoving

  lda player1_State, x
  and #player_facing
  cmp #0
  bne right
  
  lda player1_x, x
  cmp #$00
  beq lskip
  dec player1_x, x
  lskip:
  rts

  right:
  lda player1_x, x
  cmp #$f4
  beq rskip
  inc player1_x, x
  rskip:
  rts

  notmoving:
  rts
updateY:
  ;same as x
  lda gravity, x
  cmp #0
  beq falling

  dec gravity, x
  dec player1_y, x
  dec player1_y, x

  rts
  falling:

  inc player1_y, x

  rts

  checkDead:
    lda player1_health, x
    cmp #0
    bne alive

    lda player1_State, x
    ora #player_dead
    sta player1_State, x

    alive:
    rts

palettes:
  ; Background Palette
  .byte $01, $19, $3d, $0f
  .byte $01, $17, $3d, $16
  .byte $01, $06, $16, $0f
  .byte $01, $3d, $14, $38

  ; Sprite Palette
  .byte $01, $20, $2c, $05
  .byte $01, $28, $13, $16
  .byte $01, $00, $00, $00
  .byte $01, $00, $00, $00

  background:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$00,$0a,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$09,$02,$00,$00,$00,$00,$00,$0d,$09,$02,$02
	  .byte $0d,$09,$02,$02,$0d,$09,$00,$00,$00,$00,$12,$13,$00,$00,$00,$00
	  .byte $00,$00,$00,$09,$09,$02,$02,$08,$00,$00,$00,$00,$02,$02,$02,$02
	  .byte $02,$02,$02,$02,$02,$02,$00,$00,$00,$21,$22,$23,$24,$00,$00,$00
	  .byte $00,$00,$00,$0e,$02,$10,$02,$02,$08,$0a,$00,$00,$02,$02,$02,$02
	  .byte $02,$02,$02,$02,$02,$0c,$00,$00,$30,$31,$32,$33,$34,$00,$00,$00
	  .byte $00,$00,$03,$0b,$02,$02,$02,$02,$02,$0c,$00,$00,$02,$02,$02,$02
	  .byte $02,$02,$02,$02,$02,$00,$00,$00,$00,$41,$42,$43,$00,$00,$00,$00
	  .byte $00,$00,$03,$00,$02,$02,$02,$02,$0c,$00,$00,$00,$0b,$02,$10,$02
	  .byte $02,$10,$02,$02,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$0b,$02,$02,$0c,$00,$00,$00,$00,$00,$02,$02,$02
	  .byte $02,$02,$02,$02,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	secondbackground:  
    .byte $00,$00,$00,$00,$00,$02,$02,$00,$00,$00,$00,$00,$00,$02,$02,$02
	  .byte $02,$02,$02,$02,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$02,$02,$00,$00,$00,$00,$00,$00,$02,$02,$02
	  .byte $02,$02,$02,$02,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$03,$00,$00,$00,$02,$02,$00,$00,$00,$00,$00,$00,$02,$02,$02
	  .byte $02,$02,$02,$02,$02,$00,$00,$00,$0a,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$03,$00,$00,$00,$02,$02,$00,$00,$00,$00,$00,$00,$02,$02,$02
	  .byte $02,$02,$02,$02,$02,$00,$00,$09,$02,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$02,$02,$0d,$00,$00,$00,$00,$00,$02,$02,$02
	  .byte $02,$02,$02,$02,$02,$09,$02,$02,$02,$0d,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$03,$03,$02,$02,$02,$00,$00,$00,$00,$00,$02,$02,$02
	  .byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$0d,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$02,$02,$02,$0d,$00,$00,$00,$00,$02,$02,$02
	  .byte $02,$02,$04,$02,$02,$02,$02,$02,$02,$02,$02,$0d,$00,$09,$02,$0d
	  .byte $00,$00,$00,$00,$09,$02,$02,$02,$02,$0d,$00,$00,$00,$02,$02,$02
	  .byte $02,$04,$04,$04,$04,$02,$02,$02,$02,$02,$02,$02,$09,$02,$02,$02
	thirdbackground:  
    .byte $00,$00,$00,$00,$02,$02,$02,$02,$02,$02,$0d,$02,$02,$02,$04,$04
	  .byte $02,$04,$06,$04,$04,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$04
	  .byte $00,$00,$00,$00,$02,$02,$02,$02,$02,$02,$02,$07,$02,$06,$04,$04
	  .byte $02,$04,$06,$04,$04,$04,$02,$02,$02,$02,$02,$02,$02,$02,$02,$04
	  .byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$05,$07,$04,$06,$06,$04
	  .byte $04,$04,$06,$05,$04,$04,$02,$02,$02,$04,$02,$02,$02,$02,$04,$04
	  .byte $02,$02,$02,$02,$02,$02,$04,$02,$04,$02,$04,$07,$04,$04,$07,$07
	  .byte $05,$05,$06,$05,$07,$05,$02,$02,$02,$04,$02,$02,$04,$02,$05,$04
	  .byte $02,$02,$04,$02,$02,$02,$04,$04,$04,$02,$05,$07,$04,$04,$04,$05
	  .byte $04,$04,$04,$04,$04,$04,$04,$04,$02,$04,$02,$02,$06,$06,$05,$04
	  .byte $02,$02,$06,$02,$02,$02,$07,$04,$04,$04,$04,$07,$05,$26,$28,$28
	  .byte $28,$28,$28,$28,$28,$29,$04,$04,$04,$04,$04,$06,$06,$05,$06,$04
	  .byte $04,$04,$06,$04,$04,$02,$06,$04,$04,$04,$05,$06,$05,$39,$39,$37
	  .byte $37,$38,$39,$39,$39,$39,$05,$07,$05,$04,$04,$06,$05,$04,$06,$05
	  .byte $05,$06,$06,$05,$05,$05,$06,$06,$05,$05,$06,$06,$05,$04,$04,$05
	  .byte $05,$05,$04,$04,$04,$04,$07,$07,$05,$05,$06,$06,$04,$05,$06,$05
	fourthbackground:  
    .byte $04,$06,$07,$07,$04,$04,$05,$06,$06,$04,$06,$04,$04,$04,$04,$07
	  .byte $04,$04,$04,$04,$04,$04,$04,$04,$05,$05,$06,$05,$05,$05,$06,$06
	  .byte $04,$04,$04,$07,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
	  .byte $04,$04,$04,$04,$04,$04,$04,$04,$04,$05,$05,$04,$04,$04,$04,$06
	  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	  .byte $00,$00,$00,$00,$00,$00,$f0,$34,$00,$00,$00,$00,$00,$00,$0f,$03
	  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $20,$0a,$00,$00,$00,$00,$00,$00,$02,$00,$00,$00,$00,$00,$00,$00
	  .byte $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a

; Character memory
.segment "CHARS"
.incbin "background.chr"
.incbin "wizard.chr"
