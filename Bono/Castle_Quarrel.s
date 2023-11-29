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
  ticks = $99 
  nameTable = $97

  

  renderCount = $00
  gameState = $01
  groundLevel = $02

  controller1 = $05
  controller2 = $06

  animation_count = $18
  animation_count2 = $28
  gravity = $19
  gravity2 = $29

  button_A = %10000000
  button_B = %01000000
  button_Select = %00100000
  button_Start = %00010000
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

  player1_x = $10
  player1_y = $11
  player1_State = $12
  player1_pallete = $13
  player1_tile = $14
  player1_tile2 = $15
  player1_tile3 = $16
  player1_tile4 = $17
  player1_health = $1a
  shot_timer = $1b
  health_x = $1c
  health_y = $1d

  shotState = $30
  shot_x = $31
  shot_y = $32
  shot2_x = $33
  shot2_y = $34
  shot_pallete = $35
  shot2_pallete = $36

  player2_x = $20
  player2_y = $21
  player2_State = $22
  player2_pallete = $23
  player2_tile = $24
  player2_tile2 = $25
  player2_tile3 = $26
  player2_tile4 = $27
  player2_health = $2a
  shot2_timer = $2b
  health2_x = $2c
  health2_y = $2d

  shotState2 = $40
  shot3_x = $41
  shot3_y = $42
  shot4_x = $43
  shot4_y = $44
  shot3_pallete = $45
  shot4_pallete = $46

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

jsr uploadMenu
jsr LoadBackground

enable_rendering:
  lda #%10001000	; Enable NMI
  sta $2000
  lda #%00011110	; Enable Sprites
  sta $2001

initializeCharacter: 
  lda #$bc
  sta player1_y
  sta groundLevel
  lda #$40
  sta player1_x
  lda #$40
  sta player1_pallete
  sta shot_pallete
  sta shot2_pallete
  lda #$01
  sta player1_State
  sta player1_tile
  lda #$02
  sta player1_tile2
  lda #$11
  sta player1_tile3
  lda #$12
  sta player1_tile4
  lda #$05
  sta player1_health
  sta player2_health

  lda #$30
  sta health_y
  sta health2_y
  lda #$08
  sta health_x
  lda #$f7
  sta health2_x

  lda #$bc
  sta player2_y
  lda #$d0
  sta player2_x
  lda #$01
  sta player2_pallete
  sta shot3_pallete
  sta shot4_pallete
  lda #$00
  sta player2_State
  lda #$01
  sta player2_tile
  lda #$02
  sta player2_tile2
  lda #$11
  sta player2_tile3
  lda #$12
  sta player2_tile4
  lda #$00
  sta shot_timer
  sta shot2_timer
  sta shotState
  sta shotState2
  sta shot_x
  sta shot2_x
  sta shot3_x
  sta shot4_x
  sta shot_y
  sta shot2_y
  sta shot3_y
  sta shot4_y

forever:
  jmp forever

nmi:
  
  jsr Tick ;timer ticks every frame, the counts reset to 0 once it reaches 20

  jsr readcontroller ;reads both controllers

  
  lda gameState
  cmp #$00
  beq menu
  bne game


menu:
  lda controller1
  and #button_Start
  cmp #button_Start
  bne notStarted

  lda #%0000000	
  sta $2001

  jsr uploadBackground
  jsr LoadBackground

  lda #%00011110	
  sta $2001
  
  lda #1
  sta gameState

  notStarted:

rti

game:

  ldx #$00
  ldy #$10
  jsr fireballCollision
  jsr checkGravity ;checks if the player 1 is in the air and applies gravity to them
  jsr checkDead ;check if player 1 has died

  ldx #$10
  ldy #$00
  jsr fireballCollision
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

  ldx #$00
  ldy #$00
  jsr manageinput  ;decifers controller 1 inputs

  ldx #$10
  ldy #$01
  jsr manageinput  ;decifers controller 2 inputs

  playerdead:

  ldx #$00
  jsr updateFireball
  jsr renderFireball
  jsr tickShot
  

  ldx #$10
  jsr updateFireball
  jsr renderFireball
  jsr tickShot
  

  ldx #$00
  ldy #$00
  jsr UpdateRender ;renders player 1
  jsr renderHealth

  ldx #$10
  ldy #$10
  jsr UpdateRender ;renders player 2
  jsr renderHealth

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

tickShot:
  lda shot_timer, x
  cmp #0
  beq charged

  dec shot_timer, x

  charged:
  rts

readcontroller:
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


manageinput:
  lda player1_State, x
  and #player_jumping
  cmp #player_jumping
  beq notA

  lda controller1, y
  cmp #$00
  beq noinput

  lda controller1, y
  and #button_right
  cmp #button_right
  bne notRight
  
  
  lda #3
  sta player1_State, x

  lda player1_pallete, x
  ora #$40
  sta player1_pallete, x
  

  notRight:
  lda controller1, y
  and #button_left
  cmp #button_left
  bne notLeft

  lda #2
  sta player1_State, x
  lda player1_pallete, x
  and #1
  sta player1_pallete, x


  notLeft:

  lda controller1, y
  and #button_A
  cmp #button_A
  bne notA

  lda player1_State, x
  and #3
  ora #4
  sta player1_State, x

  lda #23
  sta gravity, x
  

  notA:

  lda controller1, y
  and #button_B
  cmp #button_B
  bne notB

  lda player1_State, x
  ora #8
  sta player1_State, x

  jsr shootFireball

  notB:

  jsr updateX

  rts
  noinput:
  lda player1_State, x
  and #1
  sta player1_State, x

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
  and #player_won
  cmp #player_won
  bne notwon

  jsr won

  rts
  notwon:
  
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

won:
  lda #$25
  sta player1_tile, x
  lda #$26
  sta player1_tile2, x
  lda #$35
  sta player1_tile3, x
  lda #$36
  sta player1_tile4, x

  lda player1_State, x
  and #%01000001
  sta player1_State, x
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

UpdateRender:
  lda #8
  sta renderCount
  lda player1_State, x
  and #1
  cmp #0
  bne rightrender
  
  ;storing drawing offsets in the stack for left render
  lda #8
  pha
  lda #9
  pha
  lda #0
  pha 
  lda #9
  pha 
  lda #8
  pha
  lda #1
  pha
  lda #0
  pha
  pha

  jmp leftrender

  ;storing drawing offsets in the stack for right render
  rightrender:
  lda #0
  pha
  lda #9
  pha 
  lda #8
  pha
  lda #9
  pha 
  lda #0
  pha
  lda #1
  pha
  lda #8
  pha
  lda #0
  pha
  
  leftrender:
  renderLoop:

  pla
  adc player1_y, x
  sta $2004
  lda player1_tile, y
  sta $2004
  lda player1_pallete, x
  sta $2004
  pla
  adc player1_x, x
  sta $2004

  iny
  lsr renderCount
  bcc renderLoop

  rts

checkGravity:
  lda player1_State, x
  ora #4
  sta player1_State, x

  lda gravity, x
  cmp #0 
  bne jumped

  lda player1_x, x
  cmp #$60
  bmi notPlatform
  cmp #$a8
  bpl notPlatform

  lda player1_y, x
  cmp #$96
  beq grounded

  notPlatform:
  lda player1_y, x
  cmp groundLevel
  beq grounded

  jumped:
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
  cmp #$09
  beq lskip
  dec player1_x, x
  lskip:
  rts

  right:
  lda player1_x, x
  cmp #$ee
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

    lda player1_State, y
    ora #player_won
    sta player1_State, y

    alive:
    rts

shootFireball:
  lda shot_timer, x
  cmp #$00
  beq continue

  jmp finish

  continue:
  lda #10
  sta shot_timer, x

  lda shotState, x ;checking which shot has been fired
  cmp #$01
  beq secondshot
  bmi firstshot
  cmp #$02
  beq firstshot

  ;if both shots have been fired no modifications will apply
  rts
  firstshot:
  lda shotState, x
  ora #1
  sta shotState, x

  lda player1_State, x
  and #$01
  cmp #$01
  beq rightside

  lda shot_pallete, x
  and #1
  sta shot_pallete, x

  lda player1_x, x
  sbc #8
  sta shot_x, x
  lda player1_y, x
  adc #4
  sta shot_y, x

  jmp finish
  rightside:

  lda shot_pallete, x
  ora #$40
  sta shot_pallete, x

  lda player1_x, x
  adc #16
  sta shot_x, x
  lda player1_y, x
  adc #4
  sta shot_y, x

  jmp finish
  secondshot:

  lda shotState, x
  ora #2
  sta shotState, x

  lda player1_State, x
  and #$01
  cmp #$01
  beq rightside2

  lda shot2_pallete, x
  and #1
  sta shot2_pallete, x

  lda player1_x, x
  sbc #8
  sta shot2_x, x
  lda player1_y, x
  adc #4
  sta shot2_y, x

  jmp finish
  rightside2:
  
  lda shot2_pallete, x
  ora #$40
  sta shot2_pallete, x

  lda player1_x, x
  adc #16
  sta shot2_x, x
  lda player1_y, x
  adc #4
  sta shot2_y, x
  jmp finish
  
  finish:

  rts

updateFireball:

  lda shotState, x
  and #1
  cmp #1
  bne skipFirst

  lda shot_pallete, x
  and #$40
  cmp #$40
  beq movingRight

  lda shot_x, x
  sbc #3
  bcc resetShot
  sta shot_x, x
  jmp skipFirst
  movingRight:

  lda shot_x, x
  adc #3
  bcs resetShot
  sta shot_x, x
  jmp skipFirst

  resetShot:
  lda shotState, x
  and #2
  sta shotState, x
  lda #0
  sta shot_x, x
  sta shot_y, x

  skipFirst:
  lda shotState, x
  and #2
  cmp #2
  bne skipSecond

  lda shot2_pallete, x
  and #$40
  cmp #$40
  beq movingRight2

  lda shot2_x, x
  sbc #3
  bcc resetShot2
  sta shot2_x, x
  jmp skipSecond
  movingRight2:

  lda shot2_x, x
  adc #3
  bcs resetShot2
  sta shot2_x, x
  jmp skipSecond

  resetShot2:
  lda shotState, x
  and #1
  sta shotState, x
  lda #0
  sta shot2_x, x
  sta shot2_y, x

  skipSecond:

  rts
renderFireball:
  lda shotState, x
  and #1
  cmp #$00
  beq invisible

  lda shot_y, x
  sta $2004
  lda #$1D
  sta $2004
  lda shot_pallete, x
  sta $2004
  lda shot_x, x
  sta $2004

  invisible:

  lda shotState, x
  and #2
  cmp #$00
  beq invisible2

  lda shot2_y, x
  sta $2004
  lda #$1D
  sta $2004
  lda shot2_pallete, x
  sta $2004
  lda shot2_x, x
  sta $2004
  
  invisible2:
  rts

  fireballCollision:
    lda shotState, x
    and #1
    cmp #0
    beq notFired

    lda player1_x, y
    cmp shot_x, x
    bpl notFired
    adc #$10
    cmp shot_x, x
    bmi notFired
    lda player1_y, y
    cmp shot_y, x
    bpl notFired
    adc #$10
    cmp shot_y, x
    bmi notFired

    lda #$00
    sta shot_x, x
    sta shot_y, x
    lda shotState, x
    and #$2
    sta shotState, x
    
    tya
    tax
    jsr Hit

    notFired:
    lda shotState, x
    and #2
    cmp #0
    beq notFired2

    lda player1_x, y
    cmp shot2_x, x
    bpl notFired2
    adc #$10
    cmp shot2_x, x
    bmi notFired2
    lda player1_y, y
    cmp shot2_y, x
    bpl notFired2
    adc #$10
    cmp shot2_y, x
    bmi notFired2

    lda #$00
    sta shot2_x, x
    sta shot2_y, x
    lda shotState, x
    and #$1
    sta shotState, x
    
    tya
    tax
    jsr Hit

    notFired2:

    rts

renderHealth:
  lda #0
  pha
  adc #10
  pha
  adc #10
  pha
  adc #10
  pha
  adc #10
  pha
  adc #10
  ldy #0
  healthRender:
  tya
  cmp player1_health, x
  bpl damage

  pla
  adc health_y, x
  sta $2004
  lda #$23
  sta $2004
  lda player1_pallete, x
  sta $2004
  lda health_x, x
  sta $2004
  jmp healthy

  damage:

  pla
  adc health_y, x
  sta $2004
  lda #$24
  sta $2004
  lda player1_pallete, x
  sta $2004
  lda health_x, x
  sta $2004

  healthy:
  iny
  cpy #5
  bne healthRender

  rts

LoadBackground:

  lda $2002
  LDA #$20
  STA $2006
  LDA #$00
  STA $2006
  ldy #$00
  lda #8
  sta renderCount

  backgroundLoop:	
    lda (nameTable), y 	; Load the hello message into SPR-RAM
    sta $2007
    iny
    cpy #$00 
    bne backgroundLoop
  
    inc nameTable + 1
  
    lsr renderCount
    bcc backgroundLoop

  rts

uploadMenu:
  lda #<menu1
  sta nameTable
  lda #>menu1
  sta nameTable + 1
  rts

uploadBackground:
  lda #<background
  sta nameTable
  lda #>background
  sta nameTable + 1
  rts

palettes:
  ; Background Palette
  .byte $01, $19, $3d, $0f
  .byte $01, $17, $3d, $16
  .byte $01, $06, $16, $3d
  .byte $01, $3d, $14, $38

  ; Sprite Palette
  .byte $01, $20, $2c, $05
  .byte $01, $28, $13, $16
  .byte $01, $00, $00, $00
  .byte $01, $00, $00, $00

  background:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$12,$13,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$21,$22,$23,$24,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$3c,$00,$00,$00,$00,$00,$30,$31,$32,$33,$34,$00,$00,$00
	  .byte $00,$00,$03,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$4a,$3b,$4b,$00,$00,$00,$00,$00,$41,$42,$43,$00,$00,$00,$00
	  .byte $00,$00,$03,$00,$00,$dc,$dd,$e3,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$3d,$4e,$ec,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$de,$e5,$e4,$e0,$df,$00,$58,$00,$00,$00,$00
	  .byte $00,$2e,$02,$3e,$ee,$ed,$00,$00,$00,$00,$00,$00,$00,$00,$00,$cb
    .byte $00,$00,$00,$00,$00,$f4,$e6,$e2,$e1,$00,$d7,$48,$00,$00,$00,$00
	  .byte $00,$02,$02,$02,$eb,$00,$00,$00,$00,$00,$00,$00,$00,$00,$cd,$02
	  .byte $00,$00,$d5,$d6,$f2,$f5,$fc,$fb,$f6,$d8,$4d,$47,$00,$00,$00,$00
	  .byte $2e,$02,$02,$02,$ea,$e7,$00,$00,$00,$00,$00,$00,$bb,$cf,$02,$02
	  .byte $00,$03,$00,$00,$f3,$fe,$fd,$fa,$f7,$00,$02,$02,$00,$00,$00,$00
	  .byte $02,$02,$02,$02,$02,$ef,$00,$00,$00,$00,$00,$bd,$be,$02,$02,$02
	  .byte $00,$03,$00,$00,$00,$f9,$f8,$00,$00,$2e,$02,$02,$1c,$1c,$1c,$1c
	  .byte $02,$02,$02,$02,$02,$0d,$00,$00,$00,$00,$bc,$d3,$02,$02,$02,$02
	  .byte $1b,$1b,$1b,$1b,$09,$f1,$db,$1b,$49,$02,$02,$02,$2c,$2c,$2c,$2c
	  .byte $02,$02,$02,$02,$02,$02,$0d,$1b,$1b,$1b,$ca,$02,$02,$02,$02,$02
	  .byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	  .byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	  .byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	  .byte $02,$02,$04,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	  .byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	  .byte $02,$04,$04,$04,$04,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
    .byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$04,$04
	  .byte $02,$04,$06,$04,$04,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$04
	  .byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$07,$02,$06,$04,$04
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
	  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a
    
  menu1:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$12,$13,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$21,$22,$23,$24,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$3c,$00,$00,$00,$00,$00,$30,$31,$32,$33,$34,$00,$00,$00
	  .byte $00,$00,$03,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$4a,$3b,$4b,$00,$00,$00,$00,$00,$41,$42,$43,$00,$00,$00,$00
	  .byte $00,$00,$03,$00,$00,$dc,$dd,$e3,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$3d,$4e,$ec,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$de,$e5,$e4,$e0,$df,$00,$58,$00,$00,$00,$00
	  .byte $00,$2e,$02,$3e,$ee,$ed,$00,$00,$00,$00,$00,$00,$00,$00,$00,$cb
    .byte $00,$00,$00,$00,$00,$f4,$e6,$e2,$e1,$00,$d7,$48,$00,$00,$00,$00
	  .byte $00,$02,$02,$02,$eb,$00,$00,$00,$00,$00,$00,$00,$00,$00,$cd,$02
	  .byte $00,$00,$d5,$d6,$f2,$f5,$fc,$fb,$f6,$d8,$4d,$47,$00,$00,$00,$00
	  .byte $2e,$02,$02,$02,$ea,$e7,$00,$00,$00,$00,$00,$00,$bb,$cf,$02,$02
	  .byte $00,$03,$00,$00,$f3,$fe,$fd,$fa,$f7,$00,$02,$02,$00,$00,$00,$00
	  .byte $02,$02,$02,$02,$02,$ef,$00,$00,$00,$00,$00,$bd,$be,$02,$02,$02
	  .byte $00,$03,$00,$00,$00,$f9,$f8,$00,$00,$2e,$02,$02,$1c,$1c,$1c,$1c
	  .byte $02,$02,$02,$02,$02,$0d,$00,$00,$00,$00,$bc,$d3,$02,$02,$02,$02
	  .byte $1b,$1b,$1b,$1b,$09,$f1,$db,$1b,$49,$54,$55,$02,$2c,$2c,$2c,$2c
	  .byte $02,$02,$02,$02,$02,$02,$0d,$1b,$1b,$1b,$ca,$02,$02,$02,$02,$02
	  .byte $02,$02,$02,$02,$02,$60,$61,$62,$63,$64,$65,$66,$02,$02,$02,$02
	  .byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	  .byte $02,$02,$02,$02,$02,$70,$71,$72,$73,$74,$75,$76,$02,$02,$02,$02
	  .byte $02,$02,$04,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	  .byte $02,$02,$02,$02,$02,$80,$81,$82,$83,$84,$85,$86,$02,$55,$02,$02
	  .byte $02,$04,$04,$04,$04,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02 
    .byte $02,$02,$02,$02,$02,$02,$90,$91,$02,$62,$96,$96,$66,$65,$04,$04
	  .byte $02,$04,$06,$04,$04,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$04
	  .byte $02,$02,$02,$02,$02,$02,$a0,$a1,$a2,$a4,$a6,$a6,$76,$75,$04,$04
	  .byte $02,$04,$06,$04,$04,$04,$02,$02,$02,$02,$02,$02,$02,$02,$02,$04
	  .byte $02,$02,$02,$02,$02,$02,$b0,$b1,$b2,$b4,$b6,$b6,$86,$85,$06,$04
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
    .byte $04,$06,$07,$07,$04,$04,$05,$06,$06,$04,$06,$04,$04,$04,$04,$07
	  .byte $04,$04,$04,$04,$04,$04,$04,$04,$05,$05,$06,$05,$05,$05,$06,$06
	  .byte $04,$04,$04,$07,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
	  .byte $04,$04,$04,$04,$04,$04,$04,$04,$04,$05,$05,$04,$04,$04,$04,$06
	  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$69,$6a,$6b,$6c,$6c,$01,$79
	  .byte $7a,$7b,$7c,$7a,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	  .byte $00,$00,$00,$00,$00,$00,$f0,$34,$00,$00,$00,$00,$00,$00,$0f,$03
	  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	  .byte $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a

; Character memory
.segment "CHARS"
.incbin "background.chr"
.incbin "wizard.chr"