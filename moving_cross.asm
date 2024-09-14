.define PPUCTRL $2000
.define PPUMASK $2001
.define PPUSTATUS $2002
.define OAMADDR $2003
.define OAMDATA $2004
.define PPUSCROLL $2005
.define PPUADDR $2006
.define PPUDATA $2007

.define PALETTE_BG_0 $3F00
.define PALETTE_BG_1 $3F04
.define PALETTE_BG_2 $3F08
.define PALETTE_BG_3 $3F0C
.define PALETTE_SPRITE_0 $3F10
.define PALETTE_SPRITE_1 $3F14
.define PALETTE_SPRITE_2 $3F18
.define PALETTE_SPRITE_3 $3F1C

.define PATTERN_TABLE_LEFT $0FFF
.define PATTERN_TABLE_RIGHT $1FFF

.define PINK_COLOR $24
.define LIGHT_BLUE_COLOR $21
.define DARK_RED_COLOR $16
.define ORANGE_COLOR $27
.define BROWN_COLOR $18

.define sprite_tile_index $01
.define sprite_x $03
.define sprite_y $00
.define DMA $4014

.define JOYPAD1 $4016

.db "NES", $1A, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

;inits start program address to $8000
.org $8000

reset:
    SEI             ;(SET INTERRUPT) to be sure interrupts get reset when reset command is called. CLI is to activate interrupts, instead
    CLD             ;not necessary for nes, but allows to remove number decimal mode

init:

    LDX #$FF
    TXS

    ;SPENGO NMI
    LDX #0
    STX PPUCTRL
    
    STX PPUMASK         ;set PPUMASK to 0 to avoid graphic glitches at the start of the game.
						;In this way, PPUMASK won't draw anything at the start of the game.

    wait_for_vblank:
        LDA PPUSTATUS
        AND #%10000000
        BEQ wait_for_vblank

    init_palette:
;--------------------BACKGROUND PALETTES----------------------------------
        ;palette 0 for background init
        LDA #>PALETTE_BG_0
        STA PPUADDR
        LDA #<PALETTE_BG_0
        STA PPUADDR

        ;loading colors in the palette 0 for the background
        LDA #LIGHT_BLUE_COLOR 	;first background & sprite palette color must be equal
								;(e.g. first palette 0 for the background & sprite color must be equal.
								; If not, one of them will overwrite the other).
        STA PPUDATA
        LDA #DARK_RED_COLOR
        STA PPUDATA
        LDA #ORANGE_COLOR
        STA PPUDATA
        LDA #BROWN_COLOR
        STA PPUDATA
		
		;palette 2 for background init
        LDA #>PALETTE_BG_2
        STA PPUADDR
        LDA #<PALETTE_BG_2
        STA PPUADDR

        ;loading colors in the palette 2 for the background
        LDA #DARK_RED_COLOR
        STA PPUDATA
        LDA #BROWN_COLOR
        STA PPUDATA
        LDA #BROWN_COLOR
        STA PPUDATA
        LDA #ORANGE_COLOR
        STA PPUDATA
;--------------------BACKGROUND PALETTES----------------------------------

;--------------------SPRITE PALETTES----------------------------------
        ;palette 0 for sprites init
        LDA #>PALETTE_SPRITE_0
        STA PPUADDR
        LDA #<PALETTE_SPRITE_0
        STA PPUADDR

        ;loading colors in the palette 0 for the sprites
        LDA #LIGHT_BLUE_COLOR
        STA PPUDATA
        LDA #DARK_RED_COLOR
        STA PPUDATA
        LDA #ORANGE_COLOR
        STA PPUDATA
        LDA #BROWN_COLOR
        STA PPUDATA
		
		;palette 2 for sprites init
        LDA #>PALETTE_SPRITE_2
        STA PPUADDR
        LDA #<PALETTE_SPRITE_2
        STA PPUADDR

        ;loading colors in the palette 2 for the sprites
        LDA #LIGHT_BLUE_COLOR
        STA PPUDATA
        LDA #ORANGE_COLOR
        STA PPUDATA
        LDA #ORANGE_COLOR
        STA PPUDATA
        LDA #BROWN_COLOR
        STA PPUDATA
;--------------------SPRITE PALETTES----------------------------------

    LDX #%00010000
    STX PPUMASK 

;drawing sprites in OAMDATA.
;This is possible when not setting DMA
    ; LDA #$00
    ; STA OAMADDR

    ; LDA #$40
    ; STA OAMDATA ;sprite y
    ; LDA #$01
    ; STA OAMDATA ;sprite tile index
    ; LDA #$00
    ; STA OAMDATA ;sprite attributes (flip x, flip y, ....)
    ; LDA #$40
    ; STA OAMDATA ;sprite x
	
	; LDA #$80
    ; STA OAMDATA ;sprite y
    ; LDA #$00
    ; STA OAMDATA ;sprite tile index
    ; LDA #$00
    ; STA OAMDATA ;sprite attributes (flip x, flip y, ....)
    ; LDA #$80
    ; STA OAMDATA ;sprite x
	
	; LDA #$A0
    ; STA OAMDATA ;sprite y
    ; LDA #$00
    ; STA OAMDATA ;sprite tile index
    ; LDA #$00
    ; STA OAMDATA ;sprite attributes (flip x, flip y, ....)
    ; LDA #$A0
    ; STA OAMDATA ;sprite x
	
	; LDA #%00010000
	; STA PPUCTRL
	; LDA #%00000000
	; STA PPUCTRL
	
	; LDA #>PATTERN_TABLE_LEFT
	; STA PPUCTRL
	; LDA #<PATTERN_TABLE_LEFT
	; STA PPUCTRL
	
	; set_cross_sprite_y_to_screen_center:
		; INC sprite_y
		
		; LDA sprite_y
		; CMP #%100000
		; BNE set_cross_sprite_y_to_screen_center
    
	; LDA #%11100000 ;sprite y at the bottom
	LDA #%10000000 ;sprite y at the center
	; LDA #%00001000 ;sprite y on top
	STA sprite_y
	STA sprite_x
	
	LDX #$01
	STX sprite_tile_index

game_loop:
    wait_for_vblank_sprite:
        LDA PPUSTATUS
        AND #%10000000
        BEQ wait_for_vblank_sprite
	
	; Reading input data
	lda #$01
	sta JOYPAD1
	lda #$00
	sta JOYPAD1

	; Order: A B Select Start Up Down Left Right
	; only one bit is read at a time, so we have to read JOYPAD1 eight times

	; A
		lda JOYPAD1
		and #%00000001
		cmp #%00000001
		bne A_not_pressed

	A_not_pressed:

	; B
		lda JOYPAD1
		and #%00000001
		cmp #%00000001
		bne B_not_pressed

	B_not_pressed:

	; Select
		lda JOYPAD1
		and #%00000001
		cmp #%00000001
		bne Select_not_pressed

	Select_not_pressed:

	; Start
		lda JOYPAD1
		and #%00000001
		cmp #%00000001
		bne Start_not_pressed

	Start_not_pressed:

	; Up
		lda JOYPAD1
		and #%00000001
		cmp #%00000001
		bne Up_not_pressed

		JSR move_cross_up

	Up_not_pressed:

	; Down
		lda JOYPAD1
		and #%00000001
		cmp #%00000001
		bne Down_not_pressed

		JSR move_cross_down

	Down_not_pressed:

	; Left
		lda JOYPAD1
		and #%00000001
		cmp #%00000001
		bne Left_not_pressed
		
		JSR move_cross_to_left

	Left_not_pressed:

	; Right
		lda JOYPAD1
		and #%00000001
		cmp #%00000001
		bne Right_not_pressed

		JSR move_cross_to_right

	Right_not_pressed:
	
    LDA #$00
    STA DMA

	; PPUSCROLL: rendering line in nametable
    LDA #%00000000
    STA PPUSCROLL
    LDA #%00000000
    STA PPUSCROLL

    JMP game_loop


move_cross_up:
	DEC sprite_y
	RTS
	
move_cross_down:
	INC sprite_y
	RTS
	
move_cross_to_left:
	DEC sprite_x
	RTS
	
move_cross_to_right:
	INC sprite_x
	RTS


nmi:
    RTI

irq:    
    RTI

.goto $FFFA ;inits address values to 0, from the address the current program is to the specified address (in this case, $FFFA).
			;a 2nd arg can be specified: the init value.
			
.dw nmi ;NMI (non-maskable interrupt)
.dw reset ;RESET
.dw irq ;IRQ/BRQ

;drawing pattern tables based on palette colors.
;0-0 draws first palette color;
;1-0 draws second palette color;
;0-1 draws third palette color;
;1-1 draws fourth palette color;
draw_pattern_table:
	.db %00011000
	.db %00011000
	.db %00011000
	.db %11111111
	.db %11111111
	.db %00011000
	.db %00011000
	.db %00011000

	.db %00011000
	.db %00011000
	.db %00011000
	.db %00000000
	.db %00000000
	.db %00011000
	.db %00011000
	.db %00011000


	.db %00011000
	.db %00011000
	.db %00011000
	.db %11111111
	.db %11111111
	.db %00011000
	.db %00011000
	.db %00011000

	.db %00000000
	.db %00000000
	.db %00000000
	.db %00000000
	.db %00000000
	.db %00000000
	.db %00000000
	.db %00000000
	
	.db %00000000
	.db %00000000
	.db %00000000
	.db %11100111
	.db %11100111
	.db %00000000
	.db %00000000
	.db %00000000

	.db %00000000
	.db %00000000
	.db %00000000
	.db %00000000
	.db %00000000
	.db %00000000
	.db %00000000
	.db %00000000