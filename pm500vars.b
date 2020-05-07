; ******************************************* INFO ************************************************
; Menu screen is at $0a00, menu font at $2800
; Game screen is at $0400, game font at $2000, multicolor
; Sprites 0-3 are ghosts, sprite 4 is pacman, sprites are 10 px heigh, 6px wide + xpanded -> 12px
; Sprite data pointer are static $c0-$c4 -> $3000-$3100, sprites are not multicolor
; Sprite source data is at $5300,$5400-$57ff and will copied in each cycle to the VIC sprite data
; First half of char ROM copied to lower half of user fonts
; Game screen is compressed at $81ef -> decompressed to $4000
; Game font is compressed at $8011 -> decompressed to $2000 
; Menu font is compressed at $9e82-> decompressed to $2800
; compreessed font data are 2bit count + 6bit tile numbers of table at $9dc6
; Only SID voices 1+2 are used with sawtooth+triangle
; SID voice 3 with noise and reg $1b used for random number generation 
; ***************************************** ZERO PAGE *********************************************
!addr state				= $07		; 0 = game, 3 = menu
!addr players			= $08		; 0 = 1 player, 1 = 2 players
!addr difficulty		= $09		; 0, 1, 2, 4, 6, 8, a, c
!addr delay_menu		= $0b		; jiffy-1 at start for 5s menu delay
!addr temp				= $18		; temp byte
!addr lives1			= $1a		; lives player 1 (starts with 3)
!addr lives2			= $1b		; lives player 2 (starts with 3)
!addr difficulty1		= $1e		; difficulty player 1
!addr difficulty2		= $1f		; difficulty player 2
; $20,21 score
!addr score1			= $22		; score player 1 (lowbyte, last digit always zero)
!addr score2			= $23		; score player 1 (lowbyte, last digit always zero)
; $24,25 score
; $26
; $28
!addr pointer1			= $2a		; source pointer
!addr pointer2			= $2c		; target pointer
; $2e,2f score
!addr data_tb1			= $3c		; 20 bytes from $9bbf
!addr sprite_y			= $41		; -$45 sprite y postion 
!addr jiffy				= $a2		; jiffy clock 20ms counter from raster interrupt = Vsync
!addr spritedata_pointer= $c0		; 16bit pointer for sprite data copy
!addr pressed_key		= $c5		; pressed key from interrupt
; ***************************************** VARIABLES *********************************************
!addr sprite_x			= $02d0		; -$02d4 sprite x positions (>>1 +$2c)

!ifdef 	P500{
		lda #SYSTEMBANK
		sta IndirectBank				; select bank 15
		ldy #$
		sta (VIC),y						; VIC exterior color = black
		lda #GAMEBANK
		sta IndirectBank				; select bank 0
} else{
}


