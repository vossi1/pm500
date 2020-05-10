; ******************************************* INFO ************************************************
; Menu screen is at $0c00, menu font at $2800
; Game screen is at $0400, game font at $2000, multicolor
; Sprites 0-3 are ghosts, sprite 4 is pacman, sprites are 10/11 px heigh, 6px wide + xpanded -> 12px
; Sprite data pointer are static = $c0-$c4 -> $3000-$3100, sprites are not multicolor
; Sprite source data is at $5300,$5400-$57ff and will copied in each cycle to the VIC sprite data
; First half of char ROM copied to lower half of user fonts
; Game screen is compressed at $81ef -> decompressed to $4000
; Game font is encoded at $8011 -> decoded to $2000 
; Menu font is encoded at $9e82-> decoded to $2800
; Encoded font data are 2bit count + 6bit tile numbers of table at $9dc6
; - 00-7f char, 80-bf next char repeated 0-3f, c0-fe low nib - next byte high-low nib, ff end
; 512 Nibbles table lo+hi decoded -> $4c00
; Only SID voices 1+2 are used with sawtooth+triangle
; SID voice 3 with noise and reg $1b used for random number generation 
; ***************************************** ZERO PAGE *********************************************
!addr state				= $07		; 0 = game, 3 = menu
!addr players			= $08		; 0 = 1 player, 1 = 2 players
!addr difficulty		= $09		; 0, 1, 2, 4, 6, 8, a, c
!addr delay_menu		= $0b		; jiffy-1 at start for 5s menu delay
!addr temp				= $18		; temp byte
!addr actual_player		= $19		; actual player
!addr lives				= $1a ; $1b	  lives player 1, 2 (starts with 3)
!addr level				= $1e ; $1f	  level player 1, 2
; $20,21 score
!addr score				= $22 ; $23   score player 1, 2 (lowbyte, last digit always zero)
; $24,25 score
; $26
; $28
!addr pointer1			= $2a		; source pointer
!addr pointer2			= $2c		; target pointer
; $2e,2f score
!addr data_tb1			= $3c		; 20 bytes from $9bbf
!addr sprite_y			= $41		; -$45 sprite y postion 
!addr pause				= $57		; $80 = pause
!addr jiffy				= $a2		; jiffy clock 20ms counter from raster interrupt = Vsync
!addr blink_counter		= $b9		; blink counter 1up/2up 0/1=off/on
!addr spritedata_pointer= $c0		; 16bit pointer for sprite data copy
!addr pressed_key		= $c5		; pressed key from interrupt
; ***************************************** VARIABLES *********************************************
!addr sprite_x			= $02d0		; -$02d4 sprite x positions (>>1 +$2c)

!ifdef 	P500{
		lda #GAMEBANK
		sta IndirectBank				; select bank 0 for pointer operations
}

!ifdef 	P500{
		lda #SYSTEMBANK
		sta IndirectBank				; switch back to bank 15
}


!ifdef 	P500{
} else{
}


