; ******************************************* INFO ************************************************
; Menu screen is at $0a00, menu font at $2800
; Game screen is at $0400, game font at $2000, multicolor
; Sprites 0-3 are ghosts, sprite 4 is pacman
; First half of char ROM copied to lower half of user fonts
; Game screen is compressed at $81ef -> decompressed to $4000
; Game font is compressed at $8011 -> decompressed to $2000 
; Menu font is compressed at $9e82-> decompressed to $2800
; compreessed font data are 2bit count + 6bit tile numbers of table at $9dc6
; Only SID voices 1+2 are used with sawtooth+triangle
; SID voice 3 with noise and reg $1b used for random number generation 
; ************************************** USER ADDRESSES *******************************************
!addr GameScreen		= $0400		; game screen page
!addr MenuScreen		= $0a00		; game screen page
!addr CharGame			= $2000		; user character game
!addr CharMenu			= $2800		; user character menu
!addr MapData			= $4000		; Map data
!addr ScreenBackup1		= $4400		; Game screen backup player 1
!addr ScreenBackup2		= $4800		; Game screen backup player 2
!addr MapData			= $4000		; Map data
!addr LookUpTable		= $4c00		; LookUp Table 484 nibbles

; ***************************************** ZERO PAGE *********************************************
!addr state				= $07		; 0 = game, 3 = menu
!addr players			= $08		; 0 = 1 player, 1 = 2 players
!addr difficulty		= $09		; 0, 1, 2, 4, 6, 8, a, c
!addr jiffy_start		= $0b		; jiffy-1 at start
!addr temp				= $18		; temp byte
!addr lives1			= $1a		; lives player 1 (starts with 3)
!addr lives2			= $1b		; lives player 2 (starts with 3)
!addr difficulty1		= $1e		; difficulty player 1
!addr difficulty2		= $1f		; difficulty player 2
!addr score1			= $22		; score player 1 (lowbyte, last digit always zero)
!addr score2			= $23		; score player 1 (lowbyte, last digit always zero)
; score also 20,21-45,25-2e,2f
!addr pointer1			= $2a		; source pointer
!addr pointer2			= $2c		; target pointer
!addr sprite_y			= $41		; -$45 sprite y postion 
!addr jiffy				= $a2		; jiffy clock 20ms counter from raster interrupt = Vsync
!addr pressed_key		= $c5		; pressed key from interrupt
; ***************************************** VARIABLES *********************************************
!addr sprite_x			= $02d0		; -$02d4 sprite x positions (>>1 +$2c)


; -------------------------------------------------------------------------------------------------
; $