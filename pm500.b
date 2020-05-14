; Disassembled by Vossi 04/2020
; Prepared for ACME reassembling
; Comments by Vossi 05/2020
; Converted for P500 by Vossi 05/2020
!cpu 6502
; switches
;P500 = 1		; P500 bank 0 file
;CRT = 1		; CRT header for VICE
!ifdef 	P500{!to "pm500.prg", cbm
} else{ !ifdef CRT {!to "pacman.crt", plain : *= $7fb0 : !source "crthead.b"
} else{ !to "pacman.rom", plain }}
; ########################################### TODO ################################################
; nothing
; ########################################### BUGS ################################################
;
; Fruits in options are not in multicolor so they look like zebra ;)
; Background color 2 for multicolor is not set to 2 but works on C64 because its VIC init value
; ######################################### P500 MODS #############################################
; Indirect reg standard = $15, switch only to $0 for game indirect pointer instructions 
; Game runs exclusive - Kernal not used -> IRQ vector $fffe in bank 0 set to game irq routine
; Added unused highscore text above highscore digits
; Added rasterirq -> switch to multicolor-mode in options to display the fruits correctly
; Set backgroundcolor 2 correctly
; Added uncompressed Maze, user chars and nibble tables
; ******************************************* INFO ************************************************
; Options screen is at $0c00, options font at $2800
; Game screen is at $0400, game font at $2000, multicolor
; First two lines of game screen are not multicolor
; Sprites 0-3 are monsters, 4 is pacman, sprites are 10/11 px heigh, 6px wide + xpanded -> 12px
; Sprite data pointer are static = $c0-$c4 -> $3000-$3100, sprites are not multicolor
; Sprite source data is at $5300,$5400-$57ff and will copied in each cycle to the VIC sprite data
; First half of char ROM copied to lower half of user fonts
; Game screen is compressed at $81ef -> decompressed to $4000
; Game font is encoded at $8011 -> decoded to $2000 
; Options font is encoded at $9e82-> decoded to $2800
; Encoded font data are 2bit count + 6bit tile numbers of table at $9dc6
; - 00-7f char, 80-bf next char repeated 0-3f, c0-fe low nib - next byte high-low nib, ff end
; Nibble table lo+hi decoded -> $4c00
; Only SID voices 1+2 are used with sawtooth+triangle
; SID voice 3 with noise and reg $1b used for random number generation 
; Monsters/Ghosts/Goblins: Lightred=Pinky, Lightred=Pinky, Green=Inky, Orange=Clyde
; ******************************************* FONT ************************************************
; Optionsfont:	$00-$17 = PACMAN logo 2 rows of 12 chars
; Gamefont:	 	$00-$21 = Maze parts except $01 = pill, $03 = cross, $1b = mini-pacman (live)
; (all MCM)		$22-$2b = READY:
;				$2c-$39	= GAME OVER
;				$4e-$60	= Points 1,3,5,7,10,20,30,50,00,0
;				$61-$64 = Maze parts
; Both fonts:	$3a-$4d = 10 fruits (MCM)
;				$80-$bf = Font CharROM -$8f signs, $90-$99 numbers, $a1-$bf letters
; ***************************************** CONSTANTS *********************************************
FILL					= $aa		; fills free memory areas with $aa
NOPCODE					= $ea		; nop instruction for fill
GAMEBANK				= $00		; Game code bank
SYSTEMBANK				= $0f		; systembank
; color codes
BLACK					= $00
WHITE					= $01
RED						= $02
GREEN					= $05
BLUE					= $06
YELLOW					= $07
ORANGE					= $08
BROWN					= $09
LIGHTRED				= $0a
LIGHTGREEN				= $0d
LIGHTBLUE				= $0e
GRAY3					= $0f
MCM						= $08		; bit#3 for multicolor character
!ifdef P500{OFFLOGO		= $c8		; +5 lines displacement for PACMAN logo
			OFFTEXT		= $78		; -3 lines displacement for Player selection text
} else{		OFFLOGO		= 0			; no offset
			OFFTEXT		= 0}
RASTERLINE1				= $32+7*8	; Above PACMAN logo
RASTERLINE2				= $33+12*8	; just below the options fruit in line 11 
; game
LIVES					= 3			; start lives
FRUITDELAY				= $c0		; fruit delay
; ************************************** P500 REGISTER ********************************************
VR_MODEY				= $11
VR_RASTER				= $12
VR_MOBENA				= $15
VR_MCMCSX				= $16
VR_MEMPT				= $18
VR_IRQ					= $19
VR_EIRQ					= $1a
VR_MOBXPA				= $1d
VR_MOBMOB				= $1e
VR_EXTCOL				= $20
VR_BGRCOL				= $21
VR_MOBCOL				= $27
SR_V1FREQ				= $00
SR_V1CTRL				= $04
SR_V1SR					= $06
SR_V2FREQ				= $07
SR_V2CTRL				= $0b
SR_V2SR					= $0d
SR_V3FREQ				= $0e
SR_V3CTRL				= $12
SR_MODVOL				= $18
SR_RANDOM				= $1b
; ************************************** P500 ADDRESSES *******************************************
!addr CodeBank			= $00		; code bank register
!addr IndirectBank		= $01		; indirect bank register
!addr CharROMbase		= $c000		; Character ROM
!addr ColorRAMbase		= $d400		; Color RAM
!addr VICbase			= $d800		; VIC
!addr SIDbase			= $da00		; SID
!addr CIAbase			= $dc00		; CIA
!addr TPI1base			= $de00		; TPI1
!addr TPI2base			= $df00		; TPI2
!addr HW_IRQ			= $fffe		; System IRQ Vector
!addr HW_NMI			= $fffa		; System NMI Vector
; *************************************** C64 ADDRESSES *******************************************
!addr CPUPort64			= $01		; 6510 CPU port
!addr CharROM64			= $d000		; Character RAM
!addr ColorRAM64		= $d800		; Color RAM
!addr ioinit			= $fda3		; Kernal IRQ init
!addr ramtas			= $fd50		; Kernal RAM init
!addr restor			= $fd15		; Kernal hardware I/O vector init
!addr cint				= $ff5b		; Kernal video init
; ************************************** USER ADDRESSES *******************************************
!addr GameScreen		= $0400		; Game screen page
!addr SpriteDataPtr		= $07f8		; 5 Pointer to sprite 0-4
!addr Maze				= GameScreen + 2*40	; Line 2 of game screen
!addr OptionsScreen		= $0c00		; Game screen page
!addr GameChar			= $2000		; User character game
!addr OptionsChar		= $2800		; User character options
!addr SpriteData		= $3000		; Sprite data 5x $40
!addr Player1Save		= $4400		; Game screen bac‚kup player 1
!addr Player2Save		= $4800		; Game screen backup player 2
!addr SpriteRAM			= $5300		; 5x Sprite RAM -$57ff
; ***************************************** ZERO PAGE *********************************************
!addr attract_ATARI		= $03		; Atari ATTRACT FLAG for screen saver - not used on Commodore
!addr mode				= $07		; mode flag: 0 = game, 1 = startup, 2 = delay options, 3 = options
!addr players			= $08		; 0 = 1 player, 1 = 2 players
!addr difficulty		= $09		; 0, 1, 2, 4, 6, 8, a, c
!addr restart_flag		= $0a		; 1 = new game, 2 after init, 3 at ready
!addr delay_options		= $0b		; Jiffy-1 at start for 5s options delay
!addr atract_timer_ATARI= $0c		; Countdown timer to attract mode - ONLY ATARI
!addr game_over_flag	= $0e		; 
!addr ready_flag		= $0f		; 
!addr intro_flag		= $10		; 
!addr swap_player_flag	= $11		; 
!addr reset_flag		= $12		;
!addr reset_timer		= $13		;
!addr rereck_flag		= $14		;
!addr rerack_sequence	= $15		; RERACK = Draw new dots after cleared the maze
!addr rerack_timer		= $16
!addr rerack_flash_count= $17
!addr temp				= $18		; Temp byte
!addr player_number		= $19		; Actual player 0=1, 1=2
!addr extra_pacman1		= $1a		; Lives player 1 (starts with 3)
!addr extra_pacman2		= $1b		; Lives player 2 (starts with 3)
!addr bonus_pacman		= $1c ; $1d   Bonus pacman player 1,2
!addr maze_count1		= $1e 		; Maze player 1
!addr maze_count2		= $1f 		; Maze player 2
!addr bigdot_status		= $20 ; $21   Big dot status player 1,2
!addr dots_eaten_lo		= $22 ; $23	  Dots eaten player 1,2 lowbyte
!addr dots_eaten_hi		= $24 ; $25	  Dots eaten player 1,2 highbyte
!addr score_ptr1		= $26		;
!addr score_ptr2		= $28		;
!addr pixel_get_ptr		= $2a		; Source pointer
!addr pixel_put_ptr		= $2c		; Target pointer
!addr fruit_counter		= $2e ; $2f	  Fruit counter player 1, 2
!addr bounce_timer_ATARI= $30		; ATARI key debounce - not used on Commodore
!addr tunnel_logic		= $32 ; -$33  used for tunnel logic
!addr monster_delay		= $34 ; -$37  Monster 1-4 delay

!addr pacman_screen_ptr	= $3c ; -$3d  Pointer to pacman screen address
!addr pacman_byte_ctr	= $3e		;
!addr pacman_vpos_save	= $3f		;
!addr pacman_hpos_save	= $40		;
!addr monster_vpos		= $41 ; -$44  Monster 1-4 y position (VIC: -$1b)
!addr pacman_vpos		= $45 		; Pacman y position (VIC: -$1b)
!addr monster_hpos		= $46 ; -$49  Monster 1-4 x position
!addr pacman_hpos		= $4a		; Pacman x position
!addr monster_direction	= $4b ; -$4e  Monster 1-4 direction
!addr pacman_direction	= $4f 		; Pacman old direction
!addr player_score_text = $50 ; -$55  Player score chars - 6 digits
!addr score_carry_bit	= $56		; Player score carry bit for calculation
!addr pause_flag		= $57		; $80 = pause
!addr chase_whine_delta = $58		;
!addr fruit_timer 		= $59 ; -$5a  fruit timer (10 secs)
!addr fruit_display_flag= $5b	  	; fruit display flag
!addr fruit_color		= $5c 		; fruit color
!addr fruit_score_flag	= $5d	 	;fruit score flag - only stored, not used on Commodore 
!addr fruit_score_timer = $5e 		;fruit score timer
!addr notes_counter		= $5f		; Counter for music
;!addr vpos_saver		= $60		; verical position saver - not used on Commodore
!addr hpos_saver		= $61		; horizontal position saver
!addr pacman_vmap_count = $62
!addr pacman_motion_cnt	= $63
!addr pacman_adv_turning= $64
!addr pacman_dly_eating	= $65		; pacman delay eating dots
!addr pacman_status		= $66
!addr pacman_sequence	= $67
!addr pacman_new_dir	= $68
!addr chase_timer		= $69
!addr monster_still_flag= $6a
!addr monster_skirt_flag= $6b
!addr monster_speed_sequ= $6c ; -$70  Monster 1-4, Pacman speed sequence
!addr monster_speed_cnt	= $71 ; -$74  Monster 1-4 speed count
!addr pacman_speed_count= $75
!addr monster_patt_index= $76
!addr monster_patt_count= $7a
!addr monster_targ_hpos	= $7e ; -$81 Monster 1-4 target horizontal position 
!addr monster_targ_vpos	= $82 ; -$85 Monster 1-4 target vertical position
!addr monster_timer		= $86 ; -$89 Monster 1-4 timer
!addr monster_status	= $8a ; -$8d Monster 1-4 Status - bit#7 = flight (Blue eatable)
!addr monster_start_sequ= $8e ; -$91 Monster start sequence
!addr monster_vdir		= $92 ; -$95 Monster vertical choice
!addr monster_hdir		= $96 ; -$99 Monster horizontal choice
!addr chase_sound_freq	= $9a ; CHASE = ?
!addr chase_sound_dir	= $9b ; 1=inc, 2=dec
!addr chase_sound_start	= $9c
!addr flight_sound_freq	= $9d
!addr flight_sound_dir	= $9e
!addr flight_volume		= $9f
!addr jiffy				= $a2		; Jiffy clock 20ms counter from raster interrupt = Vsync
!addr flight_sound_start= $a3
!addr freeze_flag		= $a4
!addr gulp_count1		= $a5		; GULP = eat monster
!addr gulp_count2		= $a6
!addr gulped_last		= $a7		; last monster eaten
!addr gulp_count		= $a8
!addr fizzle_flag		= $a9		; 1=fizzle
!addr fizzle_ptr		= $aa		; FIZZLE = ?
!addr fizzle_timer		= $ab
!addr fizzle_status		= $ac
!addr fizzle_sequence_no= $ad
!addr fizzle_freq_base	= $ae
!addr fizzle_frequency	= $af
!addr fizzle_counter	= $b0
!addr tweet_sound_flag  = $b1		; TWEET = standard sirene
!addr tweet_sound_freq	= $b2
!addr eatdot_sound_flag = $b3		; EAT DOT
!addr eatdot_sound_cnt	= $b4
!addr eatdot_sound_togg	= $b5 
!addr gobble_direction	= $b6		; GOBBLE = ?
!addr gobble_frequency	= $b7
!addr flash_xup_timer	= $b9		; flash counter 1up/2up 0=off / 1=on
!addr flash_timer		= $ba		; flash timer blue/white monsters
!addr flash_count		= $bb		; flash count blue/white monsters
!addr flight_timer		= $bc		; FLIGHT = blue eatable
!addr tunnel_bitmask	= $bd		; TUNNEL
!addr tunnel_iterat_cnt	= $be
!addr temp_collision	= $bf
!addr spritedata_ptr	= $c0		; 16bit pointer for sprite data copy
!addr pressed_key		= $c5		; Pressed key from interrupt - only Commodore
; ***************************************** VARIABLES *********************************************
!addr sprite_x			= $02d0	; -$02d4 Sprite 0-4 x positions (VIC >>1 +$2c) ATARI: GTIA HPOSP0
!addr sprite_pacman_x	= $02d5 ; -$02d7 Pacman X storage ?
!addr PacmanBuffer		= $5800 ; -$580f Pacman image buffer 16 bytes
!addr MonsterBuffer		= $5810 ; -$581f Monster image buffer 16 bytes

; ************************************** P500 ZERO PAGE *******************************************
!addr ColorRAM0			= $e6
!addr ColorRAM1			= $e8
!addr ColorRAM2			= $ea
!addr ColorRAM3			= $ec
!addr VIC				= $ee
!addr VIC01				= $f0
!addr VIC27				= $f2
!addr SID				= $f4
!addr CIA				= $f6
!addr TPI1				= $f8
!addr TPI2				= $fa
!addr CharROM0			= $fc
!addr CharROM1			= $fe
; ***************************************** ZONE INIT *********************************************
!zone init
!initmem FILL
*= $8000
!ifdef 	P500{
Cold:	sei
		cld
		ldx #$ff
		txs
		jmp Start
} else{ ; ROM ident
		!byte <Start, >Start, <Warm, >Warm	; ROM start addresses
		!byte $c3, $c2, $cd, "8"	; cbm-rom ident-bytes 'C'= with init, 'BM', '8' = 4k-block 8
}
; ***************************************** ZONE DATA1 ********************************************
!zone data1
*= $8008
; $8008 table unused
		!byte $30, $02, $bb, $5a, $30, $5f, $ee, $3d, $a8
; ONLY C64 ROM: Addresses to decoded ROM data, Encoded font, compressed maze
!ifndef P500 {!source "c64cdata.b"}
; ***************************************** ZONE CODE *********************************************
!zone code
; game code
Start:
!ifdef 	P500{ 
		lda #SYSTEMBANK
		sta IndirectBank				; select bank 15
		jsr InitP500
		lda #GAMEBANK
		sta IndirectBank				; select bank 0 for data copy and init
} else{
*= $8394
		jsr ioinit 						; IRQ init
		jsr ramtas 						; RAM init
		jsr restor 						; hardware I/O vector init
		jsr cint   						; video init
}
Warm:	lda #$00
		ldx #$c2
clearzp:sta $02,x						; clear zero page $03 - $c4
		sta GameScreen-1,x				; clear top of game screen $0400 - $04c1
		dex
		bne clearzp						; next byte
		inc mode						; increase mode to 1 to start in options mode
		ldx jiffy						; load jiffy = $00 (cleared at ZP clear loop)
		dex
		stx delay_options				; remember jiffy-1 for 255 x 20ms = 5s options delay
!ifdef 	P500{
; P500 Copy chars $00-$3f of the first (graphic) fontset to $80 of tboth custom fonts
		lda #SYSTEMBANK					; select bank 15 to get font from char ROM
		sta IndirectBank
fontcpy:lda (CharROM1),y				; load from character ROM - Y already $00	
		sta GameChar+$400,y				; store to game fontset from char $80
		sta OptionsChar+$400,y			; store to options fontset from char $80
		lda (CharROM0),y
		sta GameChar+$500,y
		sta OptionsChar+$500,y
		dey
		bne fontcpy
		sty IndirectBank				; select bank 0 - Y already $00
; Copy User chars
uchrcpy:lda UserFontGame,y	
		sta GameChar,y
		lda UserFontGame+$100,y	
		sta GameChar+$100,y
		lda UserFontGame+$200,y	
		sta GameChar+$200,y
		lda UserFontGame+$300,y	
		sta GameChar+$300,y
		lda UserFontOptions,y	
		sta OptionsChar,y
		dey
		bne uchrcpy
; P500 SID init							; x already $ff
		lda #SYSTEMBANK
		sta IndirectBank	; select bank 15 - from here as STANDARD!
		lda #$ff
		ldy #SR_V3FREQ
		sta (SID),y						; SID voice 3 frequency lo to $ff 
		iny
		sta (SID),y						; SID voice 3 frequency hi to $ff 
		ldy #SR_V3CTRL
		lda #$80
		sta (SID),y						; SID voice 3 to $80 = noise for random generation
		lda #$f0
		ldy #SR_V1SR
		sta (SID),y						; SID voice 1 SR to $f0
		ldy #SR_V2SR
		sta (SID),y						; SID voice 2 SR To $f0
} else{
; C64 decompress, decode and data copy routines 
!source "c64decod.b"
; $848a SID init						; x already $ff
		stx $d40e						; SID voice 3 frequency lo to $ff 
		stx $d40f						; SID voice 3 frequency hi to $ff 
		lda #$80
		sta $d412						; SID voice 3 to $80 = noise for random generation
		lda #$f0
		sta $d406						; SID voice 1 SR to $f0
		sta $d40d						; SID voice 2 SR To $f0
}
; $849d Copy fruit chars from game font to options font
		ldx #$a1						; copy $a1 bytes
fchrcpy:lda GameChar+$1cf,x
		sta OptionsChar+$1cf,x
		dex
		bne fchrcpy
!ifdef 	P500{
; P500 Hardware interrupt vector setup, enable VIC raster IRQ
		lda #$01
		ldy #VR_EIRQ
		sta (VIC),y						; VIC enable raster interrupt
		lda #$1b
		ldy #VR_MODEY
		sta (VIC),y						; VIC RC8 = 0, DEN, 40 columns, Y = 3
		lda #RASTERLINE2
		ldy #VR_RASTER
		sta (VIC),y						; VIC raster reg = $032 (start visible screen)
} else{
; $84a8 C64 Kernal interrupt vector setup
		sei								; disable interrrupts
		lda #<Interrupt
		sta $0314
		lda #>Interrupt
		sta $0315						; set IRQ vector to $8583
		lda #$01
		sta $d01a						; VIC enable raster interrupt
		lda #$1b
		sta $d011						; VIC RC8 = 0, DEN, 40 columns, Y = 3
		lda #$32
		sta $d012						; VIC raster reg = $032 (start visible screen)
}
		cli								; enable interrupts
; -------------------------------------------------------------------------------------------------
; $84c3 New game vector	
reinit:	lda #$00
		ldx #$1f
reinilp:sta game_over_flag,x			; clear game variables $0e-$2d
		dex
		bpl reinilp
		txs								; init stack with $ff
		jsr ClearAudio					; SID sound off
!ifdef 	P500{
		ldy #VR_EXTCOL
		sta (VIC),y						; VIC exterior color = black
		iny
		sta (VIC),y						; VIC background color0 = black
		lda #LIGHTBLUE
		iny
		sta (VIC),y						; VIC background color1 = lightblue
		lda #RED
		iny
		sta (VIC),y						; VIC background color2 = red
		ldy #$ff						; set Y = $ff because its set to this value after ClearAudio
} else{
		sta $d020						; VIC exterior color = black
		sta $d021						; VIC background color0 = black
		lda #LIGHTBLUE
		sta $d022						; VIC background color1 = lightblue
}
; now initialize player screens
		jsr SetColor					; sub: init color RAM and VIC Sprite colors
		jsr InitGameScreen				; sub: copy game screen to screen RAM
		jsr SaveScreenPlayer1			; sub: init game screen player1 $4400
		jsr SaveScreenPlayer2			; sub: init game screen player2 $4800
		lda mode						; mode at startup = 1
		bne loop						; branch if not game mode 0
		lda jiffy
pacgmlp:cmp jiffy
		beq pacgmlp						; wait one jiffy = interrupt cycle
!ifdef 	P500{
		lda #$18						; VM13-10=$1 screen at $0400, CB13,12,11,x=1000 char at $2000
		ldy #VR_MEMPT
		sta (VIC),y						; set VIC memory pointers
		lda #$d8
		ldy #VR_MCMCSX
		sta (VIC),y						; VIC multicolormode MCM=1, 40 columns
		lda #$1f
		ldy #VR_MOBENA
		sta (VIC),y						; VIC enable spritess 0-4
		ldy #VR_MOBXPA
		sta (VIC),y						; VIC x-expand sprites 0-4
} else{
		lda #$18						; VM13-10=$1 screen at $0400, CB13,12,11,x=1000 char at $2000
		sta $d018						; set VIC memory pointers
		lda #$d8
		sta $d016						; VIC multicolormode MCM=1, 40 columns
		lda #$1f
		sta $d015						; VIC enable spritess 0-4
		sta $d01d						; VIC x-expand sprites 0-4
}
		jsr Setup						; sub: init new game
		jsr InitGameVariables			; sub: Init game variables: lives, level, score...
		lda players
		beq p1scini						; skip if 1 player
		jsr Set2Player					; sub: print 1Up, 2Up, print zero scores
		jmp godoit						; skip if 2 player
;		
p1scini:jsr Set1Player					; sub: print 1Up, print zero score
godoit: lda #$02
		sta restart_flag				; restart = 2 after init
loop:	lda mode
		bne goloop						; branch if not game mode
		lda game_over_flag
		beq chkfkey						; game runs - check only f-keys
		lda atract_timer_ATARI
		bne goloop
		lda #$04
		bne SetMode						; set ATARI attract mode mode = 4 - NOT on Commodore
!ifdef 	P500{
goloop:	ldy #$00
		lda (CIA),y						; load CIA Port A
		ora #$3f						; ignore bit#0-5
		cmp #$ff						; check if bit#6 and 7 = 1 -> no joystick button pressed
		bne cstart
chkfkey:lda pressed_key
		cmp #$07
		beq loop						; no key pressed
debounc:cmp pressed_key
		beq debounc						; debounce key
		cmp #$03
		beq cstart						; F1 -> start new game
		ldx mode
		cpx #$03
		bcc SetModeOptions				; set mode to 3 = options (at startup its 1)
		cmp #$06
		beq IncreaseDifficulty			; F5 -> increase difficulty
		cmp #$05
		beq TogglePlayers				; F3 -> toggle players
		bne loop
} else{
goloop:	lda #$10
		bit $dc00						; check CIA1 Porta column 4 = Joy 2 button
		beq cstart
chkfkey:lda pressed_key
		cmp #$ff
		beq loop						; no key pressed
debounc:cmp pressed_key
		beq debounc						; debounce key
		cmp #$ef
		beq cstart						; F1, Joy1Button -> start new game
		ldx mode
		cpx #$03
		bcc SetModeOptions				; set mode to 3 = options (at startup its 1)
		cmp #$bf
		beq IncreaseDifficulty			; F5 -> increase difficulty
		cmp #$df
		beq TogglePlayers				; F3 -> toggle players
		bne loop
}
; start new game
cstart:	lda #$00						; start new game
		sta game_over_flag
		sta mode						; mode = 0 game mode
		lda #$01
		sta restart_flag				; restart = 1 for new game
		jmp reinit						; start the new game in next main loop
; -------------------------------------------------------------------------------------------------
; $855c toogle players
TogglePlayers:
		lda players
		eor #$01
		sta players
; $8562	set mode=options -> return to main loop	
SetModeOptions:
		lda #$03
SetMode:
		sta mode
		jmp loop
; -------------------------------------------------------------------------------------------------
; $8569 increase difficulty		
IncreaseDifficulty:
		lda difficulty
		cmp #$02
		bcs incdif2						; branch if difficulty >= 2
		inc difficulty
		bne SetModeOptions				; return in options mode
incdif2:cmp #$0c
		beq resdiff						; branch if mode = $c
		inc difficulty
		inc difficulty					; from difficulty 2 add 2 for each step
		bne SetModeOptions				; return in options mode
resdiff:lda #$00
		sta difficulty					; after $c reset difficulty to 0
		beq SetModeOptions				; return in options mode
; -------------------------------------------------------------------------------------------------
Interrupt: ; game interrupt routine every 20ms = 1 jiffy
!ifdef 	P500{
; p500 Interrupt routine
		pha
		txa
		pha
		tya
		pha
		lda IndirectBank				; load actibe indirct bank
		pha								; remember on stack
		lda #SYSTEMBANK
		sta IndirectBank				; select bank 15

		ldy #VR_IRQ
		lda (VIC),y						; load VIC interrupt reg and mask bit 1
		and #$01
		beq jendirq						; skip if source is not raster interrupt
		ldy #VR_RASTER
		lda (VIC),y						; set VIC raster reg again
		cmp #RASTERLINE1
		beq rline1						; branch if rasterline 1
		lda mode
		beq setrl1						; branch if gaem mode = 0
; rasterline 2: disable multicolor - only if no game runs
		lda #$c8
		ldy #VR_MCMCSX
		sta (VIC),y						; VIC multicolormode MCM=0, 40 columns
		lda #RASTERLINE1
		ldy #VR_RASTER
		sta (VIC),y						; set VIC raster reg again
		lda #$81
		ldy #VR_IRQ
		sta (VIC),y						; clear VIC raster interrupt
jendirq:jmp endirq						; leave interrupt routine
; rasterline 1: always enable multicolor, inc jiffy, update screen/game cycle, check keys
rline1: inc jiffy						; increase jiffy
		lda #$d8
		ldy #VR_MCMCSX
		sta (VIC),y						; VIC multicolormode MCM=1, 40 columns
		lda mode
		beq setrl1						; branch if game mode = 0
		lda #RASTERLINE2				; set rasterline 2 if mode > 0 options, startup
		bne setrast
setrl1:	lda #RASTERLINE1				; set rasterline 1 again in game mode
setrast:ldy #VR_RASTER
		sta (VIC),y						; clear VIC raster reg again
		lda #$81
		ldy #VR_IRQ
		sta (VIC),y						; clear VIC raster interrupt
		dec bounce_timer_ATARI							;
		jsr UpdateScreen				; sub: Update screen: Startup, Options, Game

		lda freeze_flag
		bne ichkkey						; skip if freeze_flag is not 0
		jsr Tunnel						; Tunnel logic

ichkkey:ldy #$00
		sty pressed_key					; clear key variable
		lda #$fe
		iny
		sta (TPI2),y					; set TPI2 port B keyboard out 0 for F1 column
		iny
if1deb:	lda (TPI2),y					; load TPI2 port C
		sta temp
		lda (TPI2),y
		cmp temp
		bne if1deb						; debounce
		lsr								; shift bit#0 in carry
		rol pressed_key					; shift bit in key variable
		lda #$fb
		dey
		sta (TPI2),y					; set TPI2 port B keyboard out 2 for F3 column
		iny
if3deb:	lda (TPI2),y					; load TPI2 port C
		sta temp
		lda (TPI2),y
		cmp temp
		bne if3deb						; debounce
		lsr								; shift bit#0 in carry
		rol pressed_key					; shift bit in key variable
		lda #$ef
		dey
		sta (TPI2),y					; set TPI2 port B keyboard out 4 for F5 column
		iny
if5deb:	lda (TPI2),y					; load TPI2 port C
		sta temp
		lda (TPI2),y
		cmp temp
		bne if5deb						; debounce
		lsr								; shift bit#0 in carry
		rol pressed_key					; shift bit in key variable

endirq: pla
		sta IndirectBank				; restore indirect bank before interrupt
		pla
		tay
		pla
		tax
		pla
		rti
} else{	
; $8583 C64 interrupt routine
		lda $d019						; load VIC interrupt reg and mask bit 1
		and #$01
		beq endirq						; skip if source is not raster interrupt
		inc jiffy						; increase jiffy
		lda #$32
		sta $d012						; set VIC raster reg again to $32 (start)
		lda #$81
		sta $d019						; clear VIC raster interrupt
		dec bounce_timer_ATARI							;
		jsr UpdateScreen				; sub: Update screen: Startup, Options, Game
		lda freeze_flag
		bne ichkkey						; skip if freeze_flag is not 0
		jsr Tunnel						; Tunnel logic
ichkkey:ldx #$ff
		stx $dc02						; set CIA1 port A for output
		dex
		stx $dc00						; store $fe to Port A = select keyboard column 0
idebkey:lda $dc01						; load CIA1 port B = keyboard row
		cmp $dc01
		bne idebkey						; debounce key
		sta pressed_key					; store pressed key $ef=F1,JoyButton1 / $df=F3 / $bf=F5
		ldx #$00
		stx $dc02						; reset CIA1 port B to input
endirq: jmp $ea7e						; jump to kernal interrupt
}
; -------------------------------------------------------------------------------------------------
; $85bd Init options - set VIC, sound off, clears sprite x
InitOptions:
		lda #$00
		ldx #$07
-		sta sprite_x,x					; clear sprite x variables
		dex
		bpl -
		lda #$3a						; VM13-10=$3 screen $0c00, CB13,12,11,x=1010 char $2800						; VIC memory pointers
!ifdef P500{
		ldy #VR_MEMPT
		sta (VIC),y						; set VIC memory pointers
		jsr ClearAudio					; returns with A=$00
		ldy #VR_MOBENA
		sta (VIC),y						; VIC disable sprites
		ldy #$ff						; set Y = $ff because its set to this value after ClearAudio
		rts
} else{
}
		sta $d018						; set VIC memory pointers
		lda #$c8
		sta $d016						; set VIC Multicolor mode off, 40 Columns
		jsr ClearAudio					; returns with A=$00
		sta $d015						; VIC disable sprites
		rts
; -------------------------------------------------------------------------------------------------
; $85d8 print 1Up, print zero score
Set1Player:
		ldx #$05						; 6 chars
		lda #$80						; <space> code
clrsclp:sta GameScreen+$20,x			; clear score position 1
		sta GameScreen+$47,x 			; clear score position 2
		dex
		bpl clrsclp
setxpl:	jsr Flash1On
		ldx #$05						; 6 chars
		lda #$90						; code 0
zero1lp:sta GameScreen+$2a,x			; write player 1 score 000000
		dex
		bpl zero1lp
		rts
; $85f3 print 1Up, 2Up, print zero scores
Set2Player:
		jsr Flash2On
		ldx #$05
		lda #$90
zero2lp:sta GameScreen+$47,x			; write player 2 score 000000
		dex
		bpl zero2lp
		bmi setxpl
; -------------------------------------------------------------------------------------------------
; C64 copy and uncompress subroutines 
!ifndef P500{
; $8602 copy and uncompress user char
UncompressChar:	
		ldy #$00
		lda (pixel_get_ptr),y			; load byte
		and #$c0
		bne fontb67						; branch if bit 6 or 7 = 1
		lda (pixel_get_ptr),y			; load byte again
		tax								; move to X as index
		lda UserFontTiles,x				; load part 0-$3f from table
		jsr StoreIncPutPtr				; store in user font
fontlp:	jsr IncGetPtr
		jmp UncompressChar				; next byte
fontb67:lsr								; shift bit#6+7 to 1+0
		lsr
		lsr
		lsr
		lsr
		lsr
		sta temp						; store in temp as repeat counter
		lda (pixel_get_ptr),y			; load byte again
		cmp #$ff						; check if end of table
		beq fontret						; branch to rts
		and #$3f
		tax
		lda UserFontTiles,x				; load part 0-$3f from table
fontrpt:jsr StoreIncPutPtr				; store in user font
		dec temp
		bpl fontrpt						; repeat number of temp counter 
		bmi fontlp						; next byte
; $8636 Load+increase pointer 1
LoadIncGetPtr:
		lda (pixel_get_ptr),y
IncGetPtr:
		inc pixel_get_ptr
		bne fontret
		inc pixel_get_ptr+1
fontret:rts
; $863f Store+increase pointer 2
StoreIncPutPtr:
		sta (pixel_put_ptr),y
		inc pixel_put_ptr
		bne +
		inc pixel_put_ptr+1
+		rts
; $8648 Load byte from pointer 1 and shift high nibble to low
LoadHiNibbleGetPtr:
		lda (pixel_get_ptr),y
		lsr
		lsr
		lsr
		lsr
		rts
}
; -------------------------------------------------------------------------------------------------
; $864f Update screen: Startup, Options, Game
UpdateScreen:
		ldy mode
		bne tstmode
		jmp GameCycle					; mode = 0 game in progress
tstmode:cpy #$01
		bne mode2						; mode > 1 options delay $ff jiffys
; mode = 1: build title screen
		jsr InitOptions					; sub: Init options - set VIC, sound off, clears sprite x
		ldx #$00
		txa
		sta $02c5
		lda #$80
clropt:	sta OptionsScreen,x				; clear options screen
		sta OptionsScreen+$100,x
		sta OptionsScreen+$200,x
		sta OptionsScreen+$300,x
		sta GameScreen+$350,x			; clear lower part game screen
		dex
		bne clropt
		ldx #$0d
atarilp:lda Text_Atari1983,x			; write "Atari 1983" to screen
		sta OptionsScreen+$267,x
		dex
		bpl atarilp
		inx								; X = $00
logolp:	txa								; write PACMAN logo to screen
		clc
		adc #$01
		sta OptionsScreen+$5f+OFFLOGO,x	; first line char 1 - 12
		txa
		adc #$0d
		sta OptionsScreen+$87+OFFLOGO,x	; second line char 13 - 24
		inx
		cpx #$0c
		bne logolp
mode1i:	inc mode						; increase mode
mode1x:	rts
; -------------------------------------------------------------------------------------------------
; $8698 mode = 2: time out title screen: about 5 seconds
mode2:	cpy #$02
		bne mode3
		lda delay_options
		cmp jiffy						; wait for $ff jiffys from startup
		bne mode1x
		beq mode1i						; increase to next mode = 3 Options
; -------------------------------------------------------------------------------------------------
; $86a4 mode = 3: build option screen
mode3:	ldx players
		inx								; add 1 to get 1/2
		txa
		ora #$90
		sta OptionsScreen+$d7-OFFTEXT	; add $90 for char and print 1/2 players to screen
		lda players
		bne opt2pl						; skip if 2 players
		cpy #$03
		bne same1p						; skip if not mode 3 (ATARI attract mode 4)
		jsr Set1Player					; sub: print 1Up, print zero score on game screen
same1p:	lda #$92
		bne opttext						; skip 2 player init
opt2pl:	cpy #$03
		bne same2p						; skip if not mode 3
		jsr Set2Player					; sub: print 1Up, 2Up, print zero scores on game screen
same2p:	lda #$91
opttext:sta OptionsScreen+$14f-OFFTEXT	; write 2/1 player for F3 to screen
		ldx #$0a
txt1lp:	lda Text_PlayerGame,x
		sta OptionsScreen+$151-OFFTEXT,x; write "Player Game" twice 
		sta OptionsScreen+$d9-OFFTEXT,x
		dex
		bpl txt1lp
		jsr InitOptions					; sub: Init options - set VIC, sound off, clears sprite x
		ldx #$0b
txt2lp:	lda Text_PressF3To,x			; write F-key messages to screen 
		sta OptionsScreen+$127-OFFTEXT,x
		lda Text_PressF5To,x
		sta OptionsScreen+$240,x
		lda Text_PressF1To,x
		sta OptionsScreen+$2e0,x
		dex
		bpl txt2lp
		ldx difficulty
		lda DifficultyFruitsOptions,x
		sta OptionsScreen+$1cc				; write two chars for the difficulty fruit
		clc
		adc #$01
		sta OptionsScreen+$1cd
		ldx #$10
txt3lp:	lda Text_ChangeDifficulty,x
		sta OptionsScreen+$265,x			; write "change difficulty"
		dex
		bpl txt3lp
		ldx #$08
txt4lp:	lda Text_PlayGame,x
		sta OptionsScreen+$309,x			; write "play game"
		dex
		bpl txt4lp
		rts
; -------------------------------------------------------------------------------------------------
; $8715 main game cycle 
GameCycle:	
!ifdef 	P500{
; set sprite positions
		ldx #$04						; start with sprite 4
		ldy #$08						; x-position reg of sprite 4
spposlp:lda sprite_x,x					; load x
		sec
		sbc #$2c						; calc sprite x postion
		asl
		sta (VIC),y						; set VIC sprite x
		sty temp						; remember Y
		ldy #$10
		lda (VIC),y						; load sprite x MSB register from VIC
		bcc spnomsb						; skip if x-value <= $ff
		ora SpriteSetMSBMask,x			; set bit with bit-set-table
		bne spnoclr						; skip nextx instruction
spnomsb:and SpriteClearMSBMask,x		; clear bit with bit-clear-table
spnoclr:sta (VIC),y						; store new X-MSB-byte to VIC
		ldy temp						; restore Y
		lda monster_vpos,x				; load y
		clc
		adc #$1b						; calc sprite y postion
		sta (VIC01),y					; set VIC sprite y
		dey
		dey
		dex
		bpl spposlp						; next sprite
} else{
; $8715 set sprite positions
		ldx #$04						; start with sprite 4
		ldy #$08						; x-position reg of sprite 4
spposlp:lda sprite_x,x					; load x
		sec
		sbc #$2c						; calc sprite x postion
		asl
		sta $d000,y						; set VIC sprite x
		lda $d010						; load sprite x MSB register from VIC
		bcc spnomsb						; skip if x-value <= $ff
		ora SpriteSetMSBMask,x			; set bit with bit-set-table
		bne spnoclr						; skip nextx instruction
spnomsb:and SpriteClearMSBMask,x		; clear bit with bit-clear-table
spnoclr:sta $d010						; store new X-MSB-byte to VIC
		lda monster_vpos,x				; load y
		clc
		adc #$1b						; calc sprite y postion
		sta $d001,y						; set VIC sprite y
		dey
		dey
		dex
		bpl spposlp						; next sprite
}
; $8740 copy sprite data pointer for all 5 sprites
		ldx #$04
		lda #(SpriteData/$40)+4			; VIC Sprite Data at $c0-$c4
sdpcopy:sta SpriteDataPtr,x
		sec
		sbc #$01
		dex
		bpl sdpcopy
; $874d copy pacman sprite data		
		lda monster_vpos+4				; set data pointer to pacman
		sta spritedata_ptr
		lda #$53
		sta spritedata_ptr+1
		jsr SetPacmanDataEnd			; sub: Set last row, last byte of pacman
!ifdef 	P500{
		lda #GAMEBANK
		sta IndirectBank				; select bank 0 for $cx access
}
spmcopy:lda (spritedata_ptr),y		; copy pacman sprite data
		sta SpriteData+$100,x
		dex
		dex
		dex
		dey
		cpy #$02						; reach last byte
		bne spmcopy
		lda monster_vpos+0
		jsr SetMonsterDataEnd				; sub: Calc pointer, set last row, last byte of monster
; $876a copy sprite data of 4 monsters
sm0copy:lda (spritedata_ptr),y		; copy monster 0 sprite data
		sta SpriteData,x
		dex
		dex
		dex
		dey
		cpy #$01						; reach last byte
		bne sm0copy
		lda monster_vpos+1
		jsr SetMonsterDataEnd				; sub: Calc pointer, set last row, last byte of monster
sm1copy:lda (spritedata_ptr),y		; copy monster 1 sprite data
		sta SpriteData+$40,x
		dex
		dex
		dex
		dey
		cpy #$01						; reach last byte
		bne sm1copy
		lda monster_vpos+2
		jsr SetMonsterDataEnd				; sub: Calc pointer, set last row, last byte of monster
sm2copy:lda (spritedata_ptr),y		; copy monster 2 sprite data
		sta SpriteData+$80,x
		dex
		dex
		dex
		dey
		cpy #$01						; reach last byte
		bne sm2copy
		lda monster_vpos+3
		jsr SetMonsterDataEnd			; sub: Calc pointer, set last row, last byte of monster
sm3copy:lda (spritedata_ptr),y		; copy monster 3 sprite data
		sta SpriteData+$c0,x
		dex
		dex
		dex
		dey
		cpy #$01						; reach last byte
		bne sm3copy
!ifdef 	P500{
		lda #SYSTEMBANK
		sta IndirectBank				; restore to bank 15
}
; $87ad check joystick button -> toggle pause game
		lda jiffy
		and #$0f
		bne vbgame1						; only every 16 cycles
!ifdef 	P500{
		ldy #$00						
		lda (CIA),y						; load CIA Port A - joystick button = pause
		ora #$3f						; ignore bit#0-5
		cmp #$ff						; check if bit#6 and 7 = 1 -> no joystick button pressed
		beq vbgame
} else{
		lda $dc00
		and #$10
		bne vbgame
}
		lda pause_flag
		eor #$80						; toggle pause -> $80 = pause
		sta pause_flag

vbgame:	lda atract_timer_ATARI
		beq vbgame1
		dec atract_timer_ATARI			; decrease attract timer - only for ATARI

vbgame1:lda pause_flag
		beq vbgame2						; skip if 0 = no pause
		jmp ClearAudio					; JUMP to Sound off in pause and return from game cycle!
vbgame2:jsr FlashXUp					; sub: blink 1/2up of active player
		lda restart_flag
		beq tstgmov						; branch to check game over
		cmp #$02
		beq vtunes						; branch if 2 to get ready
tstgmov:lda game_over_flag				; check game over
		beq testrrk						
		lda players
		beq vfls1on						; skip if 1 player
		jsr Flash2On					; Flash 2Up
vfls1on:jmp Flash1On					; Flash 1Up
; $87e6 Test flags
testrrk:lda rereck_flag
		beq testvrd
		jmp rerack
testvrd:lda ready_flag
		bne vready
		lda intro_flag
		bne vintro
vsquit:	rts
; -------------------------------------------------------------------------------------------------
; $87f6
vtunes:	inc intro_flag
		lda #$00
		sta attract_ATARI				; NOT USED in Commodore - prevents Atari screen saver
		sta restart_flag
		jmp Ready1						; sub: print READY: and level fruits
; -------------------------------------------------------------------------------------------------
; $8801 calc new monster data pointer
SetMonsterDataEnd:
		sta spritedata_ptr
		inc spritedata_ptr+1
; $8805 Set last row, last byte of pacman
SetPacmanDataEnd:
		ldy #$0c						; last sprite line monsters/pacman
		ldx #$22						; last data byte monsters/pacman row 11 byte 2
		rts
; -------------------------------------------------------------------------------------------------
; $880a
vintro:	lda jiffy
		and #$03
		bne vsquit						; frequency change every 4 jiffy
; Play music
		ldx notes_counter
		cpx #$40
		beq vstart						; skip if end of frequency table
		lda NotesHi,x
!ifdef 	P500{
		ldy #SR_V1FREQ+1
		sta (SID),y						; SID voice 1 frequency hi
		dey
		lda NotesLo,x
		sta (SID),y						; SID voice 1 frequency lo
		lda #$21
		ldy #SR_V1CTRL
		sta (SID),y						; SID voice 1 control
} else{
		sta $d401						; SID voice 1 frequency hi
		lda NotesLo,x
		sta $d400						; SID voice 1 frequency lo
		lda #$21
		sta $d404						; SID voice 1 control
}
		inc notes_counter
		cpx #$28
		bne vsquit						; return if notes_counter not = $28
		jmp Ready2
; $8830
vstart:	inc ready_flag
vstart1:inc monster_status
		jsr ready3
; $ 8837
vready:	lda reset_flag
		beq vcontn
		cmp #$01
		bne reset2
		jsr vreset
		lda swap_player_flag
		bne vsquit
		lda game_over_flag
		bne vsquit
		inc reset_flag
		lda #$40
		sta reset_timer
		rts
; -------------------------------------------------------------------------------------------------
; $8851
reset2:	lda reset_timer
		beq vstart2
		dec reset_timer
		rts
vstart2:lda #$00
		sta reset_flag
		beq vstart1
vcontn:	jsr blink3
		lda fizzle_status
		beq vplayer
		jsr vfizzl
		jmp Fizzie
vplayer:jsr eyeonly
		lda freeze_flag
		beq vfruit
		jmp vgulpr
vfruit: jsr fruity
		jsr dottest
		lda rereck_flag
		bne vplyud
; collision check
!ifdef 	P500{
		ldy #VR_MOBMOB
		lda (VIC),y						; load VIC sprite-sprite collision reg
} else{
		lda $d01e						; load VIC sprite-sprite collision reg
}
		sta temp_collision
		and #$10
		beq vplyud
		ldx #$00						; start with sprite/monster 0
		ldy #$01
l888c:	lda monster_status,x
		asl
		bmi l88be
		tya
		bit temp_collision
		beq l88be
		lda pacman_hpos
		cmp monster_hpos,x
		bcs l88a4
		sec
		lda monster_hpos,x
		sbc pacman_hpos
		jmp l88a6
l88a4:	sbc monster_hpos,x
l88a6:	cmp #$04
		bcs l88be
		lda pacman_vpos
		cmp monster_vpos,x
		bcs l88b8
		sec
		lda monster_vpos,x
		sbc pacman_vpos
		jmp l88ba
l88b8:	sbc monster_vpos,x
l88ba:	cmp #$05
		bcc l88c9
l88be:	tya
		asl
		tay
		inx
		cpx #$04						; check if last monster
		bne l888c						; next monster
vplyud:	jmp l897b
l88c9:	lda monster_status,x
		bmi l88d0
		jmp l8968
l88d0:	lda #$42
		sta monster_status,x
		stx gulped_last
		inc gulp_count
		sec
		lda pacman_hpos
		sbc #$04
		sta sprite_x+4
		clc
		ldy #$02
-		adc #$02
		sta sprite_pacman_x,y
		dey
		bpl -
		adc #$02
		sta sprite_x,x
		lda #WHITE
!ifdef 	P500{
		stx temp
		ldy temp
		sta (VIC27),y					; set VIC sprite color = white (monster)
} else{
		sta $02c7						; ATARI color3 - not used on Commodore
		sta $d027,x						; set VIC sprite color = white (monster)
}
		lda pacman_vpos
		cmp monster_vpos,x
		beq l8909
		bcc l8904
		lda #$fe
		bmi l8906
l8904:	lda #$02
l8906:	clc
		adc pacman_vpos
l8909:	sta score_ptr1
		sta score_ptr2
		lda #$53
		sta $27
		txa
		clc
		adc $27
		adc #$01
		sta $29
		lda gulp_count
		asl
		tay
		lda BlueScorePointers,y
		sta pixel_get_ptr
		iny
		lda BlueScorePointers,y
		sta pixel_get_ptr+1
!ifdef 	P500{
		lda #GAMEBANK
		sta IndirectBank				; select bank 0 for pointer operations
}
		ldy #$0f
-		lda (pixel_get_ptr),y
		sta ($26),y
		lda BlueScore0,y
		sta ($28),y
		dey
		bpl -
!ifdef 	P500{
		lda #SYSTEMBANK
		sta IndirectBank				; switch back to bank 15
}
		lda #$00
!ifdef 	P500{
		ldy #SR_V2CTRL
		sta (SID),y						; SID voice 2 control = off
} else{
		sta $d40b						; SID voice 2 control = off
}
		inc freeze_flag
		lda #$84
		sta $a6
		lda #$02
		sta gulp_count1
		lda gulp_count
		bne l894d
		lda #$02
		bne l8963
l894d:	cmp #$01
		bne l8955
		lda #$04
		bne l8963
l8955:	cmp #$02
		bne l895d
		lda #$08
		bne l8963
l895d:	lda #$01
		sta player_score_text+2
		lda #$06
l8963:	sta player_score_text+3
		jmp l946d
l8968:	lda pacman_status
		ora #$80
		sta pacman_status
		jsr ClearAudio
		lda #$01
		sta monster_still_flag
		sta fizzle_status
		lda #$60
		sta fizzle_sequence_no
l897b:	lda fruit_display_flag
		beq l89da
		lda pacman_hpos
		cmp #$7c
		bne l89da
		lda pacman_vpos
		cmp #$84
		bne l89da
		ldx player_number
		lda maze_count1,x				; load maze number
		cmp #$0c
		bcc lowfsi
		lda #$0c						; limit to $0c
lowfsi:	tax
		lda FruitScoresIndex,x			; load index from table
		tax
		ldy #$00
l899c:	lda FruitScores,x
		sta GameScreen+$241,y			; fruit middle of screen
		inx
		iny
		cpy #$05
		bne l899c
		lda #$01
		sta fruit_score_flag
		lda #$40
		sta fruit_score_timer
		lda #$00
		sta fruit_display_flag
		sta fruit_timer
		sta fruit_timer+1
		lda #$01
		sta gobble_direction
		lda #$10
		sta gobble_frequency
		ldx player_number
		lda maze_count1,x
		cmp #$0c
		bcc l89ca
		lda #$0c
l89ca:	asl
		tax
		lda FruitScoreTable,x
		sta player_score_text+2
		inx
		lda FruitScoreTable,x
		sta player_score_text+3
		jsr l946d
l89da:	lda fizzle_status
		beq l89df
l89de:	rts
; -------------------------------------------------------------------------------------------------
; $89df
l89df:	jsr flitec
		lda freeze_flag
		bne l89de
		jsr veater
		jsr l8c0d
		jsr skirts
		lda pacman_adv_turning
		beq l89f7
		lda #$00
		beq l8a00
l89f7:	lda pacman_dly_eating
		bne l8a00
		ldx #$04
		jsr l95e0
l8a00:	sta pacman_motion_cnt
		lda #$00
		sta pacman_adv_turning
		jsr pmstik
		lda #$00
		sta pacman_dly_eating
		jsr Munchy
		lda jiffy
		and #$03
		bne l8a19
		jsr l97f5
l8a19:	jsr l9617
		lda pacman_adv_turning
		bne l8a77
		ldx #$03
; $8a22 speed handler for monsters
; entry: x reg = 0-3 for monsters 1-4
l8a22:	lda monster_status,x
		and #$7f
		beq nxmspd
		lda jiffy
		and #$07
		bne l8a34
		lda monster_timer,x
		beq l8a34
		dec monster_timer,x
l8a34:	lda monster_status,x
		and #$3f
		beq nxmspd
		lda monster_status,x
		asl
		bmi nxmspd
		lda monster_vpos,x
		cmp #$74
		bne spdblue
		lda monster_hpos,x
		cmp #$a7
		bcs spdslow
		cmp #$52
		bcc spdslow
spdblue:lda monster_status,x
		bpl l8a6a
spdslow:jsr l95e0
		cmp #$00
		bne nxmspd
		ldy monster_delay,x
		cpy #$01
		bne l8a64
		sta monster_delay,x
		beq l8a71
l8a64:	lda #$01
		sta monster_delay,x
		bne nxmspd
l8a6a:	jsr l95e0
		cmp #$00
		bne nxmspd
l8a71:	jsr l9845
nxmspd:	dex
		bpl l8a22
l8a77:	rts
; -------------------------------------------------------------------------------------------------
; 
vreset:	jsr blnkon
		lda swap_player_flag
		beq vreset1
		ldx reset_timer
		beq vswap1
		dec reset_timer
		rts
; -------------------------------------------------------------------------------------------------
; $8a86 swap player
vswap1:
		cmp #$01
		bne vswap2
;
		ldx #$00
lodp1lp:lda Player1Save,x				; restore player  screen
		sta Maze,x
		lda Player1Save+$100,x
		sta Maze+$100,x
		lda Player1Save+$200,x
		sta Maze+$200,x
		lda Player1Save+$300,x
		sta Maze+$300,x
		inx
		bne lodp1lp
;
		jsr Flash2On
		lda #$00						; swap to player 1
		sta player_number
		beq vswapx
vswap2:	cmp #$02
		bne vswap3
;
		ldx #$00
lodp2lp:lda Player2Save,x				; restore player 2 screen
		sta Maze,x
		lda Player2Save+$100,x
		sta Maze+$100,x
		lda Player2Save+$200,x
		sta Maze+$200,x
		lda Player2Save+$300,x
		sta Maze+$300,x
		inx
		bne lodp2lp
;
		jsr Flash1On
		lda #$01						; swap to player 2
		sta player_number
		bne vswapx
vswap3:	jsr blnkon
		lda #$00
		sta swap_player_flag
		beq rset1pg
vswapx:	lda #$03
vswapx1:sta swap_player_flag
		rts
; -------------------------------------------------------------------------------------------------
; $8ae8
vreset1:lda players
		beq rset1pg
		lda player_number
		bne rset2pg
		lda extra_pacman2
		beq rset1pg						; player 2 dead
		lda extra_pacman1
		bne swap12						; go swap players
		lda #$02
		sta swap_player_flag			; show player 1 game over
		lda #$30
		sta reset_timer
		bne vgmend
swap12:	jsr SaveScreenPlayer1			; sub: init game screen player1 $4400
		lda #$30
		sta reset_timer
		lda #$02
		bne vswapx1
rset2pg:lda extra_pacman1
		beq rset1pg						; player 1 dead
		lda extra_pacman2
		bne swap21						; go swap players
		lda #$01
		sta swap_player_flag			; show player 2 game over
		lda #$30
		sta reset_timer
		bne vgmend
swap21:	jsr SaveScreenPlayer2			; sub: init game screen player2 $4800
		lda #$30
		sta reset_timer
		lda #$01
		sta swap_player_flag
vggone:	rts
; -------------------------------------------------------------------------------------------------
; $8b2b
rset1pg:ldx player_number
		lda extra_pacman1,x
		beq vgmend						; game is over
		jsr Setup						; sub: init new game
		jsr Ready1						; sub: print READY: and difficulty fruits
		jmp Ready2
vgmend:	lda #$2c
		ldx #$00
l8b3e:	sta GameScreen+$23d,x
		clc
		adc #$01
		inx
		cpx #$0e
		bne l8b3e
		lda #RED
!ifdef 	P500{
		ldy #VR_MOBCOL+4
		sta (VIC),y						; set VIC sprite 4 color = red (pacman)
} else{
		sta $02c7						; ATARI color3 - not used on Commodore
		sta $d02b						; set VIC sprite 4 color = red (pacman)
}
		lda swap_player_flag
		bne vggone
		ldx #$2a						; score 1 screen position
		jsr CheckHighscore
		ldx #$47						; score 2 screen position
		jsr CheckHighscore
		lda #$00
		sta reset_flag
		lda #$01
		sta game_over_flag
		lda #$e2
		sta atract_timer_ATARI			; set attract timer start value - only ATARI
		jsr Flash1On
		jmp blnkon
; $8b71 check for new highsore
CheckHighscore:
		ldy #$00
chkhslp:lda GameScreen,x
		cmp GameScreen+$39,y			; compare last player score with highscore on screen
		beq chnxdig						; didit equal, next digit
		bcc chnonew						; digit lower = no highscore -> return
		bcs chnewlp						; new highscore
chnxdig:inx
		iny
		cpy #$06
		bne chkhslp
		rts
; -------------------------------------------------------------------------------------------------
; $8b86	copy new highscore to screen
chnewlp:lda GameScreen,x
		sta GameScreen+$39,y
		inx
		iny
		cpy #$06
		bne chnewlp						; copy 6 digits
!ifdef 	P500{
		ldx #$09
-		lda Text_HighScore,x
		sta GameScreen+$0f,x
		dex
		bpl -
}
chnonew:rts
; -------------------------------------------------------------------------------------------------
; $8b93 tunnel logic
Tunnel:
		ldx #$04
l8b95:	lda monster_vpos,x
		cmp #$74
		bne l8b9e
		jsr l8ba2
l8b9e:	dex
		bpl l8b95
		rts
; -------------------------------------------------------------------------------------------------
; $8ba2
l8ba2:	sta tunnel_logic
		txa
		cmp #$04
		bne tunply
		lda #$ff
tunply:	clc
		adc #$54
		sta tunnel_logic+1
		lda #$ff
		sta tunnel_bitmask
		lda monster_hpos,x
		sta tunnel_iterat_cnt
		cmp #$c0
		bcc l8be2
		lda #$c0
l8bbe:	cmp tunnel_iterat_cnt
		beq l8c0c
		dec tunnel_iterat_cnt
		asl tunnel_bitmask
		bcs l8bbe
		lda monster_hpos,x
		cmp #$ca
		bcc l8c0c
		lda monster_direction,x
		cmp #$08
		bne l8c0c
		lda #$2a
		sta monster_hpos,x
		cpx #$04
		bne l8c0c
		dec pacman_screen_ptr+1
		lda #$df
		bne l8c0a
l8be2:	cmp #$39
		bcs l8c0c
l8be6:	lda #$38
		cmp tunnel_iterat_cnt
		beq l8c0c
		inc tunnel_iterat_cnt
		lsr tunnel_bitmask
		bcs l8be6
		lda monster_hpos,x
		cmp #$2a
		bne l8c0c
		lda monster_direction,x
		cmp #$04
		bne l8c0c
		lda #$ca
		sta monster_hpos,x
		cpx #$04
		bne l8c0c
		inc pacman_screen_ptr+1
		lda #$07
l8c0a:	sta pacman_screen_ptr
l8c0c:	rts
; -------------------------------------------------------------------------------------------------
; $8c0d Pacman game subroutines
l8c0d:	ldx gobble_direction
		beq l8c48
		lda gobble_frequency
		ldy #$21
		cpx #$07
		bcs l8c28
		cmp #$0b
		beq l8c22
		sec
		sbc #$01
		bne l8c40
l8c22:	inc gobble_direction
		ldy #$21
		bne l8c40
l8c28:	cpx #$09
		bne l8c30
		inc gobble_direction
		bne l8c32
l8c30:	ldy #$21
l8c32:	clc
		cmp #$10
		bne l8c3e
		lda #$00
		sta gobble_direction
		tay
		beq l8c40						; voice 1 = off
l8c3e:	adc #$01
l8c40:	sta gobble_frequency
!ifdef 	P500{
		sty temp
		ldy #SR_V1FREQ+1
		sta (SID),y						; set SID voice 1 frequency hi
		lda temp
		ldy #SR_V1CTRL
		sta (SID),y						; SID voice 1 control
} else{
		sta $d401						; set SID voice 1 frequency hi
		sty $d404						; SID voice 1 control
}
l8c48:	rts
; -------------------------------------------------------------------------------------------------
; $8c49
skirts:	lda jiffy
		and #$0f
		bne l8c5b
		lda monster_skirt_flag
		beq l8c59
		lda #$00
		sta monster_skirt_flag
		beq l8c5b
l8c59:	inc monster_skirt_flag
l8c5b:	rts
; -------------------------------------------------------------------------------------------------
; $8c5c rerack will reset maze after a player has cleared all dots
rerack:	lda rerack_sequence
		bne testrrs
		jsr ClearAudio
		jsr Drawit
		lda #$40
rerkxx1:sta rerack_timer
rerkxx2:inc rerack_sequence
		rts
; $l8c6
testrrs:cmp #$01
		bne testrr2
		lda rerack_timer
		bne drrwtm						; decrease rerack wait timer				
		lda #$00
		ldx #$03
tsrr1lp:sta sprite_x,x
		dex
		bpl tsrr1lp
		lda #LIGHTBLUE
!ifdef 	P500{
		ldy #VR_BGRCOL+1
		sta (VIC),y						; set VIC backgroundcolor 1 = lightblue					
} else{
		sta $d022						; set VIC backgroundcolor 1 = lightblue					
}
		lda #$07
		sta rerack_flash_count
		lda #$10
		bne rerkxx1
drrwtm:	dec rerack_timer
		rts
; $8c8f
testrr2:cmp #$02
		bne testrr3
		lda rerack_timer
		bne drrwtm						; decrease rerack color timer
		dec rerack_flash_count
		beq rerkxx2
		lda rerack_flash_count
		clc
		lsr
		bcc altblue
		lda #LIGHTBLUE
		bne setrrc
altblue:lda #WHITE
setrrc:
!ifdef 	P500{
		ldy #VR_BGRCOL+1
		sta (VIC),y						; set VIC backgroundcolor 1 = white					
} else{
		sta $d022						; set VIC backgroundcolor 1 = white
}
		lda #$10
		sta rerack_timer
		rts
; $8caf
testrr3:cmp #$03
		bne testrr4
		jsr Setup						; sub: init new game
		inc rerack_sequence
		rts
; $8cb9
testrr4:jsr newbrd
		ldx player_number
		inc extra_pacman1,x
		inc maze_count1,x
		jsr Ready1						; sub: print READY: and difficulty fruits
		jsr Ready2
		lda #$00
		sta rereck_flag
		sta rerack_sequence
		lda #$02
		sta reset_flag
		lda #$40
		sta reset_timer
		rts
; -------------------------------------------------------------------------------------------------
; $8cd7 Get ready to play: Print READY: and difficulty fruits
Ready1:	lda #$22						; first READY: char ( $22-$2b )
		ldx #$00
readylp:sta GameScreen+$23f,x			; screen position for READY:
		clc
		adc #$01
		inx
		cpx #$0a						; 10 chars
		bne readylp
		ldx player_number
		lda maze_count1,x				; load player difficulty
		cmp #$06
		bcc setred
		cmp #$0a
		bcs setred
		ldy #LIGHTGREEN
		bne setfrc						; set up for green fruits
setred:	ldy #LIGHTRED
setfrc:	sty fruit_color					; ATARI - only stored, not used on Commodore
		ldy #$00
!ifdef 	P500{				; Y already $00
		sty IndirectBank				; select bank 0 for pointer operations
}
		cmp #$06
		bcs hfruits						; branch if A >= $06
		sta temp
		lda #$e2
		sta pixel_put_ptr
		lda #$07						; pixel_put_ptr = fruit screen position $07e2
		sta pixel_put_ptr+1
		ldx #$00
!ifdef 	P500{				; X already $00
		stx IndirectBank				; select bank 0 for pointer operations
}
fruitlp:lda FruitChars,x				; load fruit char from table
		sta (pixel_put_ptr),y			; store fruit code to screen
		inc pixel_put_ptr				; screen pointer to second char
		clc
		adc #$01						; add 1 to char code for second fruit char
		sta (pixel_put_ptr),y
		cpx temp
		beq fsplit
		inx
		dec pixel_put_ptr				; next fruit position to the left
		dec pixel_put_ptr
		dec pixel_put_ptr
		bne fruitlp
hfruits:cmp #$12
		bcc hfruit1
		lda #$12						; A = max $12
hfruit1:sec
		sbc #$06						; substract $06 -> value 0 - $0c
		sta temp
		sec
		lda #<(HighFruitChars)			; = $8a
		sbc temp
		sta pixel_get_ptr
		lda #>(HighFruitChars)			; pointer to end of table = $9d8a (min -$0c)
		sbc #$00
		sta pixel_get_ptr+1
		ldx #$00
hfruitl:	lda (pixel_get_ptr),y
		sta GameScreen+$3d6,x
		clc
		adc #$01
		sta GameScreen+$3d7,x
		inx
		inx
		iny
		cpy #$07
		bne hfruitl
fsplit:
!ifdef 	P500{
		lda #SYSTEMBANK
		sta IndirectBank				; switch back to bank 15
}
		rts
; -------------------------------------------------------------------------------------------------
; $8d52
Ready2:
		jsr Drawit
		ldx player_number
		dec extra_pacman1,x				; decrease lives of actual player
; $8d59 Update extra pacmans
UpdateExtraPacs:
		ldx player_number
		lda extra_pacman1,x
		ldx #$00						; char $00 = <space>
		ldy #$1b						; Char $1b = mini pacman	
		cmp #$03
		bne twopac						; branch if not 3 lives
		sty GameScreen+$3c8				; write mini-pacmans
udxpac2:sty GameScreen+$3c6
udxpac1:sty GameScreen+$3c4
		rts
twopac:	cmp #$02
		bne onepac						; branch if not 2 lives
		jsr udnpac2						; clear 3. mini-pacman
		jmp udxpac2						; write 2 mini-pacmans
onepac:	cmp #$01						; branch if not 1 live = dead
		bne nopacs						; dead -> clear all mini-pacmans
		jsr udnpac1						; clear 2.+3. mini-pacman
		jmp udxpac1						; write 1 mini-pacman
nopacs:	stx GameScreen+$3c4
udnpac1:stx GameScreen+$3c6
udnpac2:stx GameScreen+$3c8
		rts
; -------------------------------------------------------------------------------------------------
; $8d8d
Drawit:
		lda #$01
		sta monster_still_flag
		ldx #$03
greadl:	lda monster_direction,x
		jsr monhnd
		dex
		bpl greadl
		lda #$00
		sta monster_still_flag
		jmp pacstp
ready3:	ldx #$0d
		lda #$00
redy3lp:sta GameScreen+$23d,x
		dex
		bpl redy3lp
		rts
; -------------------------------------------------------------------------------------------------
; $8dad blink 1/2up of active player
FlashXUp:
		lda jiffy
		and #$0f
		bne flashxx						; blink only every 16. jiffy cycle
		lda flash_xup_timer
		bne flshres						; branch if blink = 1 -> set to 0
		inc flash_xup_timer					; increase blink
		bne flasher						; branch always
flshres:lda #$00
		sta flash_xup_timer					; set flash counter to 0
flasher	lda player_number
		beq flspl1						; branch to 1 player
		lda flash_xup_timer
		bne Flash2On					; if blink counter > 0 write 2UP
		tax								; code 0 = <space>
		tay
		beq fl2stor
; $8dcb write 2UP
Flash2On:
		lda #$92						; code 2, U, P
		ldx #$b5
		ldy #$b0
fl2stor:sta GameScreen+$21				; write to screen right side
		stx GameScreen+$22
		sty GameScreen+$23
		rts
; $8ddb
flspl1:	lda flash_xup_timer
		bne Flash1On					; if flash_xup_timer > 0 write 1UP
		tax								; code 0 = <space>
		tay
		beq fl1stor						; clear 1UP on screen
; $8de3 write 1UP
Flash1On:
		lda #$91						; code 1, U, P
		ldx #$b5
		ldy #$b0
fl1stor:sta GameScreen+$04				; write to screen left side
		stx GameScreen+$05
		sty GameScreen+$06
flashxx:rts
; -------------------------------------------------------------------------------------------------
; $8df3 Set up monster and pacman start postions
Setup:
		jsr InitSpriteMemory			; clear sprite areas at $3000, sprite data $5300
		ldx #$8a
clrpgz:	sta $3b,x						; clear ZP $3c - $c5
		dex
		bne clrpgz
		jsr SetColor					; sub: init color RAM and VIC Sprite colors
; speed initialization
		ldx player_number
		lda maze_count1,x				; load player difficulty
		cmp #$06
		bcc lowinit
		lda #$06						; max speed level 6
lowinit	tay
		lda PacmanSpeedTable,y			; load pacman speed index from table
		tax
		lda Speed,x						; load speed from table
		sta pacman_speed_count			; store it
		lda MonsterSpeedTable,y			; load monster speed index from table
		tay
		ldx #$03						; setup 4 monsters 
spinilp:lda Speed,y						; load speed from table
		sta monster_speed_cnt,x			; store it
		dex
		bpl spinilp						; next monster
		ldx #$13
indatlp:lda SpriteInitData,x
		sta pacman_screen_ptr,x
		dex
		bpl indatlp
		ldy #$00
		jsr SetMonsterTimer
!ifdef 	P500{
		ldy #VR_MOBMOB
		lda (VIC),y						; VIC clear sprite-sprite collision
		iny
		lda (VIC),y						; VIC clear sprite-foreground collision
} else{
		lda $d01e						; VIC clear sprite-sprite collision
		lda $d01f						; VIC clear sprite-foreground collision
}
		rts
; -------------------------------------------------------------------------------------------------
; $8e38 Init game variables: lives, level, score...
InitGameVariables:
		lda #LIVES						; start with 3 lives
		sta extra_pacman1
		sta extra_pacman2
		lda difficulty
		sta maze_count1
		sta maze_count2
		ldx #$01
		jsr newbrd1						; branch to zero score
newbrd:	jsr InitGameScreen				; sub: copy game screen to screen RAM
		jsr UpdateExtraPacs				; sub: Update extra pacmans
		ldx player_number
		lda maze_count1,x
		tay
		bne newrek2
		lda jiffy
		bpl newrek2
newrek1:jsr SetMonsterTimer
		jmp newbrd0
newrek2:iny
		bne newrek1
newbrd0:ldx player_number
newbrd1:lda #$0f						; zero score
		sta bigdot_status,x
		lda #$00
		sta fruit_counter,x
		sta dots_eaten_lo,x
		sta dots_eaten_hi,x
		rts
; -------------------------------------------------------------------------------------------------
; $8e72 copies data from table to $87-$89
SetMonsterTimer:
		cpy #$03
		bcc ldmstmr
		ldy #$03
ldmstmr:ldx #$02
ldmstlp:lda BlueStartValues,y
		sta monster_timer+1,x
		iny
		dex
		bpl ldmstlp
		rts
; -------------------------------------------------------------------------------------------------
; $8e84
vfizzl:	lda fizzle_status
		cmp #$01
		bne l8ea2
		lda fizzle_sequence_no
		beq l8e9e
		jsr skirts
		ldx #$03
l8e93:	lda monster_direction,x
		jsr monhnd
		dex
		bpl l8e93
		dec fizzle_sequence_no
		rts
; -------------------------------------------------------------------------------------------------
; $8e9e
l8e9e:	inc fizzle_status
		lda fizzle_status
l8ea2:	cmp #$02
		bne l8ec9
		ldx #$03
		lda #$00
l8eaa:	sta sprite_x,x
		dex
		bpl l8eaa
		lda #$25
		sta fizzle_frequency
		sta fizzle_freq_base
		jsr ClearAudio
		sta fizzle_counter
		sta fizzle_ptr
		inc fizzle_flag
		lda #$07
		sta fizzle_timer
		lda #$03
		sta fizzle_status
		bne l8ed9
l8ec9:	cmp #$03
		beq l8ed9
		cmp #$04
		beq l8ef2
		cmp #$05
		beq l8f17
		cmp #$06
		beq l8f34
l8ed9:	lda #$21
!ifdef 	P500{
		ldy #SR_V1CTRL
		sta (SID),y						; SID voice 1 control = sawtooth, on
		lda fizzle_frequency
		ldy #SR_V1FREQ+1
		sta (SID),y						; set SID voice 1 frequency hi
} else{
		sta $d404						; SID voice 1 control = sawtooth, on
		lda fizzle_frequency
		sta $d401						; set SID voice 1 frequency hi
}
		sec
		sbc #$02
		sta fizzle_frequency
		inc fizzle_counter
		lda fizzle_counter
		cmp #$04
		bne l8f33
		beq l8f14
l8ef2:	lda #$21
!ifdef 	P500{
		ldy #SR_V1CTRL
		sta (SID),y						; SID voice 1 control = sawtooth, on
		lda fizzle_frequency
		ldy #SR_V1FREQ+1
		sta (SID),y						; set SID voice 1 frequency hi
} else{
		sta $d404						; SID voice 1 control = sawtooth, on
		lda fizzle_frequency
		sta $d401						; set SID voice 1 frequency hi
}
		clc
		adc #$02
		sta fizzle_frequency
		dec fizzle_counter
		bne l8f33
		lda fizzle_freq_base
		cmp #$13
		beq l8f31
		sec
		sbc #$02
		sta fizzle_freq_base
		sta fizzle_frequency
		lda #$03
l8f14:	sta fizzle_status
		rts
; -------------------------------------------------------------------------------------------------
; $8f17
l8f17:	lda #$21
!ifdef 	P500{
		ldy #SR_V1CTRL
		sta (SID),y						; SID voice 1 control = sawtooth, on
		lda fizzle_freq_base
		ldy #SR_V1FREQ+1
		sta (SID),y						; set SID voice 1 frequency hi
} else{
		sta $d404						; SID voice 1 control = sawtooth, on
		lda fizzle_freq_base
		sta $d401						; set SID voice 1 frequency hi
}
		clc
		adc #$02
		sta fizzle_freq_base
		cmp #$35
		bne l8f33
		jsr ClearAudio
		lda #$80
		sta fizzle_counter
l8f31:	inc fizzle_status
l8f33:	rts
; -------------------------------------------------------------------------------------------------
; $8f34
l8f34:	lda fizzle_counter
		bne l8f3b
		inc reset_flag
		rts
; -------------------------------------------------------------------------------------------------
; $8f3b
l8f3b:	dec fizzle_counter
		rts
; -------------------------------------------------------------------------------------------------
; $8f3e Fizzie will draw the pacman folding up in sequence
Fizzie:	
		lda fizzle_flag
		beq fizziex
		lda pacman_vpos
		sta pixel_put_ptr
		lda #$53
		sta pixel_put_ptr+1
		lda fizzle_timer
		beq l8f52
		dec fizzle_timer
		bpl l8f5e
l8f52:	lda fizzle_ptr
		cmp #$0f
		beq l8f8f
		inc fizzle_ptr
		lda #$05
		sta fizzle_timer
l8f5e:	lda fizzle_ptr
		beq l8f73
		cmp #$0f
		beq l8f81
		tax
		dex
		lda FizzieIndex,x
		tay
		dey
!ifdef 	P500{
		lda #GAMEBANK
		sta IndirectBank				; select bank 0 for pointer operations
}
		lda FizzieData,x
		sta (pixel_put_ptr),y
!ifdef 	P500{
		lda #SYSTEMBANK
		sta IndirectBank				; switch back to bank 15
}
		rts
; $8f73
l8f73:	ldy #$0c			; already bank 0 selected
		ldx #$09
!ifdef 	P500{
		lda #GAMEBANK
		sta IndirectBank				; select bank 0 for pointer operations
}
l8f77:	lda PacmanDie,x
		sta (pixel_put_ptr),y
		dey
		dex
		bpl l8f77
!ifdef 	P500{
		lda #SYSTEMBANK
		sta IndirectBank				; switch back to bank 15
}
		rts
; $8f81
l8f81:	ldy #$0f			; already bank 0 selected
		ldx #$0f
!ifdef 	P500{
		lda #GAMEBANK
		sta IndirectBank				; select bank 0 for pointer operations
}
l8f85:	lda PacmanExplosion,x
		sta (pixel_put_ptr),y
		dey
		dex
		bpl l8f85
!ifdef 	P500{
		lda #SYSTEMBANK
		sta IndirectBank				; switch back to bank 15
}
		rts
; $8f8f
l8f8f:	ldy #$0f			; already bank 0 selected
		lda #$00
!ifdef 	P500{
		lda #GAMEBANK
		sta IndirectBank				; select bank 0 for pointer operations
}
l8f93:	sta (pixel_put_ptr),y
		dey
		bpl l8f93
		sta fizzle_flag
!ifdef 	P500{
		lda #SYSTEMBANK
		sta IndirectBank				; switch back to bank 15
}
fizziex:	rts
; -------------------------------------------------------------------------------------------------
; $8f9b
flitec:
		lda flash_count
		beq noflit
		lda tweet_sound_flag
		bne chkfltm
		jsr vrverb
chkfltm:lda flight_timer
		beq flashsq
		dec flight_timer
		jmp setflc
noflit:	lda tweet_sound_flag
		bne fizziex
		jmp vchase
flashsq:ldx player_number
		lda maze_count1,x
		tax
		lda FlashingTimerTable,x
		cmp flash_count
		bne l8ff9
		ldx #$03						; 3-0 monsters
!ifdef 	P500{
		ldy #VR_MOBCOL+3
l8fc4:	lda monster_status,x
		bpl l8fce
		lda SpriteColors,x
		sta (VIC),y						; set VIC sprite color from table
		dey
} else{
l8fc4:	lda monster_status,x
		bpl l8fce
		lda SpriteColors,x
		sta $d027,x						; set VIC sprite color from table
}
l8fce:	dex
		bpl l8fc4
		ldx #$03
l8fd3:	lda monster_status,x
		bpl l8fe9
		cmp #$80
		beq l8fe1
		and #$bb
		beq l8fe9
		bne l8fe5
l8fe1:	lda #$00
		beq l8fe7
l8fe5:	lda #$20
l8fe7:	sta monster_status,x
l8fe9:	dex
		bpl l8fd3
		lda #$00
		sta flash_count
		sta flash_timer
		sta flight_sound_start
		lda #$a0
		sta chase_timer
		rts
; -------------------------------------------------------------------------------------------------
; $8ff9 Toggle monster color blue/white
l8ff9:	lda flash_timer
		bne l9003
		inc flash_count
		lda #$18
		sta flash_timer
l9003:	dec flash_timer
setflc:	lda flash_count
		lsr
		bcc mwhite						; color monsters white
!ifdef 	P500{
		ldx #BLUE
		bne +							; blue loaded, skip white
mwhite: ldx #WHITE
+		ldy #$03						; 3-0 monsters
mcolrlp:lda monster_status,y
		bpl mskip						; skip reborn monster (bit#7 = 0)
		txa
		sta (VIC27),y					; set VIC monster sprites color 3-0
mskip:	dey
} else{
		ldy #BLUE
		bne +							; blue loaded, skip white
mwhite: ldy #WHITE
+		ldx #$03						; 3-0 monsters
mcolrlp:lda monster_status,x
		bpl mskip						; skip reborn monster
		tya
		sta $d027,x						; set VIC monster sprites color 3-0
mskip:	dex
}
		bpl mcolrlp
		rts
; -------------------------------------------------------------------------------------------------
; $901e
vrverb:	lda flight_sound_start
		bne l9030
		lda #$05
		sta flight_volume
		lda #$02
		sta flight_sound_dir
		inc flight_sound_start
		lda #$0e
		sta flight_sound_freq
l9030:	lda flight_sound_dir
		cmp #$01
		bne l9055
		lda flight_sound_freq
		cmp #$0e
		bne l904e
		lda flight_volume
		cmp #$03
		bne l9046
		lda #$05
		sta flight_volume
l9046:	dec flight_volume
		lda #$02
		sta flight_sound_dir
		bne l9061
l904e:	sec
		lda flight_sound_freq
		sbc #$03
		bne l9066
l9055:	lda flight_sound_freq
		cmp #$20
		bne l9061
		lda #$01
		sta flight_sound_dir
		bne l904e
l9061:	clc
		lda flight_sound_freq
		adc #$03
l9066:	sta flight_sound_freq
!ifdef 	P500{
		ldy #SR_V2FREQ+1
		sta (SID),y						; SID voice 2 frequency hi
		lda #$21
l906d:	ldy #SR_V2CTRL
		sta (SID),y						; SID voice 2 control = sawtooth, on
} else{
		sta $d408						; SID voice 2 frequency hi
		lda #$21
l906d:	sta $d40b						; SID voice 2 control = sawtooth, on
}
		rts
; -------------------------------------------------------------------------------------------------
; $9071 Gulp = Eat monster
vtweet:	lda tweet_sound_freq
		bne l9079
		lda #$97
		bne l907f
l9079:	cmp #$64
		bne l907f
		lda #$97
l907f:	sec
		sbc #$03
		sta tweet_sound_freq
!ifdef 	P500{
		ldy #SR_V2FREQ+1
		sta (SID),y						; SID voice 2 frequency hi
} else{
		sta $d408						; SID voice 2 frequency hi
}
		lda #$21
		bne l906d
;
vgulpr:	dec gulp_count1
		beq distrt
		sec
		lda gulp_count2
		sbc #$04
		sta gulp_count2
		cmp #$10
		beq gulpoff
!ifdef 	P500{
		ldy #SR_V1FREQ+1
		sta (SID),y						; SID voice 1 frequency hi
		ldy #SR_V2FREQ+1
		sta (SID),y						; SID voice 2 frequency hi
} else{
		sta $d401						; SID voice 1 frequency hi
		sta $d408						; SID voice 2 frequency hi
}
		lda #$21
		bne gbranch
distrt:	lda #$02
		sta gulp_count1
		sec
		lda gulp_count2
		sbc #$03
		sta gulp_count2
!ifdef 	P500{
		ldy #SR_V1FREQ+1
		sta (SID),y						; SID voice 1 frequency hi
		ldy #SR_V2FREQ+1
		sta (SID),y						; SID voice 2 frequency hi
		lda #$21
gbranch:ldy #SR_V2CTRL
		sta (SID),y						; SID voice 2 control = sawtooth, on
} else{
		sta $d401						; SID voice 1 frequency hi
		sta $d408						; SID voice 2 frequency hi
		lda #$21
gbranch:sta $d40b						; SID voice 2 control = sawtooth, on
}
		bne veatrs
gulpoff:jsr ClearAudio			; returns with Y=$ff
		sta freeze_flag
		sta pacman_motion_cnt
		sty gulped_last
		lda #$0f
		sta $02c7
!ifdef 	P500{
		ldy #$03						; 3-0 monsters
rsetpcl:lda (VIC27),y					; load VIC sprites color 3-0 (monsters)
		cmp #$f1
		beq rsetplc
		dey
} else{
		ldx #$03						; 3-0 monsters
rsetpcl:lda $d027,x						; load VIC sprites color 3-0 (monsters)
		cmp #$f1
		beq rsetplc
		dex
}
		bpl rsetpcl
		rts
rsetplc:lda #$00
		sta pacman_motion_cnt
		jsr pmstik
		inc pacman_adv_turning
		jmp Munchy
; -------------------------------------------------------------------------------------------------
; $90e3 Eat dot sound
veater:	lda eatdot_sound_flag
		beq l910c
		ldx eatdot_sound_cnt
		cpx #$06
		bne l90f5
		lda #$00
		sta eatdot_sound_flag
		sta eatdot_sound_cnt
		beq veatrs
l90f5:	lda eatdot_sound_togg
		bne l90ff
		lda EatingDotsSoundData1,x
		jmp l9102
l90ff:	lda EatingDotsSoundData2,x
l9102:	inc eatdot_sound_cnt
!ifdef 	P500{
		ldy #SR_V1FREQ+1
		sta (SID),y						; SID voice 1 frequency hi
		lda #$21
veatrs:	ldy #SR_V1CTRL
		sta (SID),y						; SID voice 1 control = sawtooth, on
} else{
		sta $d401						; SID voice 1 frequency hi
		lda #$21
veatrs:	sta $d404						; SID voice 1 control = sawtooth, on
}
l910c:	rts
; -------------------------------------------------------------------------------------------------
; $910d Stop SID sound
ClearAudio:
!ifdef 	P500{
		lda #$88
		ldy #SR_MODVOL
		sta (SID),y						; SID mode to 3OFF, Volume = 8
		lda #$00
		ldy #SR_V1CTRL
		sta (SID),y						; SID voice 1 control = off
		ldy #SR_V2CTRL
		sta (SID),y						; SID voice 2 control = off
		ldy #$ff
		rts
} else{
		lda #$88
		sta $d418						; SID mode to 3OFF, Volume = 8
		lda #$00
		sta $d404						; SID voice 1 control = off
		sta $d40b						; SID voice 2 control = off
		ldy #$ff
		rts
}
; -------------------------------------------------------------------------------------------------
; $911d initialize sprite areas $3000-$31ff (VIC sprite data) and $5300-$57ff (Sprite data source)
InitSpriteMemory:
		ldx #$00
		txa
inipmlp:sta SpriteData,x
		sta SpriteData+$100,x
		sta SpriteRAM,x
		sta SpriteRAM+$100,x
		sta SpriteRAM+$200,x
		sta SpriteRAM+$300,x
		sta SpriteRAM+$400,x
		inx
		bne inipmlp
		txa
		rts
; -------------------------------------------------------------------------------------------------
; $913a Initialize color RAM + Sprite colors
SetColor:
!ifdef 	P500{
		lda #YELLOW+MCM
		ldy #$00
collp1:	sta (ColorRAM0),y				; init color RAM with yellow + bit#3 for multicolor
		sta (ColorRAM1),y
		sta (ColorRAM2),y
		sta (ColorRAM3),y
		dey
		bne collp1
		ldy #40*2 - 1
		lda #WHITE
collp2:sta (ColorRAM0),y				; init lines 0-1 with white
		dey
		bpl collp2
		ldy #7							; sprite 7-0
collp3:lda SpriteColors,y
		sta (VIC27),y					; init VIC Sprite colors from table
		dey
		bpl collp3
		rts
} else{
		ldx #$00
		lda #YELLOW+MCM
collp1:	sta ColorRAM64,x				; init color RAM with yellow + bit#3 for multicolor
		sta ColorRAM64+$100,x
		sta ColorRAM64+$200,x
		sta ColorRAM64+$300,x
		dex
		bne collp1
		ldx #40*2 - 1
		lda #WHITE
collp2:	sta ColorRAM64,x				; init lines 0-1 with white
		dex
		bpl collp2
		ldx #7							; sprite 7-0
collp3:	lda SpriteColors,x
		sta $d027,x						; init VIC Sprite colors from table
		dex
		bpl collp3
		rts
}
; -------------------------------------------------------------------------------------------------
; $9163 copy game screen to screen RAM
InitGameScreen:
		ldx #$00
inigslp:lda MazeData,x					; load decompressed Screen Data
		sta Maze,x					; copy to game screen from line2
		lda MazeData+$100,x
		sta Maze+$100,x
		lda MazeData+$200,x
		sta Maze+$200,x
		lda MazeData+$300,x
		sta Maze+$300,x
		inx
		bne inigslp
		rts
; -------------------------------------------------------------------------------------------------
; $9181 save game screen player 1 to $4400
SaveScreenPlayer1:
		ldx #$00
savp1lp:lda Maze,x
		sta Player1Save,x
		lda Maze+$100,x
		sta Player1Save+$100,x
		lda Maze+$200,x
		sta Player1Save+$200,x
		lda Maze+$300,x
		sta Player1Save+$300,x
		inx
		bne savp1lp
		rts
; -------------------------------------------------------------------------------------------------
; $919f save game screen player 2 to $4800
SaveScreenPlayer2:
		ldx #$00
savp2lp:lda Maze,x
		sta Player2Save,x
		lda Maze+$100,x
		sta Player2Save+$100,x
		lda Maze+$200,x
		sta Player2Save+$200,x
		lda Maze+$300,x
		sta Player2Save+$300,x
		inx
		bne savp2lp
sav2px:	rts
; -------------------------------------------------------------------------------------------------
; $91bd This subroutine will test joytick input and determine if direction chosen is valid.
; the pacman will then move in the proper direction with it's mouth opening and closing.
; this code is called during vblank and initiates motion during alternate occurances of vblank.
; mouth animation is performed every vblank.
pmstik:	lda pacman_status
		bmi sav2px
		ldx #$04
		jsr MazeHandler
		clc
		lda pacman_new_dir				; see if we change direction
		bit temp						; is it valid ?
		beq l91e3						; no
		cmp pacman_direction
		beq l91df
		ora pacman_direction
		tay
		and #$03
		beq l91df
		tya
		and #$0c
		beq l91df
		sty pacman_adv_turning
l91df:	lda pacman_new_dir
		sta pacman_direction
l91e3:	ldx player_number
!ifdef 	P500{
		ldy #$01
		lda (CIA),y						; load CIA port b bit#0-3 = joystick 1 movement
} else{
		lda $dc00						; load CIA port a bit#0-3 = joystick 2 movement
}
		and #$0f
		eor #$0f
		beq l920a
		pha
		ldx #$03
		ldy #$00
l91f3:	clc
		lsr
		bcc l91f8
		iny
l91f8:	dex
		bpl l91f3
		pla
		cpy #$02
		beq l920a
		sta attract_ATARI				; NOT USED in Commodore - prevents Atari screen saver
		sta pacman_new_dir
		bit temp
		beq l920a
		sta pacman_direction
l920a:	lda pacman_direction
		bit temp
		bne l9213
		jmp l92f4
l9213:	cmp #$01
		beq l9229
		cmp #$02
		beq l9257
		cmp #$04
		bne l9222
		jmp l92b9
l9222:	cmp #$08
		beq l9285
		jmp pacstp
l9229:	lda pacman_motion_cnt
		bne l9252
		dec pacman_vpos
		dec pacman_vpos
		lda pacman_vmap_count
		bne l923b
		lda #$03
		sta pacman_vmap_count
		bne l9252
l923b:	dec pacman_vmap_count
		bne l9252
		lda pacman_vpos
		cmp pacman_vpos_save
		beq l9252
		sec
		lda pacman_screen_ptr
		sbc #$28
		sta pacman_screen_ptr
		lda pacman_screen_ptr+1
		sbc #$00
		sta pacman_screen_ptr+1
l9252:	ldy #$06						; point to pac top
		jmp l931d
l9257:	lda pacman_motion_cnt
		bne l9280
		inc pacman_vpos
		inc pacman_vpos
		lda pacman_vmap_count
		cmp #$03
		bne l927e
		lda #$00
		sta pacman_vmap_count
		lda pacman_vpos
		cmp pacman_vpos_save
		beq l9280
		clc
		lda pacman_screen_ptr
		adc #$28
		sta pacman_screen_ptr
		lda pacman_screen_ptr+1
		adc #$00
		sta pacman_screen_ptr+1
		bne l9280
l927e:	inc pacman_vmap_count
l9280:	ldy #$08						; point to pac bottom
		jmp l931d
l9285:	lda pacman_motion_cnt
		bne l92b5
		lda pacman_hpos
		cmp #$ca
		bne l9299
		lda #$2a
		sta pacman_hpos
		lda #$df
		sta pacman_screen_ptr
		dec pacman_screen_ptr+1
l9299:	inc pacman_hpos
l929b:	lda pacman_byte_ctr
		cmp #$03
		bne l92b3
		lda #$00
		sta pacman_byte_ctr
		lda pacman_hpos
		cmp pacman_hpos_save
		beq l92b5
		inc pacman_screen_ptr
		bne l92b5
		inc pacman_screen_ptr+1
		bne l92b5
l92b3:	inc pacman_byte_ctr
l92b5:	ldy #$02						; point to pac right
		bne l931d
l92b9:	lda pacman_motion_cnt
		bne l92f0
		lda pacman_hpos
		cmp #$2a
		bne l92cd
		lda #$ca
		sta pacman_hpos
		lda #$07
		sta pacman_screen_ptr
		inc pacman_screen_ptr+1
l92cd:	dec pacman_hpos
		lda pacman_byte_ctr
		bne l92d9
		lda #$03
		sta pacman_byte_ctr
		bne l92f0
l92d9:	dec pacman_byte_ctr
		bne l92f0
		lda pacman_hpos
		cmp pacman_hpos_save
		beq l92f0
		sec
		lda pacman_screen_ptr
		sbc #$01
		sta pacman_screen_ptr
		lda pacman_screen_ptr+1
		sbc #$00
		sta pacman_screen_ptr+1
l92f0:	ldy #$04						; point to pac left
		bne l931d
l92f4:	lda pacman_direction
		cmp #$01
		bne l92fe
		ldy #$06						; up
		bne l9314
l92fe:	cmp #$02
		bne l9306
		ldy #$08						; down
		bne l9314
l9306:	cmp #$04
		bne l930e
		ldy #$04						; left
		bne l9314
l930e:	cmp #$08
		bne pacstp
		ldy #$02						; right
l9314:	lda #$0a
		bne l9333
pacstp:	ldy #$00						; point to pac dot
		tya
		beq l9333
l931d:	ldx pacman_sequence
		bne l9325
		inc pacman_sequence
		bne pacstp
l9325:	dex
		lda PacmanIndex,x
		cpx #$02
		bne l9331
		ldx #$ff
		stx pacman_sequence
l9331:	inc pacman_sequence
l9333:	tax
		lda PacmanDataPointers,y
		sta pixel_get_ptr
		iny
		lda PacmanDataPointers,y
		sta pixel_get_ptr+1
		txa
		clc
		adc pixel_get_ptr
		sta pixel_get_ptr
		lda #$00
		adc pixel_get_ptr+1
		sta pixel_get_ptr+1
		ldy #$09
!ifdef 	P500{
		lda #GAMEBANK
		sta IndirectBank				; select bank 0 for pointer operations
}
l934d:	lda (pixel_get_ptr),y
		sta PacmanBuffer+3,y
		dey
		bpl l934d
		lda #<PacmanBuffer
		sta pixel_get_ptr
		lda #>PacmanBuffer
		sta pixel_get_ptr+1
		lda pacman_vpos
		sta pixel_put_ptr
		lda #$53
		sta pixel_put_ptr+1
		clc
		lda pacman_hpos
		sta sprite_x+4
		adc #$02
		sta sprite_pacman_x+2
		adc #$02
		sta sprite_pacman_x+1
		adc #$02
		sta sprite_pacman_x
		ldy #$0f
l937c:	lda (pixel_get_ptr),y
		sta (pixel_put_ptr),y
		dey
		bpl l937c
!ifdef 	P500{
		lda #SYSTEMBANK
		sta IndirectBank				; switch back to bank 15
}
l9383:	rts
; -------------------------------------------------------------------------------------------------
; $9384 munchy subroutine eats dots
Munchy:	lda pacman_byte_ctr
		bne l93e8
		lda pacman_vmap_count
		bne l93e8
		lda pacman_hpos
		cmp pacman_hpos_save
		bne l9398
		lda pacman_vpos
		cmp pacman_vpos_save
		beq l9383
l9398:	lda pacman_vpos
		sta pacman_vpos_save
		lda pacman_hpos
		sta pacman_hpos_save
		ldy #$00
!ifdef 	P500{				; Y already $00
		sty IndirectBank				; select bank 0 for pointer operations
}
		lda (pacman_screen_ptr),y
!ifdef 	P500{
		ldx #SYSTEMBANK
		stx IndirectBank				; switch back to bank 15
}
		cmp #$01
		beq l93b0
		cmp #$02
		bne l93e8
		tya
!ifdef 	P500{				; Y already $00
		sty IndirectBank				; select bank 0 for pointer operations
}
		sta (pacman_screen_ptr),y
!ifdef 	P500{				; X already $0f
		stx IndirectBank				; switch back to bank 15
}
		rts
; -------------------------------------------------------------------------------------------------
; $93b0
l93b0:	sta player_score_text+4
		jsr l946d
		lda #$01
		sta eatdot_sound_flag
		sta pacman_dly_eating
		lda #$00
		sta eatdot_sound_cnt
		lda eatdot_sound_togg
		bne l93c7
		lda #$01
		bne l93c9
l93c7:	lda #$00
l93c9:	sta eatdot_sound_togg
		lda #$00
		tay
!ifdef 	P500{				; Y already $00
		sty IndirectBank				; select bank 0 for pointer operations
}
		sta (pacman_screen_ptr),y
!ifdef 	P500{
		ldx #SYSTEMBANK
		stx IndirectBank				; switch back to bank 15
}
l93d0:	ldx player_number
		inc dots_eaten_lo,x
		bne l93d8
		inc dots_eaten_hi,x
l93d8:	ldx player_number
		lda dots_eaten_hi,x
		beq l93e8
		lda dots_eaten_lo,x
		cmp #$04
		bne l93e8
		lda #$01
		sta rereck_flag
l93e8:	rts
; -------------------------------------------------------------------------------------------------
; $93e9
dottest:ldx player_number
		lda bigdot_status,x
		sta temp
		ldx pacman_vpos
		ldy pacman_hpos
		lda #$01
		bit temp
		beq l9401
		cpx #$3c
		bne l9401
		cpy #$3a
		beq l9428
l9401:	asl
		bit temp
		beq l940e
		cpx #$3c
		bne l940e
		cpy #$be
		beq l9428
l940e:	asl
		bit temp
		beq l941b
		cpx #$a4
		bne l941b
		cpy #$3a
		beq l9428
l941b:	asl
		bit temp
		beq l93e8
		cpx #$a4
		bne l93e8
		cpy #$be
		bne l93e8
l9428:	eor #$0f
		and temp
		ldx player_number
		sta bigdot_status,x
		lda #$05
		sta player_score_text+4
		jsr l946d
		lda #$01						; set up for blue monsters
		sta flash_count
		lda #$ff
		sta gulp_count
		ldx player_number
		lda maze_count1,x
		tax
		lda BlueTimerValues,x
		sta flight_timer
		ldx #$03
l944b:	lda monster_status,x
		asl
		bmi l9467
		lda monster_status,x
		ora #$80						; set status = flight
		sta monster_status,x
		lsr
		lsr
		lsr
		bcs l9467
		and #$3b
		beq l9467
		lda monster_direction,x
		tay
		lda BlueReverseTable,y
		sta monster_direction,x
l9467:	dex
		bpl l944b
		jmp l93d0
; -------------------------------------------------------------------------------------------------
; $946d pscore will add any points scored to the players' score		
l946d:	lda #$00
		sta score_carry_bit
		sed								; set decimal mode
		lda player_number
		beq l947a
		ldx #$4c						; player 2
		bne l947c
l947a:	ldx #$2f						; palyer one
l947c:	ldy #$05						; 6 digits
l947e:	clc
		lda GameScreen,x				; load score digit from screen and isolate lower nibble
		and #$0f
		adc score_carry_bit
		adc $0000+player_score_text,y	; add saved digit
		pha
		and #$10						; isolate bit#4
		beq l9490						; skip if not > 9
		lda #$01						; save 1 to carry_byte
l9490:	sta score_carry_bit
		pla
		ora #$10
		cmp #$10
		bne l94ab
		cpy #$00
		bne l94a0
		tya
		beq l94ab
l94a0:	lda GameScreen-1,x
		bne l94a9
		lda score_carry_bit
		beq l94ab
l94a9:	lda #$10
l94ab:	ora #$90
		sta GameScreen,x
		dex
		dey
		bpl l947e
		cld
		ldx #$05
		lda #$00
l94b9:	sta player_score_text,x
		dex
		bpl l94b9
		ldx player_number
		lda bonus_pacman,x
		bne nobonus
		cpx #$00
		bne l94d0
		lda GameScreen+$2b
		cmp #$90
		bne l94d7
nobonus:rts
l94d0:	lda GameScreen+$48
		cmp #$90
		beq nobonus
l94d7:	inc bonus_pacman,x
		inc extra_pacman1,x
		jmp UpdateExtraPacs			; sub: Update extra pacmans
; -------------------------------------------------------------------------------------------------
; $94de Maze handler subroutine
; entry: 	a reg value equals vpos
;			y reg value equals hpos
; exit:		a reg value equals permissible directions for any object from any position
; bit 0 set - up ok
; bit 1 set - dn ok
; bit 2 set - rt ok
; bit 3 set - lf ok
; carry bit is set if decision point was reached otherwise it is cleared
MazeHandler:
		lda monster_hpos,x
		sta hpos_saver
		lda monster_vpos,x
		stx temp
		ldx #$09
l94e8:	cmp VTable,x					; search vpos
		beq l94fc						; match found
		dex
		bpl l94e8						; keep looking
		lda hpos_saver					; none found so try hpos
		ldy #$09
l94f4:	cmp HTable,y					; search hpos
		beq l950d						; match found
		dey
		bpl l94f4
l94fc:	ldy #$09						; now we check hpos table
		lda hpos_saver					; to see if decision pt.
l9500:	cmp HTable,y
		beq l9512						; yes - make choice
		dey
		bpl l9500
		lda #$0c						; no - keep going
		clc
		bcc l9523
l950d:	lda #$03						; only one match found
		clc
		bcc l9523
l9512:	txa								; now index into table
		asl
		tax
		lda HorizontalTablePointers,x
		sta pixel_get_ptr
		inx
		lda HorizontalTablePointers,x
		sta pixel_get_ptr+1
!ifdef 	P500{
		lda #GAMEBANK
		sta IndirectBank				; select bank 0 for pointer operations
}
		lda (pixel_get_ptr),y
!ifdef 	P500{
		ldx #SYSTEMBANK
		stx IndirectBank				; switch back to bank 15
}
		sec
l9523:	ldx temp
		sta temp
		php
		cpx #$04
		beq l9554
		lda monster_direction,x
		tay
		lda temp
		and ReverseTable,y
		sta temp
		lda monster_vpos,x
		cmp #$64
		bne l9554
		lda monster_hpos,x
		cmp #$76
		beq l9546
		cmp #$82
		bne l9554
l9546:	lda monster_status,x
		bmi l9550
		lda temp
		and #$0e
		bne l9552
l9550:	lda #$01
l9552:	sta temp
l9554:	lda temp
		plp
		rts
; -------------------------------------------------------------------------------------------------
; $9558
vchase:	lda #$00
		sta flight_sound_start
		lda chase_sound_start
		bne l956a
		lda #$28
		sta chase_sound_freq
		lda #$01
		sta chase_sound_start
		sta chase_sound_dir
l956a:	lda chase_sound_dir
		cmp #$01
		bne l9583
		lda chase_sound_freq
		cmp #$40
		bcc l957c
		lda #$02
		sta chase_sound_dir
		bne l958f
l957c:	lda chase_whine_delta
		clc
		adc chase_sound_freq
		bne l9594
l9583:	lda chase_sound_freq
		cmp #$28
		bcs l958f
		lda #$01
		sta chase_sound_dir
		bne l957c
l958f:	sec
		lda chase_sound_freq
		sbc chase_whine_delta
l9594:	sta chase_sound_freq
!ifdef 	P500{
		ldy #SR_V2FREQ
		sta (SID),y						; SID voice 2 frequency hi
		lda #$11
		ldy #SR_V2CTRL
		sta (SID),y						; SID voice 2 control = triangle, on
} else{
		sta $d408						; SID voice 2 frequency hi
		lda #$11
		sta $d40b						; SID voice 2 control = triangle, on
}
		rts
; -------------------------------------------------------------------------------------------------
; $959f
blink3:	lda jiffy
		and #$0f
		beq blnkon
		cmp #$08
		beq l95d1
		rts
; -------------------------------------------------------------------------------------------------
; $95aa
blnkon:	ldy #$02
		ldx player_number
		lda bigdot_status,x
		sta temp
		lda #$01
		bit temp
		beq l95bb
		sty GameScreen+$cb
l95bb:	asl
		bit temp
		beq l95c3
		sty GameScreen+$ec
l95c3:	asl
		bit temp
		beq l95cb
		sty GameScreen+$2d3
l95cb:	asl
		bit temp
		bne l95dc
		rts
; $95d1
l95d1:	ldy #$00
		sty GameScreen+$cb
		sty GameScreen+$ec
		sty GameScreen+$2d3
l95dc:	sty GameScreen+$2f4
		rts
; -------------------------------------------------------------------------------------------------
; $95e0 speed sequencing for objects x reg = index value for object 
; x=0 to 3 for monsters 1 - 4, x=4 for pacman
; on exit: a reg = 0 indicates update time
l95e0:	dec monster_speed_cnt,x
		beq l95e7
		lda #$ff
		rts
; $95e7
l95e7:	lda monster_speed_sequ,x
		cmp #$03
		bne l95f1
		lda #$ff
		sta monster_speed_sequ,x
l95f1:	inc monster_speed_sequ,x
		ldy player_number
		lda $0000+maze_count1,y			; load maze in A
		cmp #$06
		bcc +
		lda #$06						; limit A to 6 and move it to Y
+		tay
		cpx #$04
		bne l9608
		lda PacmanSpeedTable,y
		bpl +							; skip always
l9608:	lda MonsterSpeedTable,y
+		clc
		adc monster_speed_sequ,x
		tay
		lda Speed,y
		sta monster_speed_cnt,x
		lda #$00
		rts
; -------------------------------------------------------------------------------------------------
; $9617 chase sequencing
l9617:	lda jiffy
		and #$07
		bne l9623
		lda chase_timer
		beq l9623
		dec chase_timer
l9623:	ldx player_number
		lda dots_eaten_hi,x
		beq l9632
l9629:	ldx #$03
		jsr l9666
		lda #$05
		bne l964f
l9632:	lda dots_eaten_lo,x
		cmp #$f0
		bcs l9629
		cmp #$e0
		bcc l9645
		ldx #$02
		jsr l9666
		lda #$04
		bne l964f
l9645:	cmp #$b0
		bcc l964d
		lda #$03
		bne l964f
l964d:	lda #$02
l964f:	sta chase_whine_delta
		ldx #$03
l9653:	lda monster_status,x
		cmp #$08
		beq l965f
		cmp #$10
		beq l965f
		bne l9662
l965f:	jsr l9a69
l9662:	dex
		bpl l9653
		rts
; -------------------------------------------------------------------------------------------------
; $9666
l9666:	lda monster_status,x
		cmp #$08
		beq l9674
		cmp #$10
		beq l9674
		cmp #$20
		bne l9678
l9674:	lda #$02
		sta monster_status,x
l9678:	dex
		bpl l9666
		stx chase_timer
		rts
; -------------------------------------------------------------------------------------------------
; $967e
fruity:	lda fruit_score_flag
		beq l9696
		lda fruit_score_timer
		bne l9693
		lda #$00
		sta fruit_score_flag
		ldx #$04
l968c:	sta GameScreen+$241,x
		dex
		bpl l968c
		rts
l9693:	dec fruit_score_timer
l9695:	rts
; -------------------------------------------------------------------------------------------------
; $9696
l9696:	lda fruit_display_flag
		bne l96d4
		ldx player_number
		lda dots_eaten_lo,x
		tay
		lda fruit_counter,x
		beq l96a8
		cmp #$01
		beq l96ac
		rts
; -------------------------------------------------------------------------------------------------
; $96a8
l96a8:	cpy #$50
		beq l96b0
l96ac:	cpy #$a0
		bne l9695
l96b0:	inc fruit_counter,x
		lda maze_count1,x
		cmp #$0c
		bcc l96ba
		lda #$0c						; max difficulty = $0c
l96ba:	tax
		lda FruitChars,x
		sta GameScreen+$243
		clc
		adc #$01
		sta GameScreen+$244
		lda #$01
		sta fruit_display_flag
		lda #FRUITDELAY					; load fruitdelay
		sta fruit_timer
		lda #$02
		sta fruit_timer+1
		rts
l96d4:	lda fruit_timer+1
		bne l96e7
		lda fruit_timer
		bne l96e7
		lda #$00
		sta GameScreen+$243
		sta GameScreen+$244
		sta fruit_display_flag
		rts
l96e7:	dec fruit_timer
		bne l96ed
		dec fruit_timer+1
l96ed:	rts
; -------------------------------------------------------------------------------------------------
; $96ee Pacman monster subroutines
eyeonly:	lda #$00
		sta tweet_sound_flag
		ldx #$03
eyeonlp:	lda monster_status,x
		cmp #$44
		beq nxteyd
		asl
		bpl nxteyd
		cpx gulped_last
		beq nxteyd
		inc tweet_sound_flag
		lda monster_hpos,x
		tay
		lda monster_vpos,x
		cpy #$7c
		bne tsteyev
		cmp #$64
		bne tsteyev
		lda monster_direction,x
		cmp #$01
		bne eyehome
		lda #$04
		sta monster_direction,x
		lda monster_vpos,x
		bne tsteyev
eyehome:lda #$44
		sta monster_status,x
		lda #$ff
		sta monster_start_sequ,x
		bne nxteyd
tsteyev:jsr MazeHandler
		bcc sameyd
		clc
		lda #$7c
		sta monster_targ_hpos,x
		lda #$64
		sta monster_targ_vpos,x
		jsr mdirct
sameyd:	lda monster_direction,x
		jsr monhnd
nxteyd:	dex
		bpl eyeonlp
		lda tweet_sound_flag
		bne gtweet
		lda #$00
		sta tweet_sound_freq
		beq l9752
gtweet:	lda freeze_flag
		bne l9752
		jsr vtweet
l9752:	ldx #$03
l9754:	lda monster_status,x
		clc
		lsr
		lsr
		lsr
		bcc l9784
		lda monster_status,x
		asl
		bpl l9767
		jsr l97d1
		jmp l9784
l9767:	lda jiffy
		and #$03
		bne l9784
		cpx #$03
		bne l9777
		jsr l97c7
		jmp l9784
l9777:	cpx #$02
		bne l9781
		jsr l97bd
		jmp l9784
l9781:	jsr l9788
l9784:	dex
		bpl l9754
		rts
; -------------------------------------------------------------------------------------------------
; $9788
l9788:	lda monster_vpos,x
		cmp #$64
		bne l97a9
		lda monster_status,x
		bne l9796
		lda #$01
		bne l97a2
l9796:	and #$80
		ora #$02
		bpl l97a2
		sta chase_timer
		ldy #$08
		bne l97a4
l97a2:	ldy #$04
l97a4:	sty monster_direction,x
		sta monster_status,x
		rts
; -------------------------------------------------------------------------------------------------
; $97a9
l97a9:	lda #$01
l97ab:	sta monster_direction,x
		jmp monhnd
l97b0:	lda monster_status,x
		and #$0f
		sta monster_status,x
!ifdef 	P500{
		txa
		clc
		adc #VR_MOBCOL
		tay
		lda SpriteColors,x
		sta (VIC),y						; set VIC sprite color from table
} else{
		lda SpriteColors,x
		sta $d027,x						; set VIC sprite color from table
}
		rts
; -------------------------------------------------------------------------------------------------
; $97bd
l97bd:	lda monster_hpos,x
		cmp #$7c
		beq l9788
l97c3:	lda #$08
		bne l97ab
l97c7:	lda monster_hpos,x
		cmp #$7c
		beq l9788
l97cd:	lda #$04
		bne l97ab
l97d1:	lda monster_vpos,x
		cmp #$74
		bne l97e1
		cpx #$02
		beq l97e5
		cpx #$03
		beq l97ed
		bne l97b0
l97e1:	lda #$02
		bne l97ab
l97e5:	lda monster_hpos,x
		cmp #$70
		bne l97cd
		beq l97b0
l97ed:	lda monster_hpos,x
		cmp #$88
		bne l97c3
		beq l97b0
l97f5:	ldx #$01
l97f7:	lda monster_status,x
		and #$7f
		bne l9820
		lda monster_timer,x
		beq l9809
		dec monster_timer,x
		jsr l9826
		jmp l9820
l9809:	cpx #$03
		bne l9813
		jsr l97c7
		jmp l9820
l9813:	cpx #$02
		bne l981d
		jsr l97bd
		jmp l9820
l981d:	jsr l9788
l9820:	inx
		cpx #$04
		bne l97f7
		rts
; -------------------------------------------------------------------------------------------------
; $9826
l9826:	lda monster_direction,x
		cmp #$01
		bne l9836
		lda monster_vpos,x
		cmp #$70
		bne l9840
		lda #$02
		bne l983e
l9836:	lda monster_vpos,x
		cmp #$78
		bne l9840
		lda #$01
l983e:	sta monster_direction,x
l9840:	lda monster_direction,x
		jmp monhnd
l9845:	lda monster_status,x
		bpl l984f
		lsr
		lsr
		lsr
		bcc l985e
		rts
; -------------------------------------------------------------------------------------------------
; $984f
l984f:	cmp #$01
		beq l9877
		cmp #$02
		bne l9861
		lda chase_timer
		bne l985e
		jmp l9915
l985e:	jmp l993f
l9861:	cmp #$08
		bne l9868
		jmp l98c8
l9868:	cmp #$10
		bne l986f
		jmp l98ea
l986f:	cmp #$20
		bne l9876
		jmp l9915
l9876:	rts
; -------------------------------------------------------------------------------------------------
; $9877
l9877:	jsr MazeHandler
		bcc l987f
		clc
		inc monster_start_sequ,x
l987f:	txa
		asl
		tay
		lda MonsterStartPointer,y
		sta pixel_get_ptr
		iny
		lda MonsterStartPointer,y
		sta pixel_get_ptr+1
		lda monster_start_sequ,x
		tay
!ifdef 	P500{
		lda #GAMEBANK
		sta IndirectBank				; select bank 0 for pointer operations
}
		lda (pixel_get_ptr),y
!ifdef 	P500{
		ldy #SYSTEMBANK
		sty IndirectBank				; switch back to bank 15
}
		cmp #$0f
		bne l98c3
		cpx #$00
		bne l98a1
		lda #$50
		sta chase_timer
		jmp l993b
l98a1:	lda #$08
		sta monster_status,x
		lda #$00
		sta monster_patt_count,x
!ifdef 	P500{
		ldy #SR_RANDOM
		lda (SID),y						; load random value SID register $1b
} else{
		lda $d41b						; load random value SID register $1b
}
		and #$0f						; calc random value from $0-$f
		sta monster_patt_index,x
		asl
		tay
		lda PatternStartHV,y
		sta monster_targ_hpos,x
		iny
		lda PatternStartHV,y
		sta monster_targ_vpos,x
		lda #$96
		sta monster_timer,x
		bne l98c8
l98c3:	sta monster_direction,x
		jmp monhnd
l98c8:	lda monster_timer,x
		bne l98cf
		jmp l9915
l98cf:	jsr MazeHandler
		bcs l98d7
l98d4:	jmp l9950
l98d7:	clc
		jsr mdirct
		lda monster_hdir,x
		bne l98d4
		lda monster_vdir,x
		bne l98d4
		lda #$10
		sta monster_status,x
		clc
		bcc l98f6
l98ea:	lda monster_timer,x
		beq l9915
		jsr MazeHandler
		bcc l98f6
		clc
		inc monster_patt_count,x
l98f6:	lda monster_patt_index,x
		tay
		lda PatternIndex,y
		adc monster_patt_count,x
		tay
		lda Pattern,y
		bne l9910
		sta monster_patt_count,x
		lda monster_patt_index,x
		tay
		lda PatternIndex,y
		tay
		lda Pattern,y
l9910:	sta monster_direction,x
		jmp l9950
l9915:	lda #$20
		sta monster_status,x
		txa
		asl
		tay
		lda HomePositionHV,y
		sta monster_targ_hpos,x
		iny
		lda HomePositionHV,y
		sta monster_targ_vpos,x
		jsr MazeHandler
		bcc l9950
		clc
		jsr mdirct
		lda monster_hdir,x
		bne l9950
		lda monster_vdir,x
		bne l9950
		jmp l98a1
l993b:	lda #$02
		sta monster_status,x
l993f:	lda pacman_hpos
		sta monster_targ_hpos,x
		lda pacman_vpos
		sta monster_targ_vpos,x
		jsr MazeHandler
		bcc l9950
		clc
		jsr mdirct
l9950:	lda monster_direction,x
monhnd:
		cmp #$01
		bne l9962
		ldy monster_still_flag
		bne l995e
		dec monster_vpos,x
		dec monster_vpos,x
l995e:	lda #$00
		beq l998f
l9962:	cmp #$02
		bne l9972
		ldy monster_still_flag
		bne l996e
		inc monster_vpos,x
		inc monster_vpos,x
l996e:	lda #$0a
		bne l998f
l9972:	cmp #$04
		bne l9980
		ldy monster_still_flag
		bne l997c
		dec monster_hpos,x
l997c:	lda #$14
		bne l998f
l9980:	cmp #$08
		bne l998e
		ldy monster_still_flag
		bne l998a
		inc monster_hpos,x
l998a:	lda #$1e
		bne l998f
l998e:	rts
; -------------------------------------------------------------------------------------------------
; $998f
l998f:	ldy monster_status,x
		bpl l9995
		lda #$28
l9995:	tay
		lda monster_status,x
		asl
		bmi l99a8
		tya
		ldy monster_skirt_flag
		beq l99a4
		ldy #$aa
		bne l99ac
l99a4:	ldy #$54
		bne l99ac
l99a8:	lda #$32
		ldy #$00
l99ac:	sty MonsterBuffer+12
		clc
		adc #<MonsterUp
		sta pixel_get_ptr
		lda #$00
		adc #>MonsterUp					; pointer to sprite data
		sta pixel_get_ptr+1
		ldy #$09
!ifdef 	P500{
		lda #GAMEBANK
		sta IndirectBank				; select bank 0 for pointer operations
}	
l99bc:	lda (pixel_get_ptr),y
		sta MonsterBuffer+2,y
		dey
		bpl l99bc
		lda #<MonsterBuffer
		sta pixel_get_ptr
		lda #>MonsterBuffer
		sta pixel_get_ptr+1
		lda monster_hpos,x
		sta sprite_x,x
		lda monster_vpos,x
		sta pixel_put_ptr
		lda #$54
		sta pixel_put_ptr+1
		txa
		clc
		adc pixel_put_ptr+1
		sta pixel_put_ptr+1
		ldy #$0f
l99e1:	lda (pixel_get_ptr),y
		sta (pixel_put_ptr),y
		dey
		bpl l99e1
!ifdef 	P500{
		lda #SYSTEMBANK
		sta IndirectBank				; switch back to bank 15
}
		rts
; -------------------------------------------------------------------------------------------------
; $99e9
mdirct:	lda monster_vpos,x
		cmp monster_targ_vpos,x
		beq l9a01
		bcc l99f9
		lda monster_status,x
		bmi l99fd
l99f5:	lda #$01
		bne l9a03
l99f9:	lda monster_status,x
		bmi l99f5
l99fd:	lda #$02
		bne l9a03
l9a01:	lda #$00
l9a03:	sta monster_vdir,x
		lda monster_hpos,x
		cmp monster_targ_hpos,x
		beq l9a1d
		bcs l9a15
		lda monster_status,x
		bmi l9a19
l9a11:	lda #$08
		bne l9a1f
l9a15:	lda monster_status,x
		bmi l9a11
l9a19:	lda #$04
		bne l9a1f
l9a1d:	lda #$00
l9a1f:	sta monster_hdir,x
		lda monster_vdir,x
		beq l9a3e
		bit temp
		beq l9a3e
		lda monster_hdir,x
		beq l9a36
		bit temp
		beq l9a36
!ifdef 	P500{
		ldy #SR_RANDOM
		lda (SID),y						; load random value SID register $1b
} else{
		lda $d41b						; load random value SID register $1b
}
		bmi l9a3a
l9a36:	lda monster_vdir,x
		bne l9a64
l9a3a:	lda monster_hdir,x
		bne l9a64
l9a3e:	lda monster_hdir,x
		bit temp
		bne l9a3a
!ifdef 	P500{
		ldy #SR_RANDOM
		lda (SID),y						; load random value SID register $1b
} else{
		lda $d41b						; load random value SID register $1b
}
		and temp
		bne l9a4d
		lda temp
l9a4d:	lsr
		bcc l9a54
		lda #$01
		bne l9a64
l9a54:	lsr
		bcc l9a5b
		lda #$02
		bne l9a64
l9a5b:	lsr
		bcc l9a62
		lda #$04
		bne l9a64
l9a62:	lda #$08
l9a64:	sta monster_direction,x
		lda monster_direction,x
		rts
; -------------------------------------------------------------------------------------------------
; $9a69
l9a69:	lda monster_vpos,x
		cmp pacman_vpos
		beq l9a76
		lda monster_hpos,x
		cmp pacman_hpos
		beq l9abb
		rts
; -------------------------------------------------------------------------------------------------
; $9a76
l9a76:	ldy #$09
l9a78:	lda VTable,y
		cmp pacman_vpos
		beq l9a83
		dey
		bpl l9a78
l9a82:	rts
; -------------------------------------------------------------------------------------------------
; $9a83
l9a83:	lda monster_hpos,x
		cmp pacman_hpos
		bcs l9aa2
		lda monster_direction,x
		cmp #$08
		bne l9a82
l9a8f:	lda HWalls,y
		cmp #$ff
		beq l9aff
		cmp monster_hpos,x
		bcs l9a9d
		iny
		bne l9a8f
l9a9d:	cmp pacman_hpos
		bcs l9aff
		rts
; -------------------------------------------------------------------------------------------------
; $9aa2
l9aa2:	lda monster_direction,x
		cmp #$04
		bne l9a82
l9aa8:	lda HWalls,y
		cmp #$ff
		beq l9aff
		cmp pacman_hpos
		bcs l9ab6
		iny
		bne l9aa8
l9ab6:	cmp monster_hpos,x
		bcs l9aff
		rts
; -------------------------------------------------------------------------------------------------
; $9abb
l9abb:	ldy #$09
l9abd:	lda HTable,y
		cmp pacman_hpos
		beq l9ac8
		dey
		bpl l9abd
l9ac7:	rts
; -------------------------------------------------------------------------------------------------
; $9ac8
l9ac8:	lda monster_vpos,x
		cmp pacman_vpos
		bcc l9ae7
		lda monster_direction,x
		cmp #$01
		bne l9ac7
l9ad4:	lda VWalls,y
		cmp #$ff
		beq l9aff
		cmp pacman_vpos
		bcs l9ae2
		iny
		bne l9ad4
l9ae2:	cmp monster_vpos,x
		bcs l9aff
		rts
; -------------------------------------------------------------------------------------------------
; $9ae7
l9ae7:	lda monster_direction,x
		cmp #$02
		bne l9ac7
l9aed:	lda VWalls,y
		cmp #$ff
		beq l9aff
		cmp monster_vpos,x
		bcs l9afb
		iny
		bne l9aed
l9afb:	cmp pacman_vpos
		bcc l9b0f
l9aff:	cpx #$02
		bne l9b07
		lda jiffy
		bmi l9b0f
l9b07:	lda #$02
		sta monster_status,x
		lda #$a0
		sta chase_timer
l9b0f:	rts
; ***************************************** ZONE DATA2 ********************************************
!zone data2
!source "pm500dat.b"				; C64 + P500 common data
!ifndef	P500{!source "c64enibb.b"}	; C64 encoded nibbles
; ***************************************** ZONE P500 *********************************************
!zone p500
!ifdef 	P500{
; P500 I/O pointer init
InitP500:
		ldx #$00
iniiolp:lda IOPointerTable,x			; copy 8 IO pointer to ZP
		sta ColorRAM0,x
		inx
		cpx #$1a						; number of IO pointers
		bne iniiolp
		
		lda #<Cold	 					; set NMI vector to Cold start
		sta HW_NMI
		lda #>Cold
		sta HW_NMI+1
		lda #<Interrupt					; set IRQ vector to interrupt routine
		sta HW_IRQ
		lda #>Interrupt
		sta HW_IRQ+1
		
		ldy #$06
		lda (TPI1),y					; load TRI1 control register
		and #$0f						; clear CA, CB control bits#4-7 vic bank 0/15 select 
		ora #$a0						; set bit#5,4=10 CA=low -> Video matrix in bank 0
		sta (TPI1),y					; set bit#7,6=10 CB=high -> Characterset in bank 0 
		ldy #$02
		lda (TPI2),y					; load TPI2 port c
		and #$3f						; clear bit#6,7 vic 16k select bank $0000-$3fff
		sta (TPI2),y					; store to TPI2 port c
		lda #$3a
		ldy #$18						; VIC reg $18 memory pointers
		sta (VIC),y						; set VM13-10=$3 screen at $0c00, CB13,12,11,x=1010 char at $2800
		lda #$7f						; bit#7=0 clears/mask out all 5 irq sources with bit#0-4 = 1
		ldy #$0d						; CIA interrupt control register
		sta (CIA),y						; disable all hardware interrupts
		lda #$00
		ldy #$05
		sta (TPI1),y					; set TPI1 reg $5 interrupt mask reg = $00 - disable all irq
		lda #$ff
		ldy #$00
		sta (TPI2),y					; reset TPI2 port a to no column
		rts
!source "p500data.b"		
}