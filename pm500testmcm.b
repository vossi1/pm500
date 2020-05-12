; Disassembled by Vossi 04/2020
; Prepared for ACME reassembling
; Comments by Vossi 05/2020
; Converted for P500 by Vossi 05/2020
!cpu 6502
; switches
P500 = 1		; P500 bank 0 file
MCMLOGO = 1
;CRT = 1		; CRT header for VICE
!ifdef 	P500{!to "pm500.prg", cbm
} else{ !ifdef CRT {!to "pm500.crt", plain
		} else{ !to "pm500.rom", plain }}
; ########################################### TODO ################################################
;
; nothing
; ########################################### BUGS ################################################
;
; Fruits in menu are not in multicolor so they look like zebra ;)
; Background color 2 for multicolor is not set to 2 but works on C64 because its VIC init value
; ######################################### P500 MODS #############################################
; Indirect reg standard = $15, switch only to $0 for game indirect pointer instructions 
; Game runs exclusive - Kernal not used -> IRQ vector $fffe in bank 0 set to game irq routine
; Added unused highscore text above highscore digits
; Added rasterirq -> switch to multicolor-mode in menu to display the fruits correctly
; Set backgroundcolor 2 correctly
; Added uncompressed Maze, user chars and nibble tables
; ******************************************* INFO ************************************************
; Menu screen is at $0c00, menu font at $2800
; Game screen is at $0400, game font at $2000, multicolor
; First two lines of game screen are not multicolor
; Sprites 0-3 are monsters, sprite 4 is pacman, sprites are 10/11 px heigh, 6px wide + xpanded -> 12px
; Sprite data pointer are static = $c0-$c4 -> $3000-$3100, sprites are not multicolor
; Sprite source data is at $5300,$5400-$57ff and will copied in each cycle to the VIC sprite data
; First half of char ROM copied to lower half of user fonts
; Game screen is compressed at $81ef -> decompressed to $4000
; Game font is encoded at $8011 -> decoded to $2000 
; Menu font is encoded at $9e82-> decoded to $2800
; Encoded font data are 2bit count + 6bit tile numbers of table at $9dc6
; - 00-7f char, 80-bf next char repeated 0-3f, c0-fe low nib - next byte high-low nib, ff end
; Nibble table lo+hi decoded -> $4c00
; Only SID voices 1+2 are used with sawtooth+triangle
; SID voice 3 with noise and reg $1b used for random number generation 
; Monsters/Ghosts/Goblins: Lightred=Pinky, Lightred=Pinky, Green=Inky, Orange=Clyde
; ******************************************* FONT ************************************************
; Menufont:		$00-$17 = PACMAN logo 2 rows of 12 chars
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
LIGHTBLUE				= $0e
GRAY3					= $0f
MCM						= $08		; bit#3 for multicolor character
; game
RASTERLINE1				= $33+4*8	; just below the PACMAN logo in line 2+3
RASTERLINE2				= $32+10*8	; just above the difficulty fruit in line 11 
RASTERLINE3				= $33+13*8	; just below the difficulty fruit in line 11 
RASTERLINE4				= $33+200	; just below the lower text line 19 
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
!addr SpriteDataPointer	= $07f8		; 5 Pointer to sprite 0-4
!addr Playfield			= GameScreen + 2*40	; Line 2 of game screen
!addr MenuScreen		= $0c00		; Game screen page
!addr CharGame			= $2000		; User character game
!addr CharMenu			= $2800		; User character menu
!addr SpriteData		= $3000		; Sprite data 5x $40
!addr Player1Save		= $4400		; Game screen bac‚kup player 1
!addr Player2Save		= $4800		; Game screen backup player 2
!addr SpriteRAM			= $5300		; 5x Sprite RAM -$57ff
; ***************************************** ZERO PAGE *********************************************
!addr attract_ATARI		= $03		; Atari ATTRACT FLAG for screen saver - not used on Commodore
!addr state				= $07		; 0 = game, 1 = startup, 2 = delay menu, 3 = menu
!addr players			= $08		; 0 = 1 player, 1 = 2 players
!addr difficulty		= $09		; 0, 1, 2, 4, 6, 8, a, c
!addr restart_flag		= $0a		; 1 = new game, 2 after init, 3 at ready
!addr delay_menu		= $0b		; Jiffy-1 at start for 5s menu delay
!addr atract_timer_ATARI= $0c		; Countdown timer to attract mode - ONLY ATARI
!addr game_over_flag	= $0e		; 
!addr ready_flag		= $0f		; 
!addr intro_flag		= $10		; 
!addr swap_player_flag	= $11		; 
!addr reset_flag		= $12		;
!addr reset_timer		= $13		;
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
!addr score_pointer1	= $26		;
!addr score_pointer2	= $28		;
!addr pointer1			= $2a		; Source pointer
!addr pointer2			= $2c		; Target pointer
!addr fruit_counter		= $2e ; $2f	  Fruit counter player 1, 2
!addr bounce_timer_ATARI= $30		; ATARI key debounce - not used on Commodore
!addr ATARI_32			= $32		; ATARI - not used
!addr ATARI_33			= $33		; ATARI - not used
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
!addr fruit_timer 		= $59	 	; fruit timer (10 secs)
!addr fruit_display_flag= $5a	  	; fruit display flag
!addr fruit_color		= $5b 		; fruit color
!addr fruit_score_flag	= $5c	 	;fruit score flag - only stored, not used on Commodore 
!addr fruit_score_timer = $5d 		;fruit score timer
!addr notes_counter		= $5f		; Counter for music
;!addr vpos_saver		= $60		; verical position saver - not used on Commodore
!addr hpos_saver		= $61		; horizontal position saver

!addr jiffy				= $a2		; Jiffy clock 20ms counter from raster interrupt = Vsync
;						= $04		 ; 0 -> Sprite Direction loop
!addr blink_counter		= $b9		; Blink counter 1up/2up 0/1=off/on
!addr spritedata_pointer= $c0		; 16bit pointer for sprite data copy
!addr pressed_key		= $c5		; Pressed key from interrupt
; ***************************************** VARIABLES *********************************************
!addr sprite_x			= $02d0	; -$02d4 Sprite 0-4 x positions (VIC >>1 +$2c)
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
; **************************************** CRT HEADER *********************************************
!zone crt
!ifdef 	CRT{
*= $7fb0
		!byte $43, $36, $34, $20, $43, $41, $52, $54, $52, $49, $44, $47, $45, $20, $20, $20
		!byte $00, $00, $00, $40, $01, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00
		!byte $50, $61, $63, $2d, $4d, $61, $6e, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $43, $48, $49, $50, $00, $00, $20, $10, $00, $00, $00, $00, $80, $00, $20, $00
}
; ***************************************** ZONE INIT *********************************************
!zone init
!initmem FILL
*= $8000
!ifdef 	P500{
Cold:
		sei
		cld
		ldx #$ff
		txs
		jmp Start
} else{ 	
; ROM ident
		!byte <Start, >Start, <Warm, >Warm	; ROM start addresses
		!byte $c3, $c2, $cd, "8"			; cbm-rom ident-bytes 'C'= with init, 'BM', '8' = 4k-block 8
}
; ***************************************** ZONE DATA1 ********************************************
!zone data1
*= $8008
; $8008 table unused
		!byte $30, $02, $bb, $5a, $30, $5f, $ee, $3d, $a8
; ONLY C64 ROM: Adresses to decoded ROM data, Encoded font, compressed maze
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
		inc state						; increase state to 1 to start in menu mode
		ldx jiffy						; load jiffy = $00 (cleared at ZP clear loop)
		dex
		stx delay_menu					; remember jiffy-1 for 255 x 20ms = 5s menu delay
!ifndef P500{
; $83b3 Copy and uncompress maze
		lda #<MazeData
		sta pointer2
		lda #>MazeData
		sta pointer2+1					; set target pointer = MazeData
		lda #<CompressedMazeData
		sta pointer1
		lda #>CompressedMazeData
		sta pointer1+1					; set source pointer = compressed MazeData
		ldy #$00
mazcopy:lda (pointer1),y				; load byte from maze
		sta temp						; store for later bit#6 check
		bmi mazbit7						; skip if bit#7 = 1
		bpl mazchar						; branch if normal char
mazbit7:cmp #$ff						; check if $ff = end of data
		beq lookupt:					; exit loop
		bit temp
		bvs mazbit6						; branch if bit#6 = 1
		and #$7f						; clear ident bit#7
		tax								; use value in X as repeat counter
		jsr IncPointer1					; read next byte
		jsr LoadIncPointer1
mazrptb:jsr StoreIncPointer2			; copy byte to maze
		dex
		bpl mazrptb						; repeat store byte X times
		bmi mazcopy						; read next byte
mazbit6:and #$3f						; clear ident bits#7,6
		jsr StoreIncPointer2			; store byte to maze
		jsr IncPointer1					; next byte
		jsr LoadHiNibblePointer1		; load and shift high nibble 4 bits right
		jsr StoreIncPointer2			; store hi nibble
		lda (pointer1),y
		and #$0f						; isolate low nibble
mazchar:jsr StoreIncPointer2			; copy byte to maze
		jsr IncPointer1
		jmp mazcopy						; read next byte
; $8401 Copy and decode Nibbles
lookupt:lda #<CompressedNibbles
		sta pointer1
		lda #>CompressedNibbles
		sta pointer1+1					; set source pointer = $9f0e
		lda #<NibbleTable
		sta pointer2
		lda #>NibbleTable
		sta pointer2+1					; set target pointer = NibbleTable
		ldy #$00
		ldx #$00
lutcopy:jsr LoadHiNibblePointer1		; load and shift high nibble 4 bits right
		jsr StoreIncPointer2			; store high nibble
		jsr LoadIncPointer1
		and #$0f
		jsr StoreIncPointer2			; store low nibble
		inx
		bne lutcopy						; next byte
}
!ifdef 	P500{
; P500 Copy chars $00-$3f of the first (graphic) fontset to $80 of tboth custom fonts
		lda #SYSTEMBANK					; select bank 15 to get font from char ROM
		sta IndirectBank
fontcpy:lda (CharROM1),y				; load from character ROM - Y already $00	
		sta CharGame+$400,y				; store to game fontset from char $80
		sta CharMenu+$400,y				; store to menu fontset from char $80
		lda (CharROM0),y
		sta CharGame+$500,y
		sta CharMenu+$500,y
		dey
		bne fontcpy
		sty IndirectBank				; select bank 0 - Y already $00
} else{
; $8426 C64
		lda $dc0e						; stop CIA1 timer A to prevent problems when switching CharROM 
		and #$fe
		sta $dc0e
		lda CPUPort64					; enable character ROM
		and #$fb
		sta CPUPort64
		ldx #$00
fontcpy:lda CharROM64+$100,x			; load from character ROM
		sta CharGame+$400,x				; store to game fontset from char $80
		sta CharMenu+$400,x				; store to menu fontset from char $80
		lda CharROM64,x
		sta CharGame+$500,x
		sta CharMenu+$500,x
		dex
		bne fontcpy
		lda CPUPort64					; disable character ROM
		ora #$04
		sta CPUPort64
		lda $dc0e						; start CIA1 timer A
		ora #$01
		sta $dc0e
}
!ifdef 	P500{
; Copy User chars
ufntcpy:lda UserFontGame,y	
		sta CharGame,y
		lda UserFontGame+$100,y	
		sta CharGame+$100,y
		lda UserFontGame+$200,y	
		sta CharGame+$200,y
		lda UserFontGame+$300,y	
		sta CharGame+$300,y
		lda UserFontMenu,y	
		sta CharMenu,y
		dey
		bne ufntcpy
} else{
; $8459 Copy and decode user fonts
		lda #<EncodedUserFontGame
		sta pointer1
		lda #>EncodedUserFontGame
		sta pointer1+1					; pointer1 = $8011
		lda #<CharGame
		sta pointer2
		lda #>CharGame
		sta pointer2+1					; pointer2 = $2000
		jsr UncompressUserFont			; copy game user font
		lda #<EncodedUserFontMenu
		sta pointer1
		lda #>EncodedUserFontMenu
		sta pointer1+1					; pointer1 = $9e82
		lda #<(CharMenu+$08)
		sta pointer2
		lda #>(CharMenu+$08)
		sta pointer2+1					; pointer2 = $2808
		jsr UncompressUserFont			; copy menu user font
; $847f Copy user chars to menu user font
		ldx #$1f
ucmcopy:lda UserCharMenu,x				; copy 32 bytes to menu user font
		sta CharMenu+$a8,x
		dex
		bpl ucmcopy						; next byte
}
!ifdef 	P500{
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
; $848a SID init						; x already $ff
		stx $d40e						; SID voice 3 frequency lo to $ff 
		stx $d40f						; SID voice 3 frequency hi to $ff 
		lda #$80
		sta $d412						; SID voice 3 to $80 = noise for random generation
		lda #$f0
		sta $d406						; SID voice 1 SR to $f0
		sta $d40d						; SID voice 2 SR To $f0
}
; $849d Copy fruit chars from game font to menu font
		ldx #$a1						; copy $a1 bytes
charcpy:lda CharGame+$1cf,x
		sta CharMenu+$1cf,x
		dex
		bne charcpy
!ifdef 	P500{
; P500 Hardware interrupt vector setup, enable VIC raster IRQ
		lda #$01
		ldy #VR_EIRQ
		sta (VIC),y						; VIC enable raster interrupt
		lda #$1b
		ldy #VR_MODEY
		sta (VIC),y						; VIC RC8 = 0, DEN, 40 columns, Y = 3
		lda #RASTERLINE4
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
; $84c3 Main loop		
mainlp:	lda #$00
		ldx #$1f
clrgvar:sta $0e,x						; clear game variables $0e-$2d
		dex
		bpl clrgvar
		txs								; init stack with $ff
		jsr SoundOff					; SID sound off
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
		ldy #$ff						; set Y = $ff because its set to this value after SoundOff
} else{
		sta $d020						; VIC exterior color = black
		sta $d021						; VIC background color0 = black
		lda #LIGHTBLUE
		sta $d022						; VIC background color1 = lightblue
}
		jsr InitColors					; sub: init color RAM and VIC Sprite colors
		jsr InitGameScreen				; sub: copy game screen to screen RAM
		jsr BackupGameScreen1			; sub: init game screen player1 $4400
		jsr BackupGameScreen2			; sub: init game screen player2 $4800
		lda state						; state at startup = 1
		bne notgame						; branch if not game state 0
		lda jiffy
waitcyc:cmp jiffy
		beq waitcyc						; wait one jiffy = interrupt cycle
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
		jsr InitNewGame					; sub: init new game
		jsr InitGameVariables			; sub: Init game variables: lives, level, score...
		lda players
		beq new1up						; skip if 1 player
		jsr Init2Player					; sub: print 1Up, 2Up, print zero scores
		jmp new2up						; skip if 2 player
;		
new1up: jsr Init1Player					; sub: print 1Up, print zero score
new2up: lda #$02
		sta restart_flag				; restart = 2 after init
notgame:lda state
		bne checkey						; branch if not game state
		lda game_over_flag
		beq chkfkey						; game runs - check only f-keys
		lda atract_timer_ATARI
		bne checkey
		lda #$04
		bne SetState					; set ATARI attract mode state = 4 - NOT on Commodore
!ifdef 	P500{
checkey:ldy #$00
		lda (CIA),y						; load CIA Port A
		ora #$3f						; ignore bit#0-5
		cmp #$ff						; check if bit#6 and 7 = 1 -> no joystick button pressed
		bne newgame
chkfkey:lda pressed_key
		cmp #$07
		beq notgame						; no key pressed
keydebo:cmp pressed_key
		beq keydebo						; debounce key
		cmp #$03
		beq newgame						; F1 -> start new game
		ldx state
		cpx #$03
		bcc SetStateMenu				; set state to 3 = menu (at startup its 1)
		cmp #$06
		beq IncreaseDifficulty			; F5 -> increase difficulty
		cmp #$05
		beq TogglePlayers				; F3 -> toggle players
		bne notgame

} else{
checkey:lda #$10
		bit $dc00						; check CIA1 Porta column 4 = Joy 2 button
		beq newgame
chkfkey:lda pressed_key
		cmp #$ff
		beq notgame						; no key pressed
debounc:cmp pressed_key
		beq debounc						; debounce key
		cmp #$ef
		beq newgame						; F1, Joy1Button -> start new game
		ldx state
		cpx #$03
		bcc SetStateMenu				; set state to 3 = menu (at startup its 1)
		cmp #$bf
		beq IncreaseDifficulty			; F5 -> increase difficulty
		cmp #$df
		beq TogglePlayers				; F3 -> toggle players
		bne notgame
}
; start new game
newgame:lda #$00						; start new game
		sta game_over_flag
		sta state						; state = 0 game mode
		lda #$01
		sta restart_flag				; restart = 1 for new game
		jmp mainlp						; start the new game in next main loop
; -------------------------------------------------------------------------------------------------
; $855c toogle players
TogglePlayers:
		lda players
		eor #$01
		sta players
; $8562	set state=menu -> return to main loop	
SetStateMenu:
		lda #$03
SetState:
		sta state
		jmp notgame
; -------------------------------------------------------------------------------------------------
; $8569 increase difficulty		
IncreaseDifficulty:
		lda difficulty
		cmp #$02
		bcs idiff01						; branch if difficulty >= 2
		inc difficulty
		bne SetStateMenu				; return in menu state
idiff01:cmp #$0c
		beq idiff0c						; branch if state = $c
		inc difficulty
		inc difficulty					; from difficulty 2 add 2 for each step
		bne SetStateMenu				; return in menu state
idiff0c:lda #$00
		sta difficulty					; after $c reset difficulty to 0
		beq SetStateMenu				; return in menu state
; -------------------------------------------------------------------------------------------------
Interrupt: ; game interrupt routine every 20ms = 1 jiffy
!ifdef 	P500{
; p500 Interrupt routine
		pha
		txa
		pha
		tya
		pha
		lda IndirectBank				; load active indirect bank
		pha								; remember on stack
		lda #SYSTEMBANK
		sta IndirectBank				; select bank 15

		ldy #VR_IRQ
		lda (VIC),y						; load VIC interrupt reg and mask bit 1
		and #$01
		beq jendirq						; skip if source is not raster interrupt
		ldy #VR_RASTER
		lda (VIC),y						; set VIC raster reg again
		ldy state
		beq gamerun
		cmp #RASTERLINE4
		bmi notras4
		lda #RASTERLINE1
		bne update

notras4:cmp #RASTERLINE3
		bmi notras3
		lda #$c8
		ldy #VR_MCMCSX
		sta (VIC),y						; VIC multicolormode MCM=1, 40 columns
		lda #RASTERLINE4
		bne setrast

notras3:cmp #RASTERLINE2
		bmi ras1
		lda #$d8
		ldy #VR_MCMCSX
		sta (VIC),y						; VIC multicolormode MCM=0, 40 columns
		lda #RASTERLINE1
		bne setrast
		
ras1	lda #$c8
		ldy #VR_MCMCSX
		sta (VIC),y						; VIC multicolormode MCM=0, 40 columns
		lda #RASTERLINE2

setrast:ldy #VR_RASTER
		sta (VIC),y						; set VIC raster reg again
		lda #$81
		ldy #VR_IRQ
		sta (VIC),y						; clear VIC raster interrupt
jendirq:jmp endirq

gamerun:lda #RASTERLINE4
update:	ldy #VR_RASTER
		sta (VIC),y						; set VIC raster reg again
		lda #$d8
		ldy #VR_MCMCSX
		sta (VIC),y						; VIC multicolormode MCM=1, 40 columns
		lda #$81
		ldy #VR_IRQ
		sta (VIC),y						; clear VIC raster interrupt		
		inc jiffy						; increase jiffy
		dec bounce_timer_ATARI							;
		jsr UpdateScreen				; sub: Update screen: Startup, Menu, Game

		lda $a4
		bne ichkkey						; skip if $a4 is not 0
		jsr SpriteDirectionCompareLoop	; sprite direction compare loop

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
		jsr UpdateScreen				; sub: Update screen: Startup, Menu, Game
		lda $a4
		bne ichkkey						; skip if $a4 is not 0
		jsr SpriteDirectionCompareLoop	; sprite direction compare loop
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
; $85bd Init menu - set VIC, sound off, clears sprite x
InitMenu:
		lda #$00
		ldx #$07
-		sta sprite_x,x					; clear sprite x variables
		dex
		bpl -
		lda #$3a						; VM13-10=$3 screen $0c00, CB13,12,11,x=1010 char $2800						; VIC memory pointers
!ifdef 	P500{
		ldy #VR_MEMPT
		sta (VIC),y						; set VIC memory pointers
		jsr SoundOff					; returns with A=$00
		ldy #VR_MOBENA
		sta (VIC),y						; VIC disable sprites
		ldy #$ff						; set Y = $ff because its set to this value after SoundOff
		rts
} else{
}
		sta $d018						; set VIC memory pointers
		lda #$c8
		sta $d016						; set VIC Multicolor mode off, 40 Columns
		jsr SoundOff					; returns with A=$00
		sta $d015						; VIC disable sprites
		rts
; -------------------------------------------------------------------------------------------------
; $85d8 print 1Up, print zero score
Init1Player:
		ldx #$05						; 6 chars
		lda #$80						; <space> code
-		sta GameScreen+$20,x			; write to screen score position 1
		sta GameScreen+$47,x 			; write to screen score position 2
		dex
		bpl -
iplayr1:jsr Write1Up
		ldx #$05						; 6 chars
		lda #$90						; code 0
-		sta GameScreen+$2a,x			; write player 1 score 000000
		dex
		bpl -
		rts
; $85f3 print 1Up, 2Up, print zero scores
Init2Player:
		jsr Write2Up
		ldx #$05
		lda #$90
-		sta GameScreen+$47,x			; write player 2 score 000000
		dex
		bpl -
		bmi iplayr1
; -------------------------------------------------------------------------------------------------
; $8602 copy and uncompress user font
UncompressUserFont:	
		ldy #$00
		lda (pointer1),y				; load byte
		and #$c0
		bne ucfnt67						; branch if bit 6 or 7 = 1
		lda (pointer1),y				; load byte again
		tax								; move to X as index
		lda UserFontTiles,x				; load part 0-$3f from table
		jsr StoreIncPointer2			; store in user font
ucfntlp:jsr IncPointer1
		jmp UncompressUserFont			; next byte
ucfnt67:lsr								; shift bit#6+7 to 1+0
		lsr
		lsr
		lsr
		lsr
		lsr
		sta temp						; store in temp as repeat counter
		lda (pointer1),y				; load byte again
		cmp #$ff						; check if end of table
		beq ucfntrt						; branch to rts
		and #$3f
		tax
		lda UserFontTiles,x				; load part 0-$3f from table
ucfntrp:jsr StoreIncPointer2			; store in user font
		dec temp
		bpl ucfntrp						; repeat number of temp counter 
		bmi ucfntlp						; next byte
; -------------------------------------------------------------------------------------------------
; $8636 Load++ pointer 1
LoadIncPointer1:
		lda (pointer1),y
IncPointer1:
		inc pointer1
		bne +
		inc pointer1+1
ucfntrt:
+		rts
; -------------------------------------------------------------------------------------------------
; $863f Store++ pointer 2
StoreIncPointer2:
		sta (pointer2),y
		inc pointer2
		bne +
		inc pointer2+1
+		rts
; -------------------------------------------------------------------------------------------------
; $8648 Load byte from pointer 1 and shift high nibble to low
LoadHiNibblePointer1:
		lda (pointer1),y
		lsr
		lsr
		lsr
		lsr
		rts
; -------------------------------------------------------------------------------------------------
; $864f Update screen: Startup, Menu, Game
UpdateScreen:
		ldy state
		bne nogame
		jmp GameCycle					; state = 0 game in progress
nogame:	cpy #$01
		bne MenuDelay					; state > 1 menu delay $ff jiffys
; state = 1: Print startup screen
		jsr InitMenu					; sub: Init menu - set VIC, sound off, clears sprite x
		ldx #$00
		txa
		sta $02c5
		lda #$80
cmenulp:sta MenuScreen,x				; clear menu screen
		sta MenuScreen+$100,x
		sta MenuScreen+$200,x
		sta MenuScreen+$300,x
		sta GameScreen+$350,x			; clear lower part game screen
		dex
		bne cmenulp
		ldx #$0d
atarilp:lda Text_Atari1983,x			; write "Atari 1983" to screen
		sta MenuScreen+$267,x
		dex
		bpl atarilp
		inx								; X = $00
pacmlp:	txa								; write PACMAN logo to screen
		clc
		adc #$01
		sta MenuScreen+$5f,x			; first line char 1 - 12
		txa
		adc #$0d
		sta MenuScreen+$87,x			; second line char 13 - 24
		inx
		cpx #$0c
		bne pacmlp
nxstate:inc state						; increase state
mwait: 	rts
; -------------------------------------------------------------------------------------------------
; $8698 state = 2: 5 seconds Menu delay loop after startup
MenuDelay:
		cpy #$02
		bne Menu
		lda delay_menu
		cmp jiffy						; wait for $ff jiffys from startup
		bne mwait
		beq nxstate						; increase to next state = 3 Menu
; -------------------------------------------------------------------------------------------------
; $86a4 state = 3: Print menu screen
Menu:	ldx players
		inx								; add 1 to get 1/2
		txa
		ora #$90
		sta MenuScreen+$d7				; add $90 for char and print 1/2 players to screen
		lda players
		bne m2playr						; skip if 2 players
		cpy #$03
		bne m1nots3						; skip if not state 3
		jsr Init1Player					; sub: print 1Up, print zero score on game screen
m1nots3:lda #$92
		bne mskp2pl						; skip 2 player init
m2playr:cpy #$03
		bne m2nots3						; skip if not state 3
		jsr Init2Player					; sub: print 1Up, 2Up, print zero scores on game screen
m2nots3:lda #$91
mskp2pl:sta MenuScreen+$14f				; write 2/1 player for F3 to screen
		ldx #$0a
-		lda Text_PlayerGame,x
		sta MenuScreen+$151,x			; write "Player Game" twice 
		sta MenuScreen+$d9,x
		dex
		bpl -
		jsr InitMenu					; sub: Init menu - set VIC, sound off, clears sprite x
		ldx #$0b
-		lda Text_PressF3To,x			; write F-key messages to screen 
		sta MenuScreen+$127,x
		lda Text_PressF5To,x
		sta MenuScreen+$240,x
		lda Text_PressF1To,x
		sta MenuScreen+$2e0,x
		dex
		bpl -
		ldx difficulty
		lda DifficultyFruitsMenu,x
		sta MenuScreen+$1cc				; write two chars for the difficulty fruit
		clc
		adc #$01
		sta MenuScreen+$1cd
		ldx #$10
-		lda Text_ChangeDifficulty,x
		sta MenuScreen+$265,x			; write "change difficulty"
		dex
		bpl -
		ldx #$08
-		lda Text_PlayGame,x
		sta MenuScreen+$309,x			; write "play game"
		dex
		bpl -
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
sdpcopy:sta SpriteDataPointer,x
		sec
		sbc #$01
		dex
		bpl sdpcopy
; $874d copy pacman sprite data		
		lda monster_vpos+4					; set data pointer to $5345 (pacman $5348-$5351)
		sta spritedata_pointer
		lda #$53
		sta spritedata_pointer+1
		jsr SetPacmanDataEnd			; sub: Set last row, last byte of pacman
!ifdef 	P500{
		lda #GAMEBANK
		sta IndirectBank				; select bank 0 for $cx access
}
spmcopy:lda (spritedata_pointer),y		; copy pacman sprite data
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
sm0copy:lda (spritedata_pointer),y		; copy monster 0 sprite data
		sta SpriteData,x
		dex
		dex
		dex
		dey
		cpy #$01						; reach last byte
		bne sm0copy
		lda monster_vpos+1
		jsr SetMonsterDataEnd				; sub: Calc pointer, set last row, last byte of monster
sm1copy:lda (spritedata_pointer),y		; copy monster 1 sprite data
		sta SpriteData+$40,x
		dex
		dex
		dex
		dey
		cpy #$01						; reach last byte
		bne sm1copy
		lda monster_vpos+2
		jsr SetMonsterDataEnd				; sub: Calc pointer, set last row, last byte of monster
sm2copy:lda (spritedata_pointer),y		; copy monster 2 sprite data
		sta SpriteData+$80,x
		dex
		dex
		dex
		dey
		cpy #$01						; reach last byte
		bne sm2copy
		lda monster_vpos+3
		jsr SetMonsterDataEnd			; sub: Calc pointer, set last row, last byte of monster
sm3copy:lda (spritedata_pointer),y		; copy monster 3 sprite data
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
		bne chkpaus						; only every 16 cycles
!ifdef 	P500{
		ldy #$00						
		lda (CIA),y						; load CIA Port A - joystick button = pause
		ora #$3f						; ignore bit#0-5
		cmp #$ff						; check if bit#6 and 7 = 1 -> no joystick button pressed
		beq +
} else{
		lda $dc00
		and #$10
		bne +
}
		lda pause_flag
		eor #$80						; toggle pause -> $80 = pause
		sta pause_flag

+		lda atract_timer_ATARI
		beq chkpaus
		dec atract_timer_ATARI			; decrease attract timer - only for ATARI

chkpaus:lda pause_flag
		beq blk12up						; skip if 0 = no pause
		jmp SoundOff					; JUMP to Sound off in pause and return from game cycle!
blk12up:jsr Blink12Up					; sub: blink 1/2up of active player
		lda restart_flag
		beq l87d8
		cmp #$02
		beq l87f6						; branch if 2 to get ready
l87d8:	lda game_over_flag
		beq l87e6						
		lda players
		beq wr1up						; skip if 1 player
		jsr Write2Up
wr1up:	jmp Write1Up
; $87e6 
l87e6:	lda $14
		beq l87ed
		jmp l8c5c
l87ed:	lda ready_flag
		bne l8837
		lda intro_flag
		bne l880a
l87f5:	rts
; -------------------------------------------------------------------------------------------------
; $87f6
l87f6:	inc intro_flag
		lda #$00
		sta attract_ATARI				; NOT USED in Commodore - prevents Atari screen saver
		sta restart_flag
		jmp Ready						; sub: print READY: and level fruits
; -------------------------------------------------------------------------------------------------
; $8801 calc new monster data pointer
SetMonsterDataEnd:
		sta spritedata_pointer
		inc spritedata_pointer+1
; $8805 Set last row, last byte of pacman
SetPacmanDataEnd:
		ldy #$0c						; last sprite line monsters/pacman
		ldx #$22						; last data byte monsters/pacman row 11 byte 2
		rts
; -------------------------------------------------------------------------------------------------
; $880a
l880a:	lda jiffy
		and #$03
		bne l87f5						; frequency change every 4 jiffy
; Play music
		ldx notes_counter
		cpx #$40
		beq l8830						; skip if end of frequency table
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
		bne l87f5						; return if notes_counter not = $28
		jmp PlayerDead					; sub: Player dead: die-animation, decrease lives
; $8830
l8830:	inc ready_flag
l8832:	inc $8a
		jsr l8da2
; $ 8837
l8837:	lda reset_flag
		beq l885e
		cmp #$01
		bne l8851
		jsr l8a78
		lda swap_player_flag
		bne l87f5
		lda game_over_flag
		bne l87f5
		inc reset_flag
		lda #$40
		sta reset_timer
		rts
; -------------------------------------------------------------------------------------------------
; $8851
l8851:	lda reset_timer
		beq l8858
		dec reset_timer
		rts
l8858:	lda #$00
		sta reset_flag
		beq l8832
l885e:	jsr l959f
		lda $ac
		beq l886b
		jsr l8e84
		jmp l8f3e
l886b:	jsr l96ee
		lda $a4
		beq l8875
		jmp l908b
l8875:	jsr l967e
		jsr l93e9
		lda $14
		bne l88c6
!ifdef 	P500{
		ldy #VR_MOBMOB
		lda (VIC),y						; load VIC sprite-sprite collision reg
} else{
		lda $d01e						; load VIC sprite-sprite collision reg
}
		sta $bf
		and #$10
		beq l88c6
		ldx #$00						; start with sprite/monster 0
		ldy #$01
l888c:	lda $8a,x
		asl
		bmi l88be
		tya
		bit $bf
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
l88c6:	jmp l897b
l88c9:	lda $8a,x
		bmi l88d0
		jmp l8968
l88d0:	lda #$42
		sta $8a,x
		stx $a7
		inc $a8
		sec
		lda pacman_hpos
		sbc #$04
		sta $02d4
		clc
		ldy #$02
-		adc #$02
		sta $02d5,y
		dey
		bpl -
		adc #$02
		sta sprite_x,x
		lda #WHITE
		sta $02c7
!ifdef 	P500{
		stx temp
		ldy temp
		sta (VIC27),y					; set VIC sprite color = white (monster)
} else{
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
l8909:	sta score_pointer1
		sta score_pointer2
		lda #$53
		sta $27
		txa
		clc
		adc $27
		adc #$01
		sta $29
		lda $a8
		asl
		tay
		lda BlueScorePointers,y
		sta pointer1
		iny
		lda BlueScorePointers,y
		sta pointer1+1
!ifdef 	P500{
		lda #GAMEBANK
		sta IndirectBank				; select bank 0 for pointer operations
}
		ldy #$0f
-		lda (pointer1),y
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
		inc $a4
		lda #$84
		sta $a6
		lda #$02
		sta $a5
		lda $a8
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
l8968:	lda $66
		ora #$80
		sta $66
		jsr SoundOff
		lda #$01
		sta $6a
		sta $ac
		lda #$60
		sta $ad
l897b:	lda fruit_color
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
		bcc l8995
		lda #$0c						; limit to $0c
l8995:	tax
		lda FruitScoresIndex,x			; load index from table
		tax
		ldy #$00
l899c:	lda FruitScores,x
		sta GameScreen+$241,y						; fruit middle of screen
		inx
		iny
		cpy #$05
		bne l899c
		lda #$01
		sta fruit_score_timer
		lda #$40
		sta $5e
		lda #$00
		sta fruit_color
		sta fruit_timer
		sta fruit_display_flag
		lda #$01
		sta $b6
		lda #$10
		sta $b7
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
l89da:	lda $ac
		beq l89df
l89de:	rts
; -------------------------------------------------------------------------------------------------
; $89df
l89df:	jsr l8f9b
		lda $a4
		bne l89de
		jsr l90e3
		jsr l8c0d
		jsr l8c49
		lda $64
		beq l89f7
		lda #$00
		beq l8a00
l89f7:	lda $65
		bne l8a00
		ldx #$04
		jsr l95e0
l8a00:	sta $63
		lda #$00
		sta $64
		jsr l91bd
		lda #$00
		sta $65
		jsr l9384
		lda jiffy
		and #$03
		bne l8a19
		jsr l97f5
l8a19:	jsr l9617
		lda $64
		bne l8a77
		ldx #$03
l8a22:	lda $8a,x
		and #$7f
		beq l8a74
		lda jiffy
		and #$07
		bne l8a34
		lda $86,x
		beq l8a34
		dec $86,x
l8a34:	lda $8a,x
		and #$3f
		beq l8a74
		lda $8a,x
		asl
		bmi l8a74
		lda monster_vpos,x
		cmp #$74
		bne l8a4f
		lda monster_hpos,x
		cmp #$a7
		bcs l8a53
		cmp #$52
		bcc l8a53
l8a4f:	lda $8a,x
		bpl l8a6a
l8a53:	jsr l95e0
		cmp #$00
		bne l8a74
		ldy monster_delay,x
		cpy #$01
		bne l8a64
		sta monster_delay,x
		beq l8a71
l8a64:	lda #$01
		sta monster_delay,x
		bne l8a74
l8a6a:	jsr l95e0
		cmp #$00
		bne l8a74
l8a71:	jsr l9845
l8a74:	dex
		bpl l8a22
l8a77:	rts
; -------------------------------------------------------------------------------------------------
; $8a78
l8a78:	jsr l95aa
		lda swap_player_flag
		beq l8ae8
		ldx reset_timer
		beq ChangePlayer
		dec reset_timer
		rts
; -------------------------------------------------------------------------------------------------
; $8a86 change player
ChangePlayer:
		cmp #$01
		bne l8ab0
		ldx #$00
l8a8c:	lda Player1Save,x				; restore player  screen
		sta Playfield,x
		lda Player1Save+$100,x
		sta Playfield+$100,x
		lda Player1Save+$200,x
		sta Playfield+$200,x
		lda Player1Save+$300,x
		sta Playfield+$300,x
		inx
		bne l8a8c
		jsr Write2Up
		lda #$00						; change to player 1
		sta player_number
		beq l8ae3
l8ab0:	cmp #$02
		bne l8ada
		ldx #$00
l8ab6:	lda Player2Save,x				; restore player 2 screen
		sta Playfield,x
		lda Player2Save+$100,x
		sta Playfield+$100,x
		lda Player2Save+$200,x
		sta Playfield+$200,x
		lda Player2Save+$300,x
		sta Playfield+$300,x
		inx
		bne l8ab6
		jsr Write1Up
		lda #$01						; change to player 2
		sta player_number
		bne l8ae3
l8ada:	jsr l95aa
		lda #$00
		sta swap_player_flag
		beq l8b2b
l8ae3:	lda #$03
l8ae5:	sta swap_player_flag
		rts
; -------------------------------------------------------------------------------------------------
; $8ae8
l8ae8:	lda players
		beq l8b2b
		lda player_number
		bne l8b0d
		lda extra_pacman2
		beq l8b2b
		lda extra_pacman1
		bne l8b02
		lda #$02
		sta swap_player_flag
		lda #$30
		sta reset_timer
		bne l8b3a
l8b02:	jsr BackupGameScreen1			; sub: init game screen player1 $4400
		lda #$30
		sta reset_timer
		lda #$02
		bne l8ae5
l8b0d:	lda extra_pacman1
		beq l8b2b
		lda extra_pacman2
		bne l8b1f
		lda #$01
		sta swap_player_flag
		lda #$30
		sta reset_timer
		bne l8b3a
l8b1f:	jsr BackupGameScreen2			; sub: init game screen player2 $4800
		lda #$30
		sta reset_timer
		lda #$01
		sta swap_player_flag
l8b2a:	rts
; -------------------------------------------------------------------------------------------------
; $8b2b
l8b2b:	ldx player_number
		lda extra_pacman1,x
		beq l8b3a
		jsr InitNewGame					; sub: init new game
		jsr Ready						; sub: print READY: and difficulty fruits
		jmp PlayerDead					; sub: Player dead: die-animation, decrease lives
l8b3a:	lda #$2c
		ldx #$00
l8b3e:	sta GameScreen+$23d,x
		clc
		adc #$01
		inx
		cpx #$0e
		bne l8b3e
		lda #RED
		sta $02c7
!ifdef 	P500{
		ldy #VR_MOBCOL+4
		sta (VIC),y						; set VIC sprite 4 color = red (pacman)
} else{
		sta $d02b						; set VIC sprite 4 color = red (pacman)
}
		lda swap_player_flag
		bne l8b2a
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
		jsr Write1Up
		jmp l95aa
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
; $8b93
SpriteDirectionCompareLoop:
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
l8ba2:	sta ATARI_32
		txa
		cmp #$04
		bne l8bab
		lda #$ff
l8bab:	clc
		adc #$54
		sta ATARI_33
		lda #$ff
		sta $bd
		lda monster_hpos,x
		sta $be
		cmp #$c0
		bcc l8be2
		lda #$c0
l8bbe:	cmp $be
		beq l8c0c
		dec $be
		asl $bd
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
		cmp $be
		beq l8c0c
		inc $be
		lsr $bd
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
; $8c0d
l8c0d:	ldx $b6
		beq l8c48
		lda $b7
		ldy #$21
		cpx #$07
		bcs l8c28
		cmp #$0b
		beq l8c22
		sec
		sbc #$01
		bne l8c40
l8c22:	inc $b6
		ldy #$21
		bne l8c40
l8c28:	cpx #$09
		bne l8c30
		inc $b6
		bne l8c32
l8c30:	ldy #$21
l8c32:	clc
		cmp #$10
		bne l8c3e
		lda #$00
		sta $b6
		tay
		beq l8c40						; voice 1 = off
l8c3e:	adc #$01
l8c40:	sta $b7
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
l8c49:	lda jiffy
		and #$0f
		bne l8c5b
		lda $6b
		beq l8c59
		lda #$00
		sta $6b
		beq l8c5b
l8c59:	inc $6b
l8c5b:	rts
; -------------------------------------------------------------------------------------------------
; $8c5c
l8c5c:	lda $15
		bne l8c6d
		jsr SoundOff
		jsr _SpriteMovementAnimationLoop
		lda #$40
l8c68:	sta $16
l8c6a:	inc $15
		rts
; -------------------------------------------------------------------------------------------------
; $l8c6
l8c6d:	cmp #$01
		bne l8c8f
		lda $16
		bne l8c8c
		lda #$00
		ldx #$03
l8c79:	sta sprite_x,x
		dex
		bpl l8c79
		lda #LIGHTBLUE
!ifdef 	P500{
		ldy #VR_BGRCOL+1
		sta (VIC),y						; set VIC backgroundcolor 1 = lightblue					
} else{
		sta $d022						; set VIC backgroundcolor 1 = lightblue					
}
		lda #$07
		sta $17
		lda #$10
		bne l8c68
l8c8c:	dec $16
		rts
; -------------------------------------------------------------------------------------------------
; $8c8f
l8c8f:	cmp #$02
		bne l8caf
		lda $16
		bne l8c8c
		dec $17
		beq l8c6a
		lda $17
		clc
		lsr
		bcc l8ca5
		lda #$0e
		bne l8ca7
l8ca5:	lda #WHITE
l8ca7:
!ifdef 	P500{
		ldy #VR_BGRCOL+1
		sta (VIC),y						; set VIC backgroundcolor 1 = white					
} else{
		sta $d022						; set VIC backgroundcolor 1 = white
}
		lda #$10
		sta $16
		rts
; -------------------------------------------------------------------------------------------------
; $8caf
l8caf:	cmp #$03
		bne l8cb9
		jsr InitNewGame					; sub: init new game
		inc $15
		rts
; -------------------------------------------------------------------------------------------------
; $8cb9
l8cb9:	jsr l8e49
		ldx player_number
		inc extra_pacman1,x
		inc maze_count1,x
		jsr Ready						; sub: print READY: and difficulty fruits
		jsr PlayerDead					; sub: Player dead: die-animation, decrease lives
		lda #$00
		sta $14
		sta $15
		lda #$02
		sta reset_flag
		lda #$40
		sta reset_timer
		rts
; -------------------------------------------------------------------------------------------------
; $8cd7 print READY: and difficulty fruits
Ready:	lda #$22						; first READY: char ( $22-$2b )
		ldx #$00
readycp:sta GameScreen+$23f,x			; screen position for READY:
		clc
		adc #$01
		inx
		cpx #$0a						; 10 chars
		bne readycp
		ldx player_number
		lda maze_count1,x				; load player difficulty
		cmp #$06
		bcc l8cf6
		cmp #$0a
		bcs l8cf6
		ldy #$0d
		bne l8cf8
l8cf6:	ldy #$0a
l8cf8:	sty fruit_score_flag			; ATARI - only stored, not used on Commodore
		ldy #$00
!ifdef 	P500{				; Y already $00
		sty IndirectBank				; select bank 0 for pointer operations
}
		cmp #$06
		bcs l8d25						; branch if A >= $06
		sta temp
		lda #$e2
		sta pointer2
		lda #$07						; pointer2 = fruit screen position $07e2
		sta pointer2+1
		ldx #$00
!ifdef 	P500{				; X already $00
		stx IndirectBank				; select bank 0 for pointer operations
}
l8d0c:	lda FruitChars,x				; load fruit char from table
		sta (pointer2),y				; store fruit code to screen
		inc pointer2					; screen pointer to second char
		clc
		adc #$01						; add 1 to char code for second fruit char
		sta (pointer2),y
		cpx temp
		beq l8d51
		inx
		dec pointer2					; next fruit position to the left
		dec pointer2
		dec pointer2
		bne l8d0c
l8d25:	cmp #$12
		bcc l8d2b
		lda #$12						; A = max $12
l8d2b:	sec
		sbc #$06						; substract $06 -> value 0 - $0c
		sta temp
		sec
		lda #<(HighFruitChars)			; = $8a
		sbc temp
		sta pointer1
		lda #>(HighFruitChars)			; pointer to end of table = $9d8a (min -$0c)
		sbc #$00
		sta pointer1+1
		ldx #$00
l8d3f:	lda (pointer1),y
		sta GameScreen+$3d6,x
		clc
		adc #$01
		sta GameScreen+$3d7,x
		inx
		inx
		iny
		cpy #$07
		bne l8d3f
l8d51:
!ifdef 	P500{
		lda #SYSTEMBANK
		sta IndirectBank				; switch back to bank 15
}
		rts
; -------------------------------------------------------------------------------------------------
; $8d52 Player dead: die-animation, decrease lives
PlayerDead:
		jsr _SpriteMovementAnimationLoop
		ldx player_number
		dec extra_pacman1,x				; decrease lives of actual player
; $8d59 update lives display -> draw mini-pacmans
UpdateLivesDisplay:
		ldx player_number
		lda extra_pacman1,x
		ldx #$00						; char $00 = <space>
		ldy #$1b						; Char $1b = mini pacman	
		cmp #$03
		bne lives2						; branch if not 3 lives
		sty GameScreen+$3c8				; write mini-pacmans
wr2live:sty GameScreen+$3c6
wr1live:sty GameScreen+$3c4
		rts
; -------------------------------------------------------------------------------------------------
; $8d6f
lives2:	cmp #$02
		bne lives1						; branch if not 2 lives
		jsr clr1liv						; clear 3. mini-pacman
		jmp wr2live						; write 2 mini-pacmans
lives1:	cmp #$01						; branch if not 1 live = dead
		bne clr3liv						; dead -> clear all mini-pacmans
		jsr clr2liv						; clear 2.+3. mini-pacman
		jmp wr1live						; write 1 mini-pacman
clr3liv:stx GameScreen+$3c4
clr2liv:stx GameScreen+$3c6
clr1liv:stx GameScreen+$3c8
		rts
; -------------------------------------------------------------------------------------------------
; $8d8d
_SpriteMovementAnimationLoop:
		lda #$01
		sta $6a
		ldx #$03
l8d93:	lda monster_direction,x
		jsr _SpriteMovementAnimation
		dex
		bpl l8d93
		lda #$00
		sta $6a
		jmp l9318
l8da2:	ldx #$0d
		lda #$00
l8da6:	sta GameScreen+$23d,x
		dex
		bpl l8da6
		rts
; -------------------------------------------------------------------------------------------------
; $8dad blink 1/2up of active player
Blink12Up:
		lda jiffy
		and #$0f
		bne blnkskp						; blink only every 16. jiffy cycle
		lda blink_counter
		bne blink0						; branch if blink = 1 -> set to 0
		inc blink_counter				; increase blink
		bne +							; branch always
blink0:	lda #$00
		sta blink_counter				; set blink to 0
+		lda player_number
		beq wc1up						; branch to 1 player
		lda blink_counter
		bne Write2Up					; if blink counter > 0 write 2UP
		tax								; code 0 = <space>
		tay
		beq wclr2up
; $8dcb write 2UP
Write2Up:
		lda #$92						; code 2, U, P
		ldx #$b5
		ldy #$b0
wclr2up:sta GameScreen+$21				; write to screen right side
		stx GameScreen+$22
		sty GameScreen+$23
		rts
; $8ddb
wc1up:	lda blink_counter
		bne Write1Up					; if blink_counter > 0 write 1UP
		tax								; code 0 = <space>
		tay
		beq wclr1up						; clear 1UP on screen
; $8de3 write 1UP
Write1Up:
		lda #$91						; code 1, U, P
		ldx #$b5
		ldy #$b0
wclr1up:sta GameScreen+$04						; write to screen left side
		stx GameScreen+$05
		sty GameScreen+$06
blnkskp:rts
; -------------------------------------------------------------------------------------------------
; $8df3 init new game
InitNewGame:
		jsr ClearSpriteRAM				; clear sprite RAM at $3000, $5300
		ldx #$8a
-		sta $3b,x
		dex
		bne -							; clear ZP $3c - $c5
		jsr InitColors					; sub: init color RAM and VIC Sprite colors
		ldx player_number
		lda maze_count1,x				; load player difficulty
		cmp #$06
		bcc +
		lda #$06						; limit A to 6 and move it to Y
+		tay
		lda DifficultyTable1,y				; load from table as index
		tax
		lda Speed,x						; copy data from RAM to ZP with index
		sta $75
		lda DifficultyTable2,y
		tay
		ldx #$03
-		lda Speed,y
		sta $71,x
		dex
		bpl -
		ldx #$13
-		lda SpriteInitData,x
		sta pacman_screen_ptr,x
		dex
		bpl -
		ldy #$00
		jsr Init87_89
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
		jsr inzersc						; zero score
l8e49:	jsr InitGameScreen				; sub: copy game screen to screen RAM
		jsr UpdateLivesDisplay			; sub: update lives display -> draw mini-pacmans
		ldx player_number
		lda maze_count1,x
		tay
		bne +
		lda jiffy
		bpl +
-		jsr Init87_89
		jmp ++
+		iny
		bne -
++		ldx player_number
inzersc:lda #$0f						; zero score
		sta bigdot_status,x
		lda #$00
		sta fruit_counter,x
		sta dots_eaten_lo,x
		sta dots_eaten_hi,x
		rts
; -------------------------------------------------------------------------------------------------
; $8e72 copies data from table to $87-$89
Init87_89:
		cpy #$03
		bcc +
		ldy #$03
+		ldx #$02
-		lda BlueStartValues,y
		sta $87,x
		iny
		dex
		bpl -
		rts
; -------------------------------------------------------------------------------------------------
; $8e84
l8e84:	lda $ac
		cmp #$01
		bne l8ea2
		lda $ad
		beq l8e9e
		jsr l8c49
		ldx #$03
l8e93:	lda monster_direction,x
		jsr _SpriteMovementAnimation
		dex
		bpl l8e93
		dec $ad
		rts
; -------------------------------------------------------------------------------------------------
; $8e9e
l8e9e:	inc $ac
		lda $ac
l8ea2:	cmp #$02
		bne l8ec9
		ldx #$03
		lda #$00
l8eaa:	sta sprite_x,x
		dex
		bpl l8eaa
		lda #$25
		sta $af
		sta $ae
		jsr SoundOff
		sta $b0
		sta $aa
		inc $a9
		lda #$07
		sta $ab
		lda #$03
		sta $ac
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
		lda $af
		ldy #SR_V1FREQ+1
		sta (SID),y						; set SID voice 1 frequency hi
} else{
		sta $d404						; SID voice 1 control = sawtooth, on
		lda $af
		sta $d401						; set SID voice 1 frequency hi
}
		sec
		sbc #$02
		sta $af
		inc $b0
		lda $b0
		cmp #$04
		bne l8f33
		beq l8f14
l8ef2:	lda #$21
!ifdef 	P500{
		ldy #SR_V1CTRL
		sta (SID),y						; SID voice 1 control = sawtooth, on
		lda $af
		ldy #SR_V1FREQ+1
		sta (SID),y						; set SID voice 1 frequency hi
} else{
		sta $d404						; SID voice 1 control = sawtooth, on
		lda $af
		sta $d401						; set SID voice 1 frequency hi
}
		clc
		adc #$02
		sta $af
		dec $b0
		bne l8f33
		lda $ae
		cmp #$13
		beq l8f31
		sec
		sbc #$02
		sta $ae
		sta $af
		lda #$03
l8f14:	sta $ac
		rts
; -------------------------------------------------------------------------------------------------
; $8f17
l8f17:	lda #$21
!ifdef 	P500{
		ldy #SR_V1CTRL
		sta (SID),y						; SID voice 1 control = sawtooth, on
		lda $ae
		ldy #SR_V1FREQ+1
		sta (SID),y						; set SID voice 1 frequency hi
} else{
		sta $d404						; SID voice 1 control = sawtooth, on
		lda $ae
		sta $d401						; set SID voice 1 frequency hi
}
		clc
		adc #$02
		sta $ae
		cmp #$35
		bne l8f33
		jsr SoundOff
		lda #$80
		sta $b0
l8f31:	inc $ac
l8f33:	rts
; -------------------------------------------------------------------------------------------------
; $8f34
l8f34:	lda $b0
		bne l8f3b
		inc reset_flag
		rts
; -------------------------------------------------------------------------------------------------
; $8f3b
l8f3b:	dec $b0
		rts
; -------------------------------------------------------------------------------------------------
; $8f3e
l8f3e:	
		lda $a9
		beq l8f9a
		lda pacman_vpos
		sta pointer2
		lda #$53
		sta pointer2+1
		lda $ab
		beq l8f52
		dec $ab
		bpl l8f5e
l8f52:	lda $aa
		cmp #$0f
		beq l8f8f
		inc $aa
		lda #$05
		sta $ab
l8f5e:	lda $aa
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
		sta (pointer2),y
!ifdef 	P500{
		lda #SYSTEMBANK
		sta IndirectBank				; switch back to bank 15
}
		rts
; -------------------------------------------------------------------------------------------------
; $8f73
l8f73:	ldy #$0c			; already bank 0 selected
		ldx #$09
!ifdef 	P500{
		lda #GAMEBANK
		sta IndirectBank				; select bank 0 for pointer operations
}
l8f77:	lda PacmanTop+$0a,x
		sta (pointer2),y
		dey
		dex
		bpl l8f77
!ifdef 	P500{
		lda #SYSTEMBANK
		sta IndirectBank				; switch back to bank 15
}
		rts
; -------------------------------------------------------------------------------------------------
; $8f81
l8f81:	ldy #$0f			; already bank 0 selected
		ldx #$0f
!ifdef 	P500{
		lda #GAMEBANK
		sta IndirectBank				; select bank 0 for pointer operations
}
l8f85:	lda PacmanExplosion,x
		sta (pointer2),y
		dey
		dex
		bpl l8f85
!ifdef 	P500{
		lda #SYSTEMBANK
		sta IndirectBank				; switch back to bank 15
}
		rts
; -------------------------------------------------------------------------------------------------
; $8f8f
l8f8f:	ldy #$0f			; already bank 0 selected
		lda #$00
!ifdef 	P500{
		lda #GAMEBANK
		sta IndirectBank				; select bank 0 for pointer operations
}
l8f93:	sta (pointer2),y
		dey
		bpl l8f93
		sta $a9
!ifdef 	P500{
		lda #SYSTEMBANK
		sta IndirectBank				; switch back to bank 15
}
l8f9a:	rts
; -------------------------------------------------------------------------------------------------
; $8f9b
l8f9b:	lda $bb
		beq l8faf
		lda $b1
		bne l8fa6
		jsr l901e
l8fa6:	lda $bc
		beq l8fb6
		dec $bc
		jmp l9005
l8faf:	lda $b1
		bne l8f9a
		jmp l9558
l8fb6:	ldx player_number
		lda maze_count1,x
		tax
		lda FlashingTimerTable,x
		cmp $bb
		bne l8ff9
		ldx #$03						; 3-0 monsters
!ifdef 	P500{
		ldy #VR_MOBCOL+3
l8fc4:	lda $8a,x
		bpl l8fce
		lda SpriteColors,x
		sta (VIC),y						; set VIC sprite color from table
		dey
} else{
l8fc4:	lda $8a,x
		bpl l8fce
		lda SpriteColors,x
		sta $d027,x						; set VIC sprite color from table
}
l8fce:	dex
		bpl l8fc4
		ldx #$03
l8fd3:	lda $8a,x
		bpl l8fe9
		cmp #$80
		beq l8fe1
		and #$bb
		beq l8fe9
		bne l8fe5
l8fe1:	lda #$00
		beq l8fe7
l8fe5:	lda #$20
l8fe7:	sta $8a,x
l8fe9:	dex
		bpl l8fd3
		lda #$00
		sta $bb
		sta $ba
		sta $a3
		lda #$a0
		sta $69
		rts
; -------------------------------------------------------------------------------------------------
; $8ff9 Toggle monster color blue/white
l8ff9:	lda $ba
		bne l9003
		inc $bb
		lda #$18
		sta $ba
l9003:	dec $ba
l9005:	lda $bb
		lsr
		bcc mwhite						; color monsters white
!ifdef 	P500{
		ldx #BLUE
		bne +							; blue loaded, skip white
mwhite: ldx #WHITE
+		ldy #$03						; 3-0 monsters
mcolrlp:lda $8a,y
		bpl mskip						; skip reborn monster
		txa
		sta (VIC27),y					; set VIC monster sprites color 3-0
mskip:	dey
} else{
		ldy #BLUE
		bne +							; blue loaded, skip white
mwhite: ldy #WHITE
+		ldx #$03						; 3-0 monsters
mcolrlp:lda $8a,x
		bpl mskip						; skip reborn monster
		tya
		sta $d027,x						; set VIC monster sprites color 3-0
mskip:	dex
}
		bpl mcolrlp
		rts
; -------------------------------------------------------------------------------------------------
; $901e
l901e:	lda $a3
		bne l9030
		lda #$05
		sta $9f
		lda #$02
		sta $9e
		inc $a3
		lda #$0e
		sta $9d
l9030:	lda $9e
		cmp #$01
		bne l9055
		lda $9d
		cmp #$0e
		bne l904e
		lda $9f
		cmp #$03
		bne l9046
		lda #$05
		sta $9f
l9046:	dec $9f
		lda #$02
		sta $9e
		bne l9061
l904e:	sec
		lda $9d
		sbc #$03
		bne l9066
l9055:	lda $9d
		cmp #$20
		bne l9061
		lda #$01
		sta $9e
		bne l904e
l9061:	clc
		lda $9d
		adc #$03
l9066:	sta $9d
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
; $9071
l9071:	lda $b2
		bne l9079
		lda #$97
		bne l907f
l9079:	cmp #$64
		bne l907f
		lda #$97
l907f:	sec
		sbc #$03
		sta $b2
!ifdef 	P500{
		ldy #SR_V2FREQ+1
		sta (SID),y						; SID voice 2 frequency hi
} else{
		sta $d408						; SID voice 2 frequency hi
}
		lda #$21
		bne l906d
l908b:	dec $a5
		beq l90a4
		sec
		lda $a6
		sbc #$04
		sta $a6
		cmp #$10
		beq l90bc
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
		bne l90b7
l90a4:	lda #$02
		sta $a5
		sec
		lda $a6
		sbc #$03
		sta $a6
!ifdef 	P500{
		ldy #SR_V1FREQ+1
		sta (SID),y						; SID voice 1 frequency hi
		ldy #SR_V2FREQ+1
		sta (SID),y						; SID voice 2 frequency hi
		lda #$21
l90b7:	ldy #SR_V2CTRL
		sta (SID),y						; SID voice 2 control = sawtooth, on
} else{
		sta $d401						; SID voice 1 frequency hi
		sta $d408						; SID voice 2 frequency hi
		lda #$21
l90b7:	sta $d40b						; SID voice 2 control = sawtooth, on
}
		bne l9109
l90bc:	jsr SoundOff
		sta $a4
		sta $63
		sty $a7
		lda #$0f
		sta $02c7
!ifdef 	P500{
		ldy #$03						; 3-0 monsters
l90cc:	lda (VIC27),y					; load VIC sprites color 3-0 (monsters)
		cmp #$f1
		beq l90d7
		dey
} else{
		ldx #$03						; 3-0 monsters
l90cc:	lda $d027,x						; load VIC sprites color 3-0 (monsters)
		cmp #$f1
		beq l90d7
		dex
}
		bpl l90cc
		rts
; -------------------------------------------------------------------------------------------------
; $90d7
l90d7:	lda #$00
		sta $63
		jsr l91bd
		inc $64
		jmp l9384
l90e3:	lda $b3
		beq l910c
		ldx $b4
		cpx #$06
		bne l90f5
		lda #$00
		sta $b3
		sta $b4
		beq l9109
l90f5:	lda $b5
		bne l90ff
		lda EatingDotsSoundData1,x
		jmp l9102
l90ff:	lda EatingDotsSoundData2,x
l9102:	inc $b4
!ifdef 	P500{
		ldy #SR_V1FREQ+1
		sta (SID),y						; SID voice 1 frequency hi
		lda #$21
l9109:	ldy #SR_V1CTRL
		sta (SID),y						; SID voice 1 control = sawtooth, on
} else{
		sta $d401						; SID voice 1 frequency hi
		lda #$21
l9109:	sta $d404						; SID voice 1 control = sawtooth, on
}
l910c:	rts
; -------------------------------------------------------------------------------------------------
; $910d Stop SID sound
SoundOff:
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
; $911d clear RAM areas $3000-$31ff (Sprite data) and $5300-$57ff (sprite data source RAM)
ClearSpriteRAM:
		ldx #$00
		txa
clramlp:sta SpriteData,x
		sta SpriteData+$100,x
		sta SpriteRAM,x
		sta SpriteRAM+$100,x
		sta SpriteRAM+$200,x
		sta SpriteRAM+$300,x
		sta SpriteRAM+$400,x
		inx
		bne clramlp
		txa
		rts
; -------------------------------------------------------------------------------------------------
; $913a Init color RAM + Sprite colors
InitColors:
!ifdef 	P500{
		lda #YELLOW+MCM
		ldy #$00
coinlp1:sta (ColorRAM0),y				; init color RAM with yellow + bit#3 for multicolor
		sta (ColorRAM1),y
		sta (ColorRAM2),y
		sta (ColorRAM3),y
		dey
		bne coinlp1
		ldy #40*2 - 1
		lda #WHITE
coinlp2:sta (ColorRAM0),y				; init lines 0-1 with white
		dey
		bpl coinlp2
		ldy #7							; sprite 7-0
coinlp3:lda SpriteColors,y
		sta (VIC27),y					; init VIC Sprite colors from table
		dey
		bpl coinlp3
		rts
} else{
		ldx #$00
		lda #YELLOW+MCM
coinlp1:sta ColorRAM64,x				; init color RAM with yellow + bit#3 for multicolor
		sta ColorRAM64+$100,x
		sta ColorRAM64+$200,x
		sta ColorRAM64+$300,x
		dex
		bne coinlp1
		ldx #40*2 - 1
		lda #WHITE
coinlp2:sta ColorRAM64,x				; init lines 0-1 with white
		dex
		bpl coinlp2
		ldx #7							; sprite 7-0
coinlp3:lda SpriteColors,x
		sta $d027,x						; init VIC Sprite colors from table
		dex
		bpl coinlp3
		rts
}
; -------------------------------------------------------------------------------------------------
; $9163 copy game screen to screen RAM
InitGameScreen:
		ldx #$00
scrinlp:lda MazeData,x					; load decompressed Screen Data
		sta Playfield,x					; copy to game screen from line2
		lda MazeData+$100,x
		sta Playfield+$100,x
		lda MazeData+$200,x
		sta Playfield+$200,x
		lda MazeData+$300,x
		sta Playfield+$300,x
		inx
		bne scrinlp
		rts
; -------------------------------------------------------------------------------------------------
; $9181 backup game screen player 1 to $4400
BackupGameScreen1:
		ldx #$00
bscr1lp:lda Playfield,x
		sta Player1Save,x
		lda Playfield+$100,x
		sta Player1Save+$100,x
		lda Playfield+$200,x
		sta Player1Save+$200,x
		lda Playfield+$300,x
		sta Player1Save+$300,x
		inx
		bne bscr1lp
		rts
; -------------------------------------------------------------------------------------------------
; $919f backup game screen player 2 to $4800
BackupGameScreen2:
		ldx #$00
bscr2lp:lda Playfield,x
		sta Player2Save,x
		lda Playfield+$100,x
		sta Player2Save+$100,x
		lda Playfield+$200,x
		sta Player2Save+$200,x
		lda Playfield+$300,x
		sta Player2Save+$300,x
		inx
		bne bscr2lp
l91bc:	rts
; -------------------------------------------------------------------------------------------------
; $91bd
l91bd:	lda $66
		bmi l91bc
		ldx #$04
		jsr MazeHandler
		clc
		lda $68
		bit temp
		beq l91e3
		cmp pacman_direction
		beq l91df
		ora pacman_direction
		tay
		and #$03
		beq l91df
		tya
		and #$0c
		beq l91df
		sty $64
l91df:	lda $68
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
		sta $68
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
		jmp l9318
l9229:	lda $63
		bne l9252
		dec pacman_vpos
		dec pacman_vpos
		lda $62
		bne l923b
		lda #$03
		sta $62
		bne l9252
l923b:	dec $62
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
l9252:	ldy #$06
		jmp l931d
l9257:	lda $63
		bne l9280
		inc pacman_vpos
		inc pacman_vpos
		lda $62
		cmp #$03
		bne l927e
		lda #$00
		sta $62
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
l927e:	inc $62
l9280:	ldy #$08
		jmp l931d
l9285:	lda $63
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
l92b5:	ldy #$02
		bne l931d
l92b9:	lda $63
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
l92f0:	ldy #$04
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
		bne l9318
		ldy #$02						; right
l9314:	lda #$0a
		bne l9333
l9318:	ldy #$00						; point to pacdot
		tya
		beq l9333
l931d:	ldx $67
		bne l9325
		inc $67
		bne l9318
l9325:	dex
		lda PacmanIndex,x
		cpx #$02
		bne l9331
		ldx #$ff
		stx $67
l9331:	inc $67
l9333:	tax
		lda PacmanDataPointers,y
		sta pointer1
		iny
		lda PacmanDataPointers,y
		sta pointer1+1
		txa
		clc
		adc pointer1
		sta pointer1
		lda #$00
		adc pointer1+1
		sta pointer1+1
		ldy #$09
!ifdef 	P500{
		lda #GAMEBANK
		sta IndirectBank				; select bank 0 for pointer operations
}
l934d:	lda (pointer1),y
		sta PacmanBuffer+3,y
		dey
		bpl l934d
		lda #<PacmanBuffer
		sta pointer1
		lda #>PacmanBuffer
		sta pointer1+1
		lda pacman_vpos
		sta pointer2
		lda #$53
		sta pointer2+1
		clc
		lda pacman_hpos
		sta $02d4
		adc #$02
		sta $02d7
		adc #$02
		sta $02d6
		adc #$02
		sta $02d5
		ldy #$0f
l937c:	lda (pointer1),y
		sta (pointer2),y
		dey
		bpl l937c
!ifdef 	P500{
		lda #SYSTEMBANK
		sta IndirectBank				; switch back to bank 15
}
l9383:	rts
; -------------------------------------------------------------------------------------------------
; $9384
l9384:	lda pacman_byte_ctr
		bne l93e8
		lda $62
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
		sta $b3
		sta $65
		lda #$00
		sta $b4
		lda $b5
		bne l93c7
		lda #$01
		bne l93c9
l93c7:	lda #$00
l93c9:	sta $b5
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
		sta $14
l93e8:	rts
; -------------------------------------------------------------------------------------------------
; $93e9
l93e9:	ldx player_number
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
		lda #$01
		sta $bb
		lda #$ff
		sta $a8
		ldx player_number
		lda maze_count1,x
		tax
		lda BlueTimerValues,x
		sta $bc
		ldx #$03
l944b:	lda $8a,x
		asl
		bmi l9467
		lda $8a,x
		ora #$80
		sta $8a,x
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
		jmp UpdateLivesDisplay			; sub: update lives display -> draw mini-pacmans
; -------------------------------------------------------------------------------------------------
; $94de ATARI: Maze handler subroutine
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
l94e8:	cmp VTable,x
		beq l94fc
		dex
		bpl l94e8
		lda hpos_saver
		ldy #$09
l94f4:	cmp HTable,y
		beq l950d
		dey
		bpl l94f4
l94fc:	ldy #$09
		lda hpos_saver
l9500:	cmp HTable,y
		beq l9512
		dey
		bpl l9500
		lda #$0c
		clc
		bcc l9523
l950d:	lda #$03
		clc
		bcc l9523
l9512:	txa
		asl
		tax
		lda HorizontalTablePointers,x
		sta pointer1
		inx
		lda HorizontalTablePointers,x
		sta pointer1+1
!ifdef 	P500{
		lda #GAMEBANK
		sta IndirectBank				; select bank 0 for pointer operations
}
		lda (pointer1),y
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
l9546:	lda $8a,x
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
l9558:	lda #$00
		sta $a3
		lda $9c
		bne l956a
		lda #$28
		sta $9a
		lda #$01
		sta $9c
		sta $9b
l956a:	lda $9b
		cmp #$01
		bne l9583
		lda $9a
		cmp #$40
		bcc l957c
		lda #$02
		sta $9b
		bne l958f
l957c:	lda chase_whine_delta
		clc
		adc $9a
		bne l9594
l9583:	lda $9a
		cmp #$28
		bcs l958f
		lda #$01
		sta $9b
		bne l957c
l958f:	sec
		lda $9a
		sbc chase_whine_delta
l9594:	sta $9a
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
l959f:	lda jiffy
		and #$0f
		beq l95aa
		cmp #$08
		beq l95d1
		rts
; -------------------------------------------------------------------------------------------------
; $95aa
l95aa:	ldy #$02
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
; -------------------------------------------------------------------------------------------------
; $95d1
l95d1:	ldy #$00
		sty GameScreen+$cb
		sty GameScreen+$ec
		sty GameScreen+$2d3
l95dc:	sty GameScreen+$2f4
		rts
; -------------------------------------------------------------------------------------------------
; $95e0
l95e0:	dec $71,x
		beq l95e7
		lda #$ff
		rts
; -------------------------------------------------------------------------------------------------
; $95e7
l95e7:	lda $6c,x
		cmp #$03
		bne l95f1
		lda #$ff
		sta $6c,x
l95f1:	inc $6c,x
		ldy player_number
		lda $0000+maze_count1,y			; load maze in A
		cmp #$06
		bcc +
		lda #$06						; limit A to 6 and move it to Y
+		tay
		cpx #$04
		bne l9608
		lda DifficultyTable1,y
		bpl +							; skip always
l9608:	lda DifficultyTable2,y
+		clc
		adc $6c,x
		tay
		lda Speed,y
		sta $71,x
		lda #$00
		rts
; -------------------------------------------------------------------------------------------------
; $9617
l9617:	lda jiffy
		and #$07
		bne l9623
		lda $69
		beq l9623
		dec $69
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
l9653:	lda $8a,x
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
l9666:	lda $8a,x
		cmp #$08
		beq l9674
		cmp #$10
		beq l9674
		cmp #$20
		bne l9678
l9674:	lda #$02
		sta $8a,x
l9678:	dex
		bpl l9666
		stx $69
		rts
; -------------------------------------------------------------------------------------------------
; $967e
l967e:	lda fruit_score_timer
		beq l9696
		lda $5e
		bne l9693
		lda #$00
		sta fruit_score_timer
		ldx #$04
l968c:	sta GameScreen+$241,x
		dex
		bpl l968c
		rts
; -------------------------------------------------------------------------------------------------
; $9693
l9693:	dec $5e
l9695:	rts
; -------------------------------------------------------------------------------------------------
; $9696
l9696:	lda fruit_color
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
		sta fruit_color
		lda #FRUITDELAY					; load fruitdelay
		sta fruit_timer
		lda #$02
		sta fruit_display_flag
		rts
l96d4:	lda fruit_display_flag
		bne l96e7
		lda fruit_timer
		bne l96e7
		lda #$00
		sta GameScreen+$243
		sta GameScreen+$244
		sta fruit_color
		rts
l96e7:	dec fruit_timer
		bne l96ed
		dec fruit_display_flag
l96ed:	rts
; -------------------------------------------------------------------------------------------------
; $96ee ATARI pacman monster subroutines
l96ee:	lda #$00
		sta $b1
		ldx #$03
l96f4:	lda $8a,x
		cmp #$44
		beq l973e
		asl
		bpl l973e
		cpx $a7
		beq l973e
		inc $b1
		lda monster_hpos,x
		tay
		lda monster_vpos,x
		cpy #$7c
		bne l9728
		cmp #$64
		bne l9728
		lda monster_direction,x
		cmp #$01
		bne l971e
		lda #$04
		sta monster_direction,x
		lda monster_vpos,x
		bne l9728
l971e:	lda #$44
		sta $8a,x
		lda #$ff
		sta $8e,x
		bne l973e
l9728:	jsr MazeHandler
		bcc l9739
		clc
		lda #$7c
		sta $7e,x
		lda #$64
		sta $82,x
		jsr l99e9
l9739:	lda monster_direction,x
		jsr _SpriteMovementAnimation
l973e:	dex
		bpl l96f4
		lda $b1
		bne l974b
		lda #$00
		sta $b2
		beq l9752
l974b:	lda $a4
		bne l9752
		jsr l9071
l9752:	ldx #$03
l9754:	lda $8a,x
		clc
		lsr
		lsr
		lsr
		bcc l9784
		lda $8a,x
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
		lda $8a,x
		bne l9796
		lda #$01
		bne l97a2
l9796:	and #$80
		ora #$02
		bpl l97a2
		sta $69
		ldy #$08
		bne l97a4
l97a2:	ldy #$04
l97a4:	sty monster_direction,x
		sta $8a,x
		rts
; -------------------------------------------------------------------------------------------------
; $97a9
l97a9:	lda #$01
l97ab:	sta monster_direction,x
		jmp _SpriteMovementAnimation
l97b0:	lda $8a,x
		and #$0f
		sta $8a,x
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
l97f7:	lda $8a,x
		and #$7f
		bne l9820
		lda $86,x
		beq l9809
		dec $86,x
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
		jmp _SpriteMovementAnimation
l9845:	lda $8a,x
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
		lda $69
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
		inc $8e,x
l987f:	txa
		asl
		tay
		lda MonsterStartPointer,y
		sta pointer1
		iny
		lda MonsterStartPointer,y
		sta pointer1+1
		lda $8e,x
		tay
!ifdef 	P500{
		lda #GAMEBANK
		sta IndirectBank				; select bank 0 for pointer operations
}
		lda (pointer1),y
!ifdef 	P500{
		ldy #SYSTEMBANK
		sty IndirectBank				; switch back to bank 15
}
		cmp #$0f
		bne l98c3
		cpx #$00
		bne l98a1
		lda #$50
		sta $69
		jmp l993b
l98a1:	lda #$08
		sta $8a,x
		lda #$00
		sta $7a,x
!ifdef 	P500{
		ldy #SR_RANDOM
		lda (SID),y						; load random value SID register $1b
} else{
		lda $d41b						; load random value SID register $1b
}
		and #$0f						; calc random value from $0-$f
		sta $76,x
		asl
		tay
		lda PatternStartHV,y
		sta $7e,x
		iny
		lda PatternStartHV,y
		sta $82,x
		lda #$96
		sta $86,x
		bne l98c8
l98c3:	sta monster_direction,x
		jmp _SpriteMovementAnimation
l98c8:	lda $86,x
		bne l98cf
		jmp l9915
l98cf:	jsr MazeHandler
		bcs l98d7
l98d4:	jmp l9950
l98d7:	clc
		jsr l99e9
		lda $96,x
		bne l98d4
		lda $92,x
		bne l98d4
		lda #$10
		sta $8a,x
		clc
		bcc l98f6
l98ea:	lda $86,x
		beq l9915
		jsr MazeHandler
		bcc l98f6
		clc
		inc $7a,x
l98f6:	lda $76,x
		tay
		lda PatternIndex,y
		adc $7a,x
		tay
		lda Pattern,y
		bne l9910
		sta $7a,x
		lda $76,x
		tay
		lda PatternIndex,y
		tay
		lda Pattern,y
l9910:	sta monster_direction,x
		jmp l9950
l9915:	lda #$20
		sta $8a,x
		txa
		asl
		tay
		lda HomePositionHV,y
		sta $7e,x
		iny
		lda HomePositionHV,y
		sta $82,x
		jsr MazeHandler
		bcc l9950
		clc
		jsr l99e9
		lda $96,x
		bne l9950
		lda $92,x
		bne l9950
		jmp l98a1
l993b:	lda #$02
		sta $8a,x
l993f:	lda pacman_hpos
		sta $7e,x
		lda pacman_vpos
		sta $82,x
		jsr MazeHandler
		bcc l9950
		clc
		jsr l99e9
l9950:	lda monster_direction,x
_SpriteMovementAnimation:
		cmp #$01
		bne l9962
		ldy $6a
		bne l995e
		dec monster_vpos,x
		dec monster_vpos,x
l995e:	lda #$00
		beq l998f
l9962:	cmp #$02
		bne l9972
		ldy $6a
		bne l996e
		inc monster_vpos,x
		inc monster_vpos,x
l996e:	lda #$0a
		bne l998f
l9972:	cmp #$04
		bne l9980
		ldy $6a
		bne l997c
		dec monster_hpos,x
l997c:	lda #$14
		bne l998f
l9980:	cmp #$08
		bne l998e
		ldy $6a
		bne l998a
		inc monster_hpos,x
l998a:	lda #$1e
		bne l998f
l998e:	rts
; -------------------------------------------------------------------------------------------------
; $998f
l998f:	ldy $8a,x
		bpl l9995
		lda #$28
l9995:	tay
		lda $8a,x
		asl
		bmi l99a8
		tya
		ldy $6b
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
		sta pointer1
		lda #$00
		adc #>MonsterUp					; pointer to sprite data
		sta pointer1+1
		ldy #$09
!ifdef 	P500{
		lda #GAMEBANK
		sta IndirectBank				; select bank 0 for pointer operations
}	
l99bc:	lda (pointer1),y
		sta MonsterBuffer+2,y
		dey
		bpl l99bc
		lda #<MonsterBuffer
		sta pointer1
		lda #>MonsterBuffer
		sta pointer1+1
		lda monster_hpos,x
		sta sprite_x,x
		lda monster_vpos,x
		sta pointer2
		lda #$54
		sta pointer2+1
		txa
		clc
		adc pointer2+1
		sta pointer2+1
		ldy #$0f
l99e1:	lda (pointer1),y
		sta (pointer2),y
		dey
		bpl l99e1
!ifdef 	P500{
		lda #SYSTEMBANK
		sta IndirectBank				; switch back to bank 15
}
		rts
; -------------------------------------------------------------------------------------------------
; $99e9
l99e9:	lda monster_vpos,x
		cmp $82,x
		beq l9a01
		bcc l99f9
		lda $8a,x
		bmi l99fd
l99f5:	lda #$01
		bne l9a03
l99f9:	lda $8a,x
		bmi l99f5
l99fd:	lda #$02
		bne l9a03
l9a01:	lda #$00
l9a03:	sta $92,x
		lda monster_hpos,x
		cmp $7e,x
		beq l9a1d
		bcs l9a15
		lda $8a,x
		bmi l9a19
l9a11:	lda #$08
		bne l9a1f
l9a15:	lda $8a,x
		bmi l9a11
l9a19:	lda #$04
		bne l9a1f
l9a1d:	lda #$00
l9a1f:	sta $96,x
		lda $92,x
		beq l9a3e
		bit temp
		beq l9a3e
		lda $96,x
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
l9a36:	lda $92,x
		bne l9a64
l9a3a:	lda $96,x
		bne l9a64
l9a3e:	lda $96,x
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
		sta $8a,x
		lda #$a0
		sta $69
l9b0f:	rts
; ***************************************** ZONE DATA2 ********************************************
!zone data2
!source "pm500dat.b"				; C64 + P500 data
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