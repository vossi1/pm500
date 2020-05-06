; Disassembled by Vossi 04/2020
; Prepared for ACME reassembling
; Comments by Vossi 05/2020
; Converted for P500 by Vossi 05/2020
!cpu 6502
; switches
;P500 = 1		; P500 bank 0 file
;CRT = 1			; CRT header for VICE
!ifdef 	P500{!to "pm500.prg", cbm
} else	{ !ifdef CRT {!to "pm500.crt", plain
		} else{ !to "pm500.rom", plain }}
; ########################################### TODO ################################################
;
; VIC, CIA, SID, Color RAM indirect access
;
; #################################################################################################
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
YELLOW					= $07
ORANGE					= $08
BROWN					= $09
LIGHTRED				= $0a
LIGHTBLUE				= $0e
GRAY3					= $0f
; game
LIVES					= 3			; start lives
; ************************************** P500 REGISTER ********************************************
VR_MODEY				= $11
VR_RASTER				= $12
VR_EIRQ					= $1a
; ************************************** P500 ADDRESSES *******************************************
!addr CodeBank			= $00		; code bank register
!addr IndirectBank		= $01		; indirect bank register
!addr CharROMbase		= $c000		; Character ROM
!addr ScreenRAM			= $d000		; Screen RAM
!addr ColorRAMbase		= $d400						; set SID voice 1 frequency lo		; Color RAM
!addr VICbase			= $d800		; VIC
!addr SIDbase			= $da00		; SID
!addr CIAbase			= $dc00		; CIA
!addr TPI1base			= $de00		; TPI1
!addr TPI2base			= $df00		; TPI2
; *************************************** C64 ADDRESSES *******************************************
!addr CPUPort64			= $01		; 6510 CPU port
!addr CharROM64			= $d000		; Character RAM
!addr ColorRAM64		= $d800		; Color RAM
!addr ioinit			= $fda3		; Kernal IRQ init
!addr ramtas			= $fd50		; Kernal RAM init
!addr restor			= $fd15		; Kernal hardware I/O vector init
!addr cint				= $ff5b		; Kernal video init
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
; ************************************** P500 ZERO PAGE *******************************************
!addr ColorRAM			= $f0
!addr VIC				= $f2
!addr SID				= $f4
!addr CIA				= $f6
!addr TPI1				= $f8
!addr TPI2				= $fa
!addr CharROM0			= $fc
!addr CharROM1			= $fe
; ****************************************** MACROS ***********************************************
; **************************************** CRT HEADER *********************************************
!ifdef 	CRT{
*= $7fb0
		!byte $43, $36, $34, $20, $43, $41, $52, $54
		!byte $52, $49, $44, $47, $45, $20, $20, $20
		!byte $00, $00, $00, $40, $01, $00, $00, $00
		!byte $00, $01, $00, $00, $00, $00, $00, $00
		!byte $50, $61, $63, $2d, $4d, $61, $6e, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00
		!byte $43, $48, $49, $50, $00, $00, $20, $10
		!byte $00, $00, $00, $00, $80, $00, $20, $00
}
; ***************************************** ZONE MAIN *********************************************
!zone main
!initmem FILL
*= $8000
!ifdef 	P500{
		jsr InitP500
		jmp Start
} else{ 	
; ROM ident
		!byte <Start, >Start, <Warm, >Warm	; ROM start addresses
		!byte $c3, $c2, $cd, "8"			; cbm-rom ident-bytes 'C'= with init, 'BM', '8' = 4k-block 8
}
; ***************************************** ZONE DATA1 ********************************************
!zone data1
*= $8008
; table
		!byte $30, $02, $bb, $5a, $30, $5f, $ee, $3d
		!byte $a8
; -------------------------------------------------------------------------------------------------
; $8011 Compressed game user font (bytes 0-$3f from FontData, bit 6+7 = count)
cUserFontGame:
		!byte $c0, $c0, $80, $57, $80, $57, $ee, $57
		!byte $40, $01, $c4, $04, $40, $19, $ca, $ca
		!byte $4a, $19, $40, $c4, $04, $01, $c0, $1e
		!byte $40, $1b, $44, $40, $19, $4a, $19, $c0
		!byte $1e, $40, $05, $4a, $40, $01, $44, $01
		!byte $c0, $2e, $c0, $80, $1e, $40, $1e, $40
		!byte $c4, $c4, $40, $1e, $c0, $00, $ca, $ca
		!byte $c0, $00, $1e, $c0, $40, $c1, $c1, $c0
		!byte $c1, $c1, $d9, $d9, $40, $1e, $40, $1b
		!byte $44, $40, $1e, $40, $05, $4a, $44, $1b
		!byte $40, $1e, $40, $4a, $05, $40, $1e, $40
		!byte $4a, $05, $40, $05, $4a, $44, $1b, $40
		!byte $1b, $44, $17, $2e, $2f, $09, $2f, $2e
		!byte $17, $80, $0d, $19, $00, $05, $ca, $05
		!byte $00, $19, $0d, $c0, $1d, $01, $00, $1b
		!byte $c4, $1b, $00, $01, $1d, $c0, $40, $d9
		!byte $d9, $c0, $00, $2f, $57, $2f, $57, $00
		!byte $00, $2b, $57, $6b, $17, $40, $2e, $2b
		!byte $2e, $6b, $2e, $40, $25, $03, $89, $28
		!byte $40, $2b, $2d, $49, $2e, $09, $40, $2f
		!byte $d7, $2f, $40, $25, $2b, $57, $2b, $25
		!byte $40, $6b, $2f, $89, $40, $6c, $26, $00
		!byte $43, $40, $a5, $00, $65, $40, $2f, $6b
		!byte $2c, $2b, $2f, $40, $2b, $00, $03, $ac
		!byte $40, $17, $2e, $66, $2e, $26, $40, $49
		!byte $e8, $40, $03, $28, $2e, $15, $43, $40
		!byte $e8, $68, $40, $2d, $00, $2b, $40, $2d
		!byte $80, $c3, $80, $2e, $e6, $2e, $40, $09
		!byte $a8, $26, $80, $d7, $2b, $25, $40, $2e
		!byte $2b, $2e, $6b, $2e, $40, $28, $c9, $28
		!byte $40, $2d, $49, $2d, $17, $09, $40, $03
		!byte $30, $91, $16, $11, $14, $2b, $14, $63
		!byte $2a, $23, $00, $03, $09, $11, $13, $11
		!byte $12, $07, $02, $00, $25, $21, $29, $61
		!byte $1f, $00, $42, $09, $ef, $09, $40, $25
		!byte $eb, $25, $03, $07, $d1, $11, $07, $00
		!byte $1f, $61, $69, $21, $1f, $43, $07, $d1
		!byte $07, $40, $1f, $e1, $1f, $43, $07, $d1
		!byte $07, $40, $1f, $e1, $1f, $9e, $4d, $05
		!byte $43, $9d, $5b, $19, $40, $9e, $4d, $05
		!byte $43, $9d, $5b, $19, $40, $03, $08, $af
		!byte $16, $12, $07, $00, $25, $ab, $24, $21
		!byte $1f, $07, $06, $07, $c3, $03, $21, $0e
		!byte $21, $25, $2b, $25, $2b, $25, $00, $03
		!byte $c0, $03, $00, $25, $e6, $26, $2b, $00
		!byte $2f, $40, $03, $00, $14, $09, $00, $2b
		!byte $15, $66, $55, $25, $00, $2f, $14, $2f
		!byte $40, $14, $09, $00, $2b, $03, $26, $95
		!byte $25, $00, $2f, $40, $03, $b0, $00, $2b
		!byte $15, $26, $83, $40, $03, $09, $c3, $09
		!byte $00, $03, $f0, $30, $26, $00, $2f, $25
		!byte $00, $09, $14, $25, $2e, $00, $03, $67
		!byte $b0, $26, $00, $2e, $00, $03, $09, $00
		!byte $25, $2f, $00, $26, $27, $70, $67, $03
		!byte $00, $2e, $25, $2e, $40, $25, $2f, $00
		!byte $26, $70, $a7, $03, $00, $25, $d5, $15
		!byte $25, $00, $2b, $f0, $30, $2b, $00, $17
		!byte $e6, $26, $17, $00, $40, $1e, $40, $1d
		!byte $c1, $1d, $40, $1e, $c0, $1e, $40, $0d
		!byte $d9, $0d, $40, $1e, $40, $ff
; -------------------------------------------------------------------------------------------------
; $81ef Compressed map data
cMapData:
		!byte $00, $11, $1c, $8f, $0c, $15, $16, $8f
		!byte $0c, $1e, $20, $00, $00, $13, $0f, $8f
		!byte $01, $0d, $0f, $8f, $01, $0d, $14, $00
		!byte $00, $13, $cf, $13, $82, $0e, $c4, $13
		!byte $85, $0e, $c4, $1d, $cf, $13, $85, $0e
		!byte $c4, $13, $82, $0e, $c4, $1d, $14, $00
		!byte $00, $13, $cf, $26, $82, $10, $c5, $16
		!byte $85, $10, $c5, $16, $c5, $16, $85, $10
		!byte $c5, $16, $82, $10, $c5, $2d, $14, $00
		!byte $00, $13, $0f, $a1, $01, $0d, $14, $00
		!byte $00, $13, $cf, $1a, $82, $0c, $08, $c1
		!byte $3e, $c4, $1a, $83, $0c, $07, $09, $83
		!byte $0c, $c8, $13, $ce, $41, $0a, $82, $0c
		!byte $c8, $1d, $14, $00, $00, $13, $0f, $86
		!byte $01, $cd, $0f, $85, $01, $0d, $0f, $85
		!byte $01, $cd, $0f, $86, $01, $0d, $14, $00
		!byte $00, $12, $1d, $83, $0c, $61, $c4, $1d
		!byte $00, $19, $83, $0c, $c8, $06, $c5, $0a
		!byte $83, $0c, $1a, $c0, $f1, $03, $63, $83
		!byte $0c, $1f, $21, $87, $00, $13, $cf, $1d
		!byte $00, $0f, $8d, $00, $cd, $0f, $01, $0d
		!byte $14, $88, $00, $84, $0c, $62, $c5, $16
		!byte $10, $c5, $03, $83, $0e, $0b, $0b, $83
		!byte $0e, $04, $00, $06, $10, $c5, $16, $64
		!byte $84, $0c, $8a, $00, $01, $83, $00, $0d
		!byte $89, $00, $0f, $83, $00, $01, $8a, $00
		!byte $84, $0c, $61, $c4, $13, $ce, $40, $06
		!byte $89, $10, $c5, $03, $ce, $41, $03, $63
		!byte $84, $0c, $88, $00, $13, $cf, $1d, $00
		!byte $0f, $8d, $00, $cd, $0f, $01, $0d, $14
		!byte $87, $00, $11, $1c, $83, $0c, $62, $c5
		!byte $16, $10, $c5, $0a, $83, $0c, $07, $09
		!byte $83, $0c, $c8, $06, $10, $c5, $16, $64
		!byte $83, $0c, $1e, $20, $00, $00, $13, $0f
		!byte $8f, $01, $0d, $0f, $8f, $01, $0d, $14
		!byte $00, $00, $13, $cf, $1a, $0c, $0c, $15
		!byte $c4, $1a, $85, $0c, $c8, $16, $c5, $1a
		!byte $85, $0c, $c8, $13, $16, $cc, $c8, $01
		!byte $0d, $14, $00, $00, $13, $0f, $02, $82
		!byte $01, $0d, $0f, $89, $01, $00, $00, $89
		!byte $01, $0d, $0f, $82, $01, $02, $0d, $14
		!byte $00, $00, $13, $19, $cc, $c8, $c1, $65
		!byte $c1, $3e, $c4, $1a, $83, $0c, $07, $09
		!byte $83, $0c, $c8, $13, $ce, $41, $c6, $51
		!byte $ca, $cc, $1a, $14, $00, $00, $13, $0f
		!byte $86, $01, $cd, $0f, $85, $01, $0d, $0f
		!byte $85, $01, $cd, $0f, $86, $01, $0d, $14
		!byte $00, $00, $13, $cf, $1a, $84, $0c, $17
		!byte $10, $18, $83, $0c, $c8, $16, $c5, $1a
		!byte $83, $0c, $17, $10, $18, $84, $0c, $c8
		!byte $1d, $14, $00, $00, $13, $0f, $a1, $01
		!byte $0d, $14, $00, $00, $12, $1d, $a1, $0c
		!byte $1f, $21, $a8, $00, $ff
; ***************************************** ZONE CODE *********************************************
!zone code
*= $8394 ; game code
Start:
!ifdef 	P500{ 
		jsr Test
		nop
		nop
		nop
		nop
		nop
		lda #GAMEBANK
		sta IndirectBank				; select bank 0
} else{ ; 12 bytes
		jsr ioinit 						; IRQ init
		jsr ramtas 						; RAM init
		jsr restor 						; hardware I/O vector init
		jsr cint   						; video init
}
Warm:	lda #$00
		ldx #$c2
clrzplp:sta $02,x						; clear zero page $03 - $c4
		sta GameScreen-1,x				; clear top of game screen $0400 - $04c1
		dex
		bne clrzplp						; next byte
		inc state						; increase state to 1 to start in menu mode
		ldx jiffy						; load jiffy clock low byte
		dex
		stx jiffy_start					; store jiffy - 1 in $0b
; $83b3 Copy and uncompress map
		lda #<MapData
		sta pointer2
		lda #>MapData
		sta pointer2+1					; set target pointer = MapData
		lda #<cMapData
		sta pointer1
		lda #>cMapData
		sta pointer1+1					; set source pointer = compressed MapData
		ldy #$00
mapcplp:lda (pointer1),y				; load byte from map
		sta temp						; store for later bit#6 check
		bmi mapbit7						; skip if bit#7 = 1
		bpl mapchar						; branch if normal char
mapbit7:cmp #$ff						; check if $ff = end of data
		beq mapend						; exit loop
		bit temp
		bvs mapbit6						; branch if bit#6 = 1
		and #$7f						; clear ident bit#7
		tax								; use value in X as repeat counter
		jsr IncPointer1					; read next byte
		jsr LoadIncPointer1
maprptb:jsr StoreIncPointer2			; copy byte to map
		dex
		bpl maprptb						; repeat store byte X times
		bmi mapcplp						; read next byte
mapbit6:and #$3f						; clear ident bits#7,6
		jsr StoreIncPointer2			; store byte to map
		jsr IncPointer1					; next byte
		jsr LoadHiNibblePointer1		; load and shift high nibble 4 bits right
		jsr StoreIncPointer2			; store hi nibble
		lda (pointer1),y
		and #$0f						; isolate low nibble
mapchar:jsr StoreIncPointer2			; copy byte to map
		jsr IncPointer1
		jmp mapcplp						; read next byte
; $8401 Copy and decode LookUpTable Nibbles
mapend:	lda #<cLookUpTable
		sta pointer1
		lda #>cLookUpTable
		sta pointer1+1					; set source pointer = $9f0e
		lda #<LookUpTable
		sta pointer2
		lda #>LookUpTable
		sta pointer2+1					; set target pointer = LookUpTable
		ldy #$00
		ldx #$00
ludeclp:jsr LoadHiNibblePointer1		; load and shift high nibble 4 bits right
		jsr StoreIncPointer2			; store high nibble
		jsr LoadIncPointer1
		and #$0f
		jsr StoreIncPointer2			; store low nibble
		inx
		bne ludeclp						; next byte
; $8426 Copy chars $00-$7f of the first (graphic) fontset to $80 of the custom fonts
!ifdef 	P500{ ; 25 bytes
		lda #SYSTEMBANK					; select bank 15 to get font from char ROM
		sta IndirectBank
fntcplp:lda (CharROM1),y				; load from character ROM - Y already $00	
		sta CharGame+$400,y				; store to game fontset from char $80
		sta CharMenu+$400,y				; store to menu fontset from char $80
		lda (CharROM0),y
		sta CharGame+$500,y
		sta CharMenu+$500,y
		dey
		bne fntcplp
		sty IndirectBank				; select bank 0 - Y already $00
!fill 26, NOPCODE
} else{ ; 51 bytes
		lda $dc0e						; stop CIA1 timer A to prevent problems when switching CharROM 
		and #$fe
		sta $dc0e
		lda CPUPort64					; enable character ROM
		and #$fb
		sta CPUPort64
		ldx #$00
fntcplp:lda CharROM64+$100,x			; load from character ROM
		sta CharGame+$400,x				; store to game fontset from char $80
		sta CharMenu+$400,x				; store to menu fontset from char $80
		lda CharROM64,x
		sta CharGame+$500,x
		sta CharMenu+$500,x
		dex
		bne fntcplp
		lda CPUPort64					; disable character ROM
		ora #$04
		sta CPUPort64
		lda $dc0e						; start CIA1 timer A
		ora #$01
		sta $dc0e
}
; $8459 Copy and uncompress user fonts
		lda #<cUserFontGame
		sta pointer1
		lda #>cUserFontGame
		sta pointer1+1					; pointer1 = $8011
		lda #<CharGame
		sta pointer2
		lda #>CharGame
		sta pointer2+1					; pointer2 = $2000
		jsr UncompressUserFont			; copy game user font
		lda #<cUserFontMenu
		sta pointer1
		lda #>cUserFontMenu
		sta pointer1+1					; pointer1 = $9e82
		lda #<CharMenu+$08
		sta pointer2
		lda #>CharMenu
		sta pointer2+1					; pointer2 = $2808
		jsr UncompressUserFont			; copy menu user font
; $847f Copy user chars to menu user font
		ldx #$1f
uccplp1:lda UserCharMenu,x				; copy 32 bytes to menu user font
		sta CharMenu+$a8,x
		dex
		bpl uccplp1						; next byte
; $848a SID init						; x already $ff
		stx $d40e						; SID voice 3 frequency lo to $ff 
		stx $d40f						; SID voice 3 frequency hi to $ff 
		lda #$80
		sta $d412						; SID voice 3 to $80 = noise for random generation
		lda #$f0
		sta $d406						; SID voice 1 SR to $f0
		sta $d40d						; SID voice 2 SR To $f0
; $849d Copy user chars from game to menu user font
		ldx #$a1						; copy $a1 bytes
uccplp2:lda CharGame+$1cf,x
		sta CharMenu+$1cf,x
		dex
		bne uccplp2
; $84a8 Interrupt vector setup
!ifdef 	P500{ ; 27 bytes
		lda #SYSTEMBANK
		sta IndirectBank				; select bank 15
		ldy #VR_EIRQ
		lda #$01
		sta (VIC),y						; VIC enable raster interrupt
		ldy #VR_MODEY
		lda #$1b
		sta (VIC),y						; VIC RC8 = 0, DEN, 40 columns, Y = 3
		ldy #VR_RASTER
		lda #$32
		sta (VIC),y						; VIC raster reg = $032 (start visible screen)
		lda #GAMEBANK
		sta IndirectBank				; select bank 15
		cli								; enable interrupts
} else{ ; 27 bytes
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
		cli								; enable interrupts
}
; -------------------------------------------------------------------------------------------------
; $84c3 Main loop		
mainlp:	lda #$00
		ldx #$1f
clvarlp:sta $0e,x						; clear game variables $0e-$2d
		dex
		bpl clvarlp
		txs								; init stack with $ff
		jsr SoundOff					; SID sound off
		sta $d020						; VIC exterior color = black
		sta $d021						; VIC background color0 = black
		lda #LIGHTBLUE
		sta $d022						; VIC background color1 = lightblue
		jsr ColorInit					; init color RAM and VIC Sprite colors
		jsr InitGameScreen				; copy game screen to screen RAM
		jsr BackupGameScreen1			; init game screen player1 $4400
		jsr BackupGameScreen2			; init game screen player2 $4800
		lda state						; state at startup = 1
		bne notgame						; branch if not game state = 0
; start new game
		lda jiffy
waitlp :cmp jiffy
		beq waitlp						; wait one jiffy = 20ms
		lda #$18						; VM13-10=$1 screen at $0400, CB13,12,11,x=1000 char at $2000
		sta $d018						; set VIC memory pointers
		lda #$d8
		sta $d016						; VIC multicolormode MCM=1, 40 columns
		lda #$1f
		sta $d015						; VIC enable spritess 0-4
		sta $d01d						; VIC x-expand sprites 0-4
		jsr InitNewGame
		jsr InitGameVariables
		lda players
		beq new1up						; skip if 1 player
		jsr Init2Player					; write 1up, 2up, score=0
		jmp new2up						; skip if 2 player
;		
new1up: jsr Init1Player					; write 1up, score=0
new2up: lda #$02
		sta $0a							; $0a=2 if 2 players
notgame:lda state
		bne checkey						; branch if not game state
		lda $0e
		beq chkfkey						; game runs - check only f-keys
		lda $0c
		bne checkey
		lda #$04
		bne SetState4					; set state to 4
checkey:lda #$10
		bit $dc00						; check CIA1 Porta column 4 = Joy 2 button
		beq mnewgam
chkfkey:lda pressed_key
		cmp #$ff
		beq notgame						; no key pressed
keydebo:cmp pressed_key
		beq keydebo						; debounce key
		cmp #$ef
		beq mnewgam						; F1, Joy1Button -> start new game
		ldx state
		cpx #$03
		bcc SetStateMenu				; set state to 3 = menu (at startup its 1)
		cmp #$bf
		beq IncreaseDifficulty			; F5 -> increase difficulty
		cmp #$df
		beq TogglePlayers				; F3 -> toggle players
		bne notgame
; start new game
mnewgam:lda #$00						; start new game
		sta $0e
		sta state						; state = 0 game mode
		lda #$01
		sta $0a
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
SetState4:
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
; $8583 interrupt 
Interrupt:
		lda $d019						; load VIC interrupt reg and mask bit 1
		and #$01
		beq inorast						; skip if source is not raster interrupt
		inc jiffy						; increase jiffy
		lda #$32
		sta $d012						; set VIC raster reg again to $32 (start)
		lda #$81
		sta $d019						; clear VIC raster interrupt
		dec $30							;
		jsr l864f				; draw screen
		lda $a4
		bne iskpspr						; skip if $a4 is not 0
		jsr l8b93				; sprite direction compare loop
iskpspr:ldx #$ff
		stx $dc02						; set CIA1 port A for output
		dex
		stx $dc00						; store $fe to Port A = select keyboard column 0
idebkey:lda $dc01						; load CIA1 port B = keyboard row
		cmp $dc01
		bne idebkey						; debounce key
		sta pressed_key					; store pressed key $ef=F1,JoyButton1 / $df=F3 / $bf=F5
		ldx #$00
		stx $dc02						; reset CIA1 port B to input
inorast:jmp $ea7e						; jump to kernal interrupt
; -------------------------------------------------------------------------------------------------
; $85bd
InitMenu:
		lda #$00
		ldx #$07
-		sta sprite_x,x
		dex
		bpl -
		lda #$3a						; VM13-10=$3 screen $0a00, CB13,12,11,x=1010 char $2800						; VIC memory pointers
		sta $d018						; set VIC memory pointers
		lda #$c8
		sta $d016						; set VIC Multicolor mode off, 40 Columns
		jsr SoundOff					; returns with A=$00
		sta $d015						; VIC disable sprites
		rts
; -------------------------------------------------------------------------------------------------
; $85d8
Init1Player:
		ldx #$05
		lda #$80
-		sta $0420,x
		sta $0447,x
		dex
		bpl -
iplayer:jsr l8de3
		ldx #$05
		lda #$90
-		sta $042a,x
		dex
		bpl -
		rts
; $85f3 Init2Player
Init2Player:
		jsr l8dcb
		ldx #$05
		lda #$90
-		sta $0447,x
		dex
		bpl -
		bmi iplayer
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
; $8636 Load++ pointer 1 sub
LoadIncPointer1:
		lda (pointer1),y
IncPointer1:
		inc pointer1
		bne +
		inc pointer1+1
ucfntrt:
+		rts
; -------------------------------------------------------------------------------------------------
; $863f Store++ pointer 2 sub
StoreIncPointer2:
		sta (pointer2),y
		inc pointer2
		bne +
		inc pointer2+1
+		rts
; -------------------------------------------------------------------------------------------------
; $8648 Load byte from pointer 1 and shift high nibble to low sub
LoadHiNibblePointer1:
		lda (pointer1),y
		lsr
		lsr
		lsr
		lsr
		rts
; -------------------------------------------------------------------------------------------------
; $864f
l864f:	ldy state
		bne +
		jmp SetSpritePositions
+		cpy #$01
		bne l8698
		jsr InitMenu
		ldx #$00
		txa
		sta $02c5
		lda #$80
-		sta $0c00,x
		sta $0d00,x
		sta $0e00,x
		sta $0f00,x
		sta $0750,x
		dex
		bne -
		ldx #$0d
l8679:	lda Table1,x
		sta $0e67,x
		dex
		bpl l8679
		inx
l8683:	txa
		clc
		adc #$01
		sta $0c5f,x
		txa
		adc #$0d
		sta $0c87,x
		inx
		cpx #$0c
		bne l8683
l8695:	inc state
l8697:	rts
; -------------------------------------------------------------------------------------------------
; $8698
l8698:	cpy #$02
		bne l86a4
		lda jiffy_start
		cmp jiffy
		bne l8697
		beq l8695
l86a4:	ldx players
		inx
		txa
		ora #$90
		sta $0cd7
		lda players
		bne l86bc
		cpy #$03
		bne l86b8
		jsr Init1Player
l86b8:	lda #$92
		bne l86c5
l86bc:	cpy #$03
		bne l86c3
		jsr Init2Player
l86c3:	lda #$91
l86c5:	sta $0d4f
		ldx #$0a
l86ca:	lda $9e58,x
		sta $0d51,x
		sta $0cd9,x
		dex
		bpl l86ca
		jsr InitMenu
		ldx #$0b
l86db:	lda $9e37,x
		sta $0d27,x
		lda $9e43,x
		sta $0e40,x
		lda $9e2b,x
		sta $0ee0,x
		dex
		bpl l86db
		ldx difficulty
		lda $9e74,x
		sta $0dcc
		clc
		adc #$01
		sta $0dcd
		ldx #$10
l8700:	lda $9e63,x
		sta $0e65,x
		dex
		bpl l8700
		ldx #$08
l870b:	lda $9e4f,x
		sta $0f09,x
		dex
		bpl l870b
		rts
; -------------------------------------------------------------------------------------------------
; $8715 set sprite positions 0-4
SetSpritePositions:
		ldx #$04						; start with sprite 4
		ldy #$08						; x-position reg of sprite 4
spposlp:lda sprite_x,x						; load x
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
		lda sprite_y,x						; load y
		clc
		adc #$1b						; calc sprite y postion
		sta $d001,y						; set VIC sprite y
		dey
		dey
		dex
		bpl spposlp						; next sprite
		ldx #$04
		lda #$c4
l8744:	sta $07f8,x
		sec
		sbc #$01
		dex
		bpl l8744
		lda $45
		sta $c0
		lda #$53
		sta $c1
		jsr l8805
l8758:	lda ($c0),y
		sta $3100,x
		dex
		dex
		dex
		dey
		cpy #$02
		bne l8758
		lda sprite_y
		jsr l8801
l876a:	lda ($c0),y
		sta $3000,x
		dex
		dex
		dex
		dey
		cpy #$01
		bne l876a
		lda $42
		jsr l8801
l877c:	lda ($c0),y
		sta $3040,x
		dex
		dex
		dex
		dey
		cpy #$01
		bne l877c
		lda $43
		jsr l8801
l878e:	lda ($c0),y
		sta $3080,x
		dex
		dex
		dex
		dey
		cpy #$01
		bne l878e
		lda $44
		jsr l8801
l87a0:	lda ($c0),y
		sta $30c0,x
		dex
		dex
		dex
		dey
		cpy #$01
		bne l87a0
		lda jiffy
		and #$0f
		bne l87c6
		lda $dc00
		and #$10
		bne l87c0
		lda $57
		eor #$80
		sta $57
l87c0:	lda $0c
		beq l87c6
		dec $0c
l87c6:	lda $57
		beq l87cd
		jmp SoundOff
l87cd:	jsr l8dad
		lda $0a
		beq l87d8
		cmp #$02
		beq l87f6
l87d8:	lda $0e
		beq l87e6
		lda players
		beq l87e3
		jsr l8dcb
l87e3:	jmp l8de3
l87e6:	lda $14
		beq l87ed
		jmp l8c5c
l87ed:	lda $0f
		bne l8837
		lda $10
		bne l880a
l87f5:	rts
; -------------------------------------------------------------------------------------------------
; $87f6
l87f6:	inc $10
		lda #$00
		sta $03
		sta $0a
		jmp l8cd7
l8801:	sta $c0
		inc $c1
l8805:	ldy #$0c
		ldx #$22
		rts
; -------------------------------------------------------------------------------------------------
; $880a
l880a:	lda jiffy
		and #$03
		bne l87f5
		ldx $5f
		cpx #$40
		beq l8830
		lda $9c9c,x
		sta $d401						; SID voice 1 frequency hi
		lda $9cdc,x
		sta $d400						; SID voice 1 frequency lo
		lda #$21
		sta $d404						; SID voice 1 control
		inc $5f
		cpx #$28
		bne l87f5
		jmp PlayerDead
l8830:	inc $0f
l8832:	inc $8a
		jsr l8da2
l8837:	lda $12
		beq l885e
		cmp #$01
		bne l8851
		jsr l8a78
		lda $11
		bne l87f5
		lda $0e
		bne l87f5
		inc $12
		lda #$40
		sta $13
		rts
; -------------------------------------------------------------------------------------------------
; $8851
l8851:	lda $13
		beq l8858
		dec $13
		rts
l8858:	lda #$00
		sta $12
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
		lda $d01e						; load VIC sprite-sprite collision reg
		sta $bf
		and #$10
		beq l88c6
		ldx #$00						; start with sprite/ghost 0
		ldy #$01
l888c:	lda $8a,x
		asl
		bmi l88be
		tya
		bit $bf
		beq l88be
		lda $4a
		cmp $46,x
		bcs l88a4
		sec
		lda $46,x
		sbc $4a
		jmp l88a6
l88a4:	sbc $46,x
l88a6:	cmp #$04
		bcs l88be
		lda $45
		cmp sprite_y,x
		bcs l88b8
		sec
		lda sprite_y,x
		sbc $45
		jmp l88ba
l88b8:	sbc sprite_y,x
l88ba:	cmp #$05
		bcc l88c9
l88be:	tya
		asl
		tay
		inx
		cpx #$04						; check if last ghost
		bne l888c						; next ghost
l88c6:	jmp l897b
l88c9:	lda $8a,x
		bmi l88d0
		jmp l8968
l88d0:	lda #$42
		sta $8a,x
		stx $a7
		inc $a8
		sec
		lda $4a
		sbc #$04
		sta $02d4
		clc
		ldy #$02
l88e3:	adc #$02
		sta $02d5,y
		dey
		bpl l88e3
		adc #$02
		sta sprite_x,x
		lda #WHITE
		sta $02c7
		sta $d027,x						; set VIC sprite color = white (ghost)
		lda $45
		cmp sprite_y,x
		beq l8909
		bcc l8904
		lda #$fe
		bmi l8906
l8904:	lda #$02
l8906:	clc
		adc $45
l8909:	sta $26
		sta $28
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
		lda $9d1c,y
		sta pointer1
		iny
		lda $9d1c,y
		sta pointer1+1
		ldy #$0f
l892a:	lda (pointer1),y
		sta ($26),y
		lda $9c5f,y
		sta ($28),y
		dey
		bpl l892a
		lda #$00
		sta $d40b						; SID voice 2 control = off
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
		sta $52
		lda #$06
l8963:	sta $53
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
l897b:	lda $5b
		beq l89da
		lda $4a
		cmp #$7c
		bne l89da
		lda $45
		cmp #$84
		bne l89da
		ldx $19
		lda difficulty1,x
		cmp #$0c
		bcc l8995
		lda #$0c
l8995:	tax
		lda $9db9,x
		tax
		ldy #$00
l899c:	lda $9d91,x
		sta $0641,y
		inx
		iny
		cpy #$05
		bne l899c
		lda #$01
		sta $5d
		lda #$40
		sta $5e
		lda #$00
		sta $5b
		sta $59
		sta $5a
		lda #$01
		sta $b6
		lda #$10
		sta $b7
		ldx $19
		lda difficulty1,x
		cmp #$0c
		bcc l89ca
		lda #$0c
l89ca:	asl
		tax
		lda $4d94,x
		sta $52
		inx
		lda $4d94,x
		sta $53
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
		lda sprite_y,x
		cmp #$74
		bne l8a4f
		lda $46,x
		cmp #$a7
		bcs l8a53
		cmp #$52
		bcc l8a53
l8a4f:	lda $8a,x
		bpl l8a6a
l8a53:	jsr l95e0
		cmp #$00
		bne l8a74
		ldy $34,x
		cpy #$01
		bne l8a64
		sta $34,x
		beq l8a71
l8a64:	lda #$01
		sta $34,x
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
		lda $11
		beq l8ae8
		ldx $13
		beq l8a86
		dec $13
		rts
; -------------------------------------------------------------------------------------------------
; $8a86
l8a86:	cmp #$01
		bne l8ab0
		ldx #$00
l8a8c:	lda $4400,x
		sta $0450,x
		lda $4500,x
		sta $0550,x
		lda $4600,x
		sta $0650,x
		lda $4700,x
		sta $0750,x
		inx
		bne l8a8c
		jsr l8dcb
		lda #$00
		sta $19
		beq l8ae3
l8ab0:	cmp #$02
		bne l8ada
		ldx #$00
l8ab6:	lda $4800,x
		sta $0450,x
		lda $4900,x
		sta $0550,x
		lda $4a00,x
		sta $0650,x
		lda $4b00,x
		sta $0750,x
		inx
		bne l8ab6
		jsr l8de3
		lda #$01
		sta $19
		bne l8ae3
l8ada:	jsr l95aa
		lda #$00
		sta $11
		beq l8b2b
l8ae3:	lda #$03
l8ae5:	sta $11
		rts
; -------------------------------------------------------------------------------------------------
; $8ae8
l8ae8:	lda players
		beq l8b2b
		lda $19
		bne l8b0d
		lda lives2
		beq l8b2b
		lda lives1
		bne l8b02
		lda #$02
		sta $11
		lda #$30
		sta $13
		bne l8b3a
l8b02:	jsr BackupGameScreen1			; copies game screen to $4400
		lda #$30
		sta $13
		lda #$02
		bne l8ae5
l8b0d:	lda lives1
		beq l8b2b
		lda lives2
		bne l8b1f
		lda #$01
		sta $11
		lda #$30
		sta $13
		bne l8b3a
l8b1f:	jsr BackupGameScreen2			; copies game screen to $4800
		lda #$30
		sta $13
		lda #$01
		sta $11
l8b2a:	rts
; -------------------------------------------------------------------------------------------------
; $8b2b
l8b2b:	ldx $19
		lda lives1,x
		beq l8b3a
		jsr InitNewGame
		jsr l8cd7
		jmp PlayerDead
l8b3a:	lda #$2c
		ldx #$00
l8b3e:	sta $063d,x
		clc
		adc #$01
		inx
		cpx #$0e
		bne l8b3e
		lda #RED
		sta $02c7
		sta $d02b						; set VIC sprite 4 color = red (pacman)
		lda $11
		bne l8b2a
		ldx #$2a
		jsr l8b71
		ldx #$47
		jsr l8b71
		lda #$00
		sta $12
		lda #$01
		sta $0e
		lda #$e2
		sta $0c
		jsr l8de3
		jmp l95aa
l8b71:	ldy #$00
l8b73:	lda $0400,x
		cmp $0439,y
		beq l8b7f
		bcc l8b92
		bcs l8b86
l8b7f:	inx
		iny
		cpy #$06
		bne l8b73
		rts
; -------------------------------------------------------------------------------------------------
; $8b86
l8b86:	lda $0400,x
		sta $0439,y
		inx
		iny
		cpy #$06
		bne l8b86
l8b92:	rts
; -------------------------------------------------------------------------------------------------
; $8b93
l8b93:	ldx #$04
l8b95:	lda sprite_y,x
		cmp #$74
		bne l8b9e
		jsr l8ba2
l8b9e:	dex
		bpl l8b95
		rts
; -------------------------------------------------------------------------------------------------
; $8ba2
l8ba2:	sta $32
		txa
		cmp #$04
		bne l8bab
		lda #$ff
l8bab:	clc
		adc #$54
		sta $33
		lda #$ff
		sta $bd
		lda $46,x
		sta $be
		cmp #$c0
		bcc l8be2
		lda #$c0
l8bbe:	cmp $be
		beq l8c0c
		dec $be
		asl $bd
		bcs l8bbe
		lda $46,x
		cmp #$ca
		bcc l8c0c
		lda $4b,x
		cmp #$08
		bne l8c0c
		lda #$2a
		sta $46,x
		cpx #$04
		bne l8c0c
		dec $3d
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
		lda $46,x
		cmp #$2a
		bne l8c0c
		lda $4b,x
		cmp #$04
		bne l8c0c
		lda #$ca
		sta $46,x
		cpx #$04
		bne l8c0c
		inc $3d
		lda #$07
l8c0a:	sta $3c
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
		sta $d401						; set SID voice 1 frequency hi
		sty $d404						; SID voice 1 control
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
		jsr l8d8d
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
		sta $d022						; set VIC backgroundcolor 1 = lightblue					
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
l8ca7:	sta $d022						; set VIC backgroundcolor 1 = white
		lda #$10
		sta $16
		rts
; -------------------------------------------------------------------------------------------------
; $8caf
l8caf:	cmp #$03
		bne l8cb9
		jsr InitNewGame
		inc $15
		rts
; -------------------------------------------------------------------------------------------------
; $8cb9
l8cb9:	jsr l8e49
		ldx $19
		inc lives1,x
		inc difficulty1,x
		jsr l8cd7
		jsr PlayerDead
		lda #$00
		sta $14
		sta $15
		lda #$02
		sta $12
		lda #$40
		sta $13
		rts
; -------------------------------------------------------------------------------------------------
; $8cd7
l8cd7:	lda #$22
		ldx #$00
l8cdb:	sta $063f,x
		clc
		adc #$01
		inx
		cpx #$0a
		bne l8cdb
		ldx $19
		lda difficulty1,x
		cmp #$06
		bcc l8cf6
		cmp #$0a
		bcs l8cf6
		ldy #$0d
		bne l8cf8
l8cf6:	ldy #$0a
l8cf8:	sty $5c
		ldy #$00
		cmp #$06
		bcs l8d25
		sta temp
		lda #$e2
		sta pointer2
		lda #$07
		sta pointer2+1
		ldx #$00
l8d0c:	lda $9d72,x
		sta (pointer2),y
		inc pointer2
		clc
		adc #$01
		sta (pointer2),y
		cpx temp
		beq l8d51
		inx
		dec pointer2
		dec pointer2
		dec pointer2
		bne l8d0c
l8d25:	cmp #$12
		bcc l8d2b
		lda #$12
l8d2b:	sec
		sbc #$06
		sta temp
		sec
		lda #$8a
		sbc temp
		sta pointer1
		lda #$9d
		sbc #$00
		sta pointer1+1
		ldx #$00
l8d3f:	lda (pointer1),y
		sta $07d6,x
		clc
		adc #$01
		sta $07d7,x
		inx
		inx
		iny
		cpy #$07
		bne l8d3f
l8d51:	rts
; -------------------------------------------------------------------------------------------------
; $8d52
PlayerDead:
		jsr l8d8d
		ldx $19
		dec lives1,x
; $8d59
UpdateLivesDisplay:
		ldx $19
		lda lives1,x
		ldx #$00
		ldy #$1b
		cmp #$03
		bne l8d6f
		sty $07c8
l8d68:	sty $07c6
l8d6b:	sty $07c4
		rts
; -------------------------------------------------------------------------------------------------
; $8d6f
l8d6f:	cmp #$02
		bne l8d79
		jsr l8d89
		jmp l8d68
l8d79:	cmp #$01
		bne l8d83
		jsr l8d86
		jmp l8d6b
l8d83:	stx $07c4
l8d86:	stx $07c6
l8d89:	stx $07c8
		rts
; -------------------------------------------------------------------------------------------------
; $8d8d
l8d8d:	lda #$01
		sta $6a
		ldx #$03
l8d93:	lda $4b,x
		jsr l9952
		dex
		bpl l8d93
		lda #$00
		sta $6a
		jmp l9318
l8da2:	ldx #$0d
		lda #$00
l8da6:	sta $063d,x
		dex
		bpl l8da6
		rts
; -------------------------------------------------------------------------------------------------
; $8dad
l8dad:	lda jiffy
		and #$0f
		bne l8df2
		lda $b9
		bne l8dbb
		inc $b9
		bne l8dbf
l8dbb:	lda #$00
		sta $b9
l8dbf:	lda $19
		beq l8ddb
		lda $b9
		bne l8dcb
		tax
		tay
		beq l8dd1
l8dcb:	lda #$92
		ldx #$b5
		ldy #$b0
l8dd1:	sta $0421
		stx $0422
		sty $0423
		rts
l8ddb:	lda $b9
		bne l8de3
		tax
		tay
		beq l8de9
l8de3:	lda #$91
		ldx #$b5
		ldy #$b0
l8de9:	sta $0404
		stx $0405
		sty $0406
l8df2:	rts
; -------------------------------------------------------------------------------------------------
; $8df3 init new game
InitNewGame:
		jsr ClearGameRAM				; clear RAM at $3000, $5300
		ldx #$8a
-		sta $3b,x
		dex
		bne -							; clear ZP $3c - $c5
		jsr ColorInit					; init color RAM and VIC Sprite colors
		ldx $19
		lda difficulty1,x
		cmp #$06
		bcc +
		lda #$06
+		tay
		lda DifficultyIndex,y			; load from table as index
		tax
		lda $4dae,x						; copy data from RAM to ZP with index
		sta $75
		lda DifficultyTable,y
		tay
		ldx #$03
-		lda $4dae,y
		sta $71,x
		dex
		bpl -
		ldx #$13
-		lda $9bbf,x
		sta $3c,x
		dex
		bpl -
		ldy #$00
		jsr Init87_89
		lda $d01e						; VIC clear sprite-sprite collision
		lda $d01f						; VIC clear sprite-foreground collision
		rts
; -------------------------------------------------------------------------------------------------
; $8e38 Init game variables - lives, score...
InitGameVariables:
		lda #LIVES						; start with 3 lives
		sta lives1
		sta lives2
		lda difficulty
		sta difficulty1
		sta difficulty2
		ldx #$01
		jsr inzersc						; zero score
l8e49:	jsr InitGameScreen				; copy game screen to screen RAM
		jsr UpdateLivesDisplay
		ldx $19
		lda difficulty1,x
		tay
		bne +
		lda jiffy
		bpl +
-		jsr Init87_89
		jmp ++
+		iny
		bne -
++		ldx $19
inzersc:lda #$0f						; zero score
		sta $20,x
		lda #$00
		sta $2e,x
		sta $22,x
		sta $24,x
		rts
; -------------------------------------------------------------------------------------------------
; $8e72 copies data from table to $87-$89
Init87_89:
		cpy #$03
		bcc +
		ldy #$03
+		ldx #$02
-		lda $9c3e,y
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
l8e93:	lda $4b,x
		jsr l9952
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
		sta $d404						; SID voice 1 control = sawtooth, on
		lda $af
		sta $d401						; set SID voice 1 frequency hi
		sec
		sbc #$02
		sta $af
		inc $b0
		lda $b0
		cmp #$04
		bne l8f33
		beq l8f14
l8ef2:	lda #$21
		sta $d404						; SID voice 1 control = sawtooth, on
		lda $af
		sta $d401						; set SID voice 1 frequency hi
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
		sta $d404						; SID voice 1 control = sawtooth, on
		lda $ae
		sta $d401						; set SID voice 1 frequency hi
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
		inc $12
		rts
; -------------------------------------------------------------------------------------------------
; $8f3b
l8f3b:	dec $b0
		rts
; -------------------------------------------------------------------------------------------------
; $8f3e
l8f3e:	lda $a9
		beq l8f9a
		lda $45
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
		lda LookUpTable,x
		tay
		dey
		lda $9ba7,x
		sta (pointer2),y
		rts
; -------------------------------------------------------------------------------------------------
; $8f73
l8f73:	ldy #$0c
		ldx #$09
l8f77:	lda $9b88,x
		sta (pointer2),y
		dey
		dex
		bpl l8f77
		rts
; -------------------------------------------------------------------------------------------------
; $8f81
l8f81:	ldy #$0f
		ldx #$0f
l8f85:	lda $9c53,x
		sta (pointer2),y
		dey
		dex
		bpl l8f85
		rts
; -------------------------------------------------------------------------------------------------
; $8f8f
l8f8f:	ldy #$0f
		lda #$00
l8f93:	sta (pointer2),y
		dey
		bpl l8f93
		sta $a9
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
l8fb6:	ldx $19
		lda difficulty1,x
		tax
		lda $4c72,x
		cmp $bb
		bne l8ff9
		ldx #$03						; 3-0 ghosts
l8fc4:	lda $8a,x
		bpl l8fce
		lda SpriteColors,x
		sta $d027,x						; set VIC sprite color from table
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
; $8ff9
l8ff9:	lda $ba
		bne l9003
		inc $bb
		lda #$18
		sta $ba
l9003:	dec $ba
l9005:	lda $bb
		lsr
		bcc l900e
		ldy #$06
		bne l9010
l900e:	ldy #WHITE
l9010:	ldx #$03						; 3-0 ghosts
l9012:	lda $8a,x
		bpl l901a
		tya
		sta $d027,x						; set VIC sprites color 3-0 (ghosts)
l901a:	dex
		bpl l9012
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
		sta $d408						; SID voice 2 frequency hi
		lda #$21
l906d:	sta $d40b						; SID voice 2 control = sawtooth, on
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
		sta $d408						; SID voice 2 frequency hi
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
		sta $d401						; SID voice 1 frequency hi
		sta $d408						; SID voice 2 frequency hi
		lda #$21
		bne l90b7
l90a4:	lda #$02
		sta $a5
		sec
		lda $a6
		sbc #$03
		sta $a6
		sta $d401						; SID voice 1 frequency hi
		sta $d408						; SID voice 2 frequency hi
		lda #$21
l90b7:	sta $d40b						; SID voice 2 control = sawtooth, on
		bne l9109
l90bc:	jsr SoundOff
		sta $a4
		sta $63
		sty $a7
		lda #$0f
		sta $02c7
		ldx #$03						; 3-0 ghosts
l90cc:	lda $d027,x						; load VIC sprites color 3-0 (ghosts)
		cmp #$f1
		beq l90d7
		dex
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
		lda $9d24,x
		jmp l9102
l90ff:	lda $9d2b,x
l9102:	inc $b4
		sta $d401						; SID voice 1 frequency hi
		lda #$21
l9109:	sta $d404						; SID voice 1 control = sawtooth, on
l910c:	rts
; -------------------------------------------------------------------------------------------------
; $910d Stop SID sound
SoundOff:
		lda #$88
		sta $d418						; SID mode to 3OFF, Volume = 8
		lda #$00
		sta $d404						; SID voice 1 control = off
		sta $d40b						; SID voice 2 control = off
		ldy #$ff
		rts
; -------------------------------------------------------------------------------------------------
; $911d clear RAM areas $3000-$31ff and $5300-$57ff
ClearGameRAM:
		ldx #$00
		txa
clramlp:sta $3000,x
		sta $3100,x
		sta $5300,x
		sta $5400,x
		sta $5500,x
		sta $5600,x
		sta $5700,x
		inx
		bne clramlp
		txa
		rts
; -------------------------------------------------------------------------------------------------
; $913a Init color RAM + Sprite colors
ColorInit:
		ldx #$00
		lda #GRAY3
coinlp1:sta ColorRAM64,x				; init color RAM with gray3
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
; -------------------------------------------------------------------------------------------------
; $9163 init Screen
InitGameScreen:
		ldx #$00
scrinlp:lda MapData,x					; load decompressed Screen Data
		sta GameScreen+$50,x			; copy to game screen from line2
		lda MapData+$100,x
		sta GameScreen+$150,x
		lda MapData+$200,x
		sta GameScreen+$250,x
		lda MapData+$300,x
		sta GameScreen+$350,x
		inx
		bne scrinlp
		rts
; -------------------------------------------------------------------------------------------------
; $9181 backup game screen to $4400
BackupGameScreen1:
		ldx #$00
bscr1lp:lda GameScreen+$50,x
		sta ScreenBackup1,x
		lda GameScreen+$150,x
		sta ScreenBackup1+$100,x
		lda GameScreen+$250,x
		sta ScreenBackup1+$200,x
		lda GameScreen+$350,x
		sta ScreenBackup1+$300,x
		inx
		bne bscr1lp
		rts
; -------------------------------------------------------------------------------------------------
; $919f backup game screen to $4800
BackupGameScreen2:
		ldx #$00
bscr2lp:lda GameScreen+$50,x
		sta ScreenBackup2,x
		lda GameScreen+$150,x
		sta ScreenBackup2+$100,x
		lda GameScreen+$250,x
		sta ScreenBackup2+$200,x
		lda GameScreen+$350,x
		sta ScreenBackup2+$300,x
		inx
		bne bscr2lp
l91bc:	rts
; -------------------------------------------------------------------------------------------------
; $91bd
l91bd:	lda $66
		bmi l91bc
		ldx #$04
		jsr l94de
		clc
		lda $68
		bit temp
		beq l91e3
		cmp $4f
		beq l91df
		ora $4f
		tay
		and #$03
		beq l91df
		tya
		and #$0c
		beq l91df
		sty $64
l91df:	lda $68
		sta $4f
l91e3:	ldx $19
		lda $dc00
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
		sta $03
		sta $68
		bit temp
		beq l920a
		sta $4f
l920a:	lda $4f
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
		dec $45
		dec $45
		lda $62
		bne l923b
		lda #$03
		sta $62
		bne l9252
l923b:	dec $62
		bne l9252
		lda $45
		cmp $3f
		beq l9252
		sec
		lda $3c
		sbc #$28
		sta $3c
		lda $3d
		sbc #$00
		sta $3d
l9252:	ldy #$06
		jmp l931d
l9257:	lda $63
		bne l9280
		inc $45
		inc $45
		lda $62
		cmp #$03
		bne l927e
		lda #$00
		sta $62
		lda $45
		cmp $3f
		beq l9280
		clc
		lda $3c
		adc #$28
		sta $3c
		lda $3d
		adc #$00
		sta $3d
		bne l9280
l927e:	inc $62
l9280:	ldy #$08
		jmp l931d
l9285:	lda $63
		bne l92b5
		lda $4a
		cmp #$ca
		bne l9299
		lda #$2a
		sta $4a
		lda #$df
		sta $3c
		dec $3d
l9299:	inc $4a
l929b:	lda $3e
		cmp #$03
		bne l92b3
		lda #$00
		sta $3e
		lda $4a
		cmp $40
		beq l92b5
		inc $3c
		bne l92b5
		inc $3d
		bne l92b5
l92b3:	inc $3e
l92b5:	ldy #$02
		bne l931d
l92b9:	lda $63
		bne l92f0
		lda $4a
		cmp #$2a
		bne l92cd
		lda #$ca
		sta $4a
		lda #$07
		sta $3c
		inc $3d
l92cd:	dec $4a
		lda $3e
		bne l92d9
		lda #$03
		sta $3e
		bne l92f0
l92d9:	dec $3e
		bne l92f0
		lda $4a
		cmp $40
		beq l92f0
		sec
		lda $3c
		sbc #$01
		sta $3c
		lda $3d
		sbc #$00
		sta $3d
l92f0:	ldy #$04
		bne l931d
l92f4:	lda $4f
		cmp #$01
		bne l92fe
		ldy #$06
		bne l9314
l92fe:	cmp #$02
		bne l9306
		ldy #$08
		bne l9314
l9306:	cmp #$04
		bne l930e
		ldy #$04
		bne l9314
l930e:	cmp #$08
		bne l9318
		ldy #$02
l9314:	lda #$0a
		bne l9333
l9318:	ldy #$00
		tya
		beq l9333
l931d:	ldx $67
		bne l9325
		inc $67
		bne l9318
l9325:	dex
		lda $9ba5,x
		cpx #$02
		bne l9331
		ldx #$ff
		stx $67
l9331:	inc $67
l9333:	tax
		lda $9bb5,y
		sta pointer1
		iny
		lda $9bb5,y
		sta pointer1+1
		txa
		clc
		adc pointer1
		sta pointer1
		lda #$00
		adc pointer1+1
		sta pointer1+1
		ldy #$09
l934d:	lda (pointer1),y
		sta $5803,y
		dey
		bpl l934d
		lda #$00
		sta pointer1
		lda #$58
		sta pointer1+1
		lda $45
		sta pointer2
		lda #$53
		sta pointer2+1
		clc
		lda $4a
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
l9383:	rts
; -------------------------------------------------------------------------------------------------
; $9384
l9384:	lda $3e
		bne l93e8
		lda $62
		bne l93e8
		lda $4a
		cmp $40
		bne l9398
		lda $45
		cmp $3f
		beq l9383
l9398:	lda $45
		sta $3f
		lda $4a
		sta $40
		ldy #$00
		lda ($3c),y
		cmp #$01
		beq l93b0
		cmp #$02
		bne l93e8
		tya
		sta ($3c),y
		rts
; -------------------------------------------------------------------------------------------------
; $93b0
l93b0:	sta $54
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
		sta ($3c),y
l93d0:	ldx $19
		inc $22,x
		bne l93d8
		inc $24,x
l93d8:	ldx $19
		lda $24,x
		beq l93e8
		lda $22,x
		cmp #$04
		bne l93e8
		lda #$01
		sta $14
l93e8:	rts
; -------------------------------------------------------------------------------------------------
; $93e9
l93e9:	ldx $19
		lda $20,x
		sta temp
		ldx $45
		ldy $4a
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
		ldx $19
		sta $20,x
		lda #$05
		sta $54
		jsr l946d
		lda #$01
		sta $bb
		lda #$ff
		sta $a8
		ldx $19
		lda difficulty1,x
		tax
		lda $9c44,x
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
		lda $4b,x
		tay
		lda $4dcf,y
		sta $4b,x
l9467:	dex
		bpl l944b
		jmp l93d0
l946d:	lda #$00
		sta $56
		sed
		lda $19
		beq l947a
		ldx #$4c
		bne l947c
l947a:	ldx #$2f
l947c:	ldy #$05
l947e:	clc
		lda $0400,x
		and #$0f
		adc $56
		adc $0050,y
		pha
		and #$10
		beq l9490
		lda #$01
l9490:	sta $56
		pla
		ora #$10
		cmp #$10
		bne l94ab
		cpy #$00
		bne l94a0
		tya
		beq l94ab
l94a0:	lda $03ff,x
		bne l94a9
		lda $56
		beq l94ab
l94a9:	lda #$10
l94ab:	ora #$90
		sta $0400,x
		dex
		dey
		bpl l947e
		cld
		ldx #$05
		lda #$00
l94b9:	sta $50,x
		dex
		bpl l94b9
		ldx $19
		lda $1c,x
		bne l94cf
		cpx #$00
		bne l94d0
		lda $042b
		cmp #$90
		bne l94d7
l94cf:	rts
; -------------------------------------------------------------------------------------------------
; $94d0
l94d0:	lda $0448
		cmp #$90
		beq l94cf
l94d7:	inc $1c,x
		inc lives1,x
		jmp UpdateLivesDisplay
l94de:	lda $46,x
		sta $61
		lda sprite_y,x
		stx temp
		ldx #$09
l94e8:	cmp $9bd3,x
		beq l94fc
		dex
		bpl l94e8
		lda $61
		ldy #$09
l94f4:	cmp $9bdd,y
		beq l950d
		dey
		bpl l94f4
l94fc:	ldy #$09
		lda $61
l9500:	cmp $9bdd,y
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
		lda $9be7,x
		sta pointer1
		inx
		lda $9be7,x
		sta pointer1+1
		lda (pointer1),y
		sec
l9523:	ldx temp
		sta temp
		php
		cpx #$04
		beq l9554
		lda $4b,x
		tay
		lda temp
		and $4dc6,y
		sta temp
		lda sprite_y,x
		cmp #$64
		bne l9554
		lda $46,x
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
l957c:	lda $58
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
		sbc $58
l9594:	sta $9a
		sta $d408						; SID voice 2 frequency hi
		lda #$11
		sta $d40b						; SID voice 2 control = triangle, on
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
		ldx $19
		lda $20,x
		sta temp
		lda #$01
		bit temp
		beq l95bb
		sty $04cb
l95bb:	asl
		bit temp
		beq l95c3
		sty $04ec
l95c3:	asl
		bit temp
		beq l95cb
		sty $06d3
l95cb:	asl
		bit temp
		bne l95dc
		rts
; -------------------------------------------------------------------------------------------------
; $95d1
l95d1:	ldy #$00
		sty $04cb
		sty $04ec
		sty $06d3
l95dc:	sty $06f4
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
		ldy $19
		lda $001e,y
		cmp #$06
		bcc l95fe
		lda #$06
l95fe:	tay
		cpx #$04
		bne l9608
		lda DifficultyIndex,y
		bpl +							; skip always
l9608:	lda DifficultyTable,y
+		clc
		adc $6c,x
		tay
		lda $4dae,y
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
l9623:	ldx $19
		lda $24,x
		beq l9632
l9629:	ldx #$03
		jsr l9666
		lda #$05
		bne l964f
l9632:	lda $22,x
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
l964f:	sta $58
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
l967e:	lda $5d
		beq l9696
		lda $5e
		bne l9693
		lda #$00
		sta $5d
		ldx #$04
l968c:	sta $0641,x
		dex
		bpl l968c
		rts
; -------------------------------------------------------------------------------------------------
; $9693
l9693:	dec $5e
l9695:	rts
; -------------------------------------------------------------------------------------------------
; $9696
l9696:	lda $5b
		bne l96d4
		ldx $19
		lda $22,x
		tay
		lda $2e,x
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
l96b0:	inc $2e,x
		lda difficulty1,x
		cmp #$0c
		bcc l96ba
		lda #$0c
l96ba:	tax
		lda $9d72,x
		sta $0643
		clc
		adc #$01
		sta $0644
		lda #$01
		sta $5b
		lda #$c0
		sta $59
		lda #$02
		sta $5a
		rts
; -------------------------------------------------------------------------------------------------
; $96d4
l96d4:	lda $5a
		bne l96e7
		lda $59
		bne l96e7
		lda #$00
		sta $0643
		sta $0644
		sta $5b
		rts
; -------------------------------------------------------------------------------------------------
; $96e7
l96e7:	dec $59
		bne l96ed
		dec $5a
l96ed:	rts
; -------------------------------------------------------------------------------------------------
; $96ee
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
		lda $46,x
		tay
		lda sprite_y,x
		cpy #$7c
		bne l9728
		cmp #$64
		bne l9728
		lda $4b,x
		cmp #$01
		bne l971e
		lda #$04
		sta $4b,x
		lda sprite_y,x
		bne l9728
l971e:	lda #$44
		sta $8a,x
		lda #$ff
		sta $8e,x
		bne l973e
l9728:	jsr l94de
		bcc l9739
		clc
		lda #$7c
		sta $7e,x
		lda #$64
		sta $82,x
		jsr l99e9
l9739:	lda $4b,x
		jsr l9952
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
l9788:	lda sprite_y,x
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
l97a4:	sty $4b,x
		sta $8a,x
		rts
; -------------------------------------------------------------------------------------------------
; $97a9
l97a9:	lda #$01
l97ab:	sta $4b,x
		jmp l9952
l97b0:	lda $8a,x
		and #$0f
		sta $8a,x
		lda SpriteColors,x
		sta $d027,x						; set VIC sprite color from table
		rts
; -------------------------------------------------------------------------------------------------
; $97bd
l97bd:	lda $46,x
		cmp #$7c
		beq l9788
l97c3:	lda #$08
		bne l97ab
l97c7:	lda $46,x
		cmp #$7c
		beq l9788
l97cd:	lda #$04
		bne l97ab
l97d1:	lda sprite_y,x
		cmp #$74
		bne l97e1
		cpx #$02
		beq l97e5
		cpx #$03
		beq l97ed
		bne l97b0
l97e1:	lda #$02
		bne l97ab
l97e5:	lda $46,x
		cmp #$70
		bne l97cd
		beq l97b0
l97ed:	lda $46,x
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
l9826:	lda $4b,x
		cmp #$01
		bne l9836
		lda sprite_y,x
		cmp #$70
		bne l9840
		lda #$02
		bne l983e
l9836:	lda sprite_y,x
		cmp #$78
		bne l9840
		lda #$01
l983e:	sta $4b,x
l9840:	lda $4b,x
		jmp l9952
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
l9877:	jsr l94de
		bcc l987f
		clc
		inc $8e,x
l987f:	txa
		asl
		tay
		lda $9d32,y
		sta pointer1
		iny
		lda $9d32,y
		sta pointer1+1
		lda $8e,x
		tay
		lda (pointer1),y
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
		lda $d41b						; load random value SID register $1b
		and #$0f						; calc random value from $0-$f
		sta $76,x
		asl
		tay
		lda $9d3a,y
		sta $7e,x
		iny
		lda $9d3a,y
		sta $82,x
		lda #$96
		sta $86,x
		bne l98c8
l98c3:	sta $4b,x
		jmp l9952
l98c8:	lda $86,x
		bne l98cf
		jmp l9915
l98cf:	jsr l94de
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
		jsr l94de
		bcc l98f6
		clc
		inc $7a,x
l98f6:	lda $76,x
		tay
		lda $9d62,y
		adc $7a,x
		tay
		lda $4cae,y
		bne l9910
		sta $7a,x
		lda $76,x
		tay
		lda $9d62,y
		tay
		lda $4cae,y
l9910:	sta $4b,x
		jmp l9950
l9915:	lda #$20
		sta $8a,x
		txa
		asl
		tay
		lda $9d5a,y
		sta $7e,x
		iny
		lda $9d5a,y
		sta $82,x
		jsr l94de
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
l993f:	lda $4a
		sta $7e,x
		lda $45
		sta $82,x
		jsr l94de
		bcc l9950
		clc
		jsr l99e9
l9950:	lda $4b,x
l9952:	cmp #$01
		bne l9962
		ldy $6a
		bne l995e
		dec sprite_y,x
		dec sprite_y,x
l995e:	lda #$00
		beq l998f
l9962:	cmp #$02
		bne l9972
		ldy $6a
		bne l996e
		inc sprite_y,x
		inc sprite_y,x
l996e:	lda #$0a
		bne l998f
l9972:	cmp #$04
		bne l9980
		ldy $6a
		bne l997c
		dec $46,x
l997c:	lda #$14
		bne l998f
l9980:	cmp #$08
		bne l998e
		ldy $6a
		bne l998a
		inc $46,x
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
l99ac:	sty $581c
		clc
		adc #$10
		sta pointer1
		lda #$00
		adc #$9b
		sta pointer1+1
		ldy #$09
l99bc:	lda (pointer1),y
		sta $5812,y
		dey
		bpl l99bc
		lda #$10
		sta pointer1
		lda #$58
		sta pointer1+1
		lda $46,x
		sta sprite_x,x
		lda sprite_y,x
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
		rts
; -------------------------------------------------------------------------------------------------
; $99e9
l99e9:	lda sprite_y,x
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
		lda $46,x
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
		lda $d41b						; load random value SID register $1b
		bmi l9a3a
l9a36:	lda $92,x
		bne l9a64
l9a3a:	lda $96,x
		bne l9a64
l9a3e:	lda $96,x
		bit temp
		bne l9a3a
		lda $d41b						; load random value SID register $1b
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
l9a64:	sta $4b,x
		lda $4b,x
		rts
; -------------------------------------------------------------------------------------------------
; $9a69
l9a69:	lda sprite_y,x
		cmp $45
		beq l9a76
		lda $46,x
		cmp $4a
		beq l9abb
		rts
; -------------------------------------------------------------------------------------------------
; $9a76
l9a76:	ldy #$09
l9a78:	lda $9bd3,y
		cmp $45
		beq l9a83
		dey
		bpl l9a78
l9a82:	rts
; -------------------------------------------------------------------------------------------------
; $9a83
l9a83:	lda $46,x
		cmp $4a
		bcs l9aa2
		lda $4b,x
		cmp #$08
		bne l9a82
l9a8f:	lda $9c08,y
		cmp #$ff
		beq l9aff
		cmp $46,x
		bcs l9a9d
		iny
		bne l9a8f
l9a9d:	cmp $4a
		bcs l9aff
		rts
; -------------------------------------------------------------------------------------------------
; $9aa2
l9aa2:	lda $4b,x
		cmp #$04
		bne l9a82
l9aa8:	lda $9c08,y
		cmp #$ff
		beq l9aff
		cmp $4a
		bcs l9ab6
		iny
		bne l9aa8
l9ab6:	cmp $46,x
		bcs l9aff
		rts
; -------------------------------------------------------------------------------------------------
; $9abb
l9abb:	ldy #$09
l9abd:	lda $9bdd,y
		cmp $4a
		beq l9ac8
		dey
		bpl l9abd
l9ac7:	rts
; -------------------------------------------------------------------------------------------------
; $9ac8
l9ac8:	lda sprite_y,x
		cmp $45
		bcc l9ae7
		lda $4b,x
		cmp #$01
		bne l9ac7
l9ad4:	lda $9c28,y
		cmp #$ff
		beq l9aff
		cmp $45
		bcs l9ae2
		iny
		bne l9ad4
l9ae2:	cmp sprite_y,x
		bcs l9aff
		rts
; -------------------------------------------------------------------------------------------------
; $9ae7
l9ae7:	lda $4b,x
		cmp #$02
		bne l9ac7
l9aed:	lda $9c28,y
		cmp #$ff
		beq l9aff
		cmp sprite_y,x
		bcs l9afb
		iny
		bne l9aed
l9afb:	cmp $45
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
*= $9b10
; Sprite data
		!byte $38, $7c, $d6, $d6, $d6, $fe, $fe, $fe
		!byte $fe, $fe, $38, $7c, $fe, $fe, $fe, $fe
		!byte $d6, $d6, $d6, $fe, $38, $7c, $fe, $fe
		!byte $ae, $ae, $ae, $fe, $fe, $fe, $38, $7c
		!byte $fe, $fe, $ea, $ea, $ea, $fe, $fe, $fe
		!byte $38, $7c, $fe, $d6, $d6, $d6, $fe, $d6
		!byte $aa, $fe, $00, $00, $00, $28, $28, $28
		!byte $00, $00, $00, $00, $38, $7c, $fe, $fe
		!byte $fe, $fe, $fe, $fe, $7c, $38, $38, $7c
		!byte $fe, $f8, $e0, $e0, $f8, $fe, $7c, $38
		!byte $38, $7c, $f8, $f0, $e0, $e0, $f0, $f8
		!byte $7c, $38, $38, $7c, $fe, $3e, $0e, $0e
		!byte $3e, $fe, $7c, $38, $38, $7c, $3e, $1e
		!byte $0e, $0e, $1e, $3e, $7c, $38, $00, $44
		!byte $c6, $c6, $ee, $ee, $fe, $fe, $7c, $38
		!byte $00, $00, $82, $c6, $ee, $ee, $fe, $fe
		!byte $7c, $38, $38, $7c, $fe, $fe, $ee, $ee
		!byte $c6, $c6, $44, $00, $38, $7c, $fe, $fe
		!byte $ee, $ee, $c6, $82, $00, $00, $0a, $00
		!byte $82, $00, $c6, $82, $00, $7c, $38, $10
		!byte $7c, $38, $38, $10, $10, $4c, $9b, $56
		!byte $9b, $6a, $9b, $7e, $9b, $92, $9b, $e3
		!byte $06, $02, $a6, $7a, $64, $74, $74, $74
		!byte $a4, $7c, $7c, $70, $88, $7c, $04, $02
		!byte $01, $01, $04, $2c, $44, $54, $64, $74
		!byte $84, $94, $a4, $b4, $c4, $3a, $46, $52
		!byte $62, $76, $82, $96, $a6, $b2, $be, $0e
		!byte $4c, $18, $4c, $22, $4c, $2c, $4c, $36
		!byte $4c, $40, $4c, $4a, $4c, $54, $4c, $5e
		!byte $4c, $68, $4c, $7c, $ff, $58, $7c, $9e
		!byte $ff, $58, $9e, $ff, $3c, $ac, $ff, $ff
		!byte $00, $0c, $02, $06, $00, $06, $00, $09
		!byte $02, $0c, $64, $84, $ff, $38, $4c, $64
		!byte $84, $9c, $bc, $ff, $3c, $ff, $38, $5c
		!byte $9c, $bc, $ff, $4c, $74, $8c, $ac, $ff
		!byte $00, $03, $0a, $0c, $11, $11, $0c, $0a
		!byte $03, $00
; -------------------------------------------------------------------------------------------------
; SpriteColors Sprite color table
SpriteColors:
		!byte RED, LIGHTRED, GREEN, ORANGE
		!byte YELLOW, BROWN, ORANGE, GRAY3
; -------------------------------------------------------------------------------------------------
; 		
		!byte $0f, $0a, $0f, $0d, $90, $60
		!byte $30, $04, $00, $00, $ff, $c0, $80, $40
		!byte $00, $c0, $00, $00, $00, $c0, $00, $00
		!byte $00, $40, $00, $00, $00, $00, $00, $00
		!byte $92, $54, $00, $c6, $00, $54, $92, $00
		!byte $00, $00, $00, $c6, $29, $29, $29, $29
		!byte $29, $c6, $00, $00, $00, $00, $00, $38
		!byte $45, $05, $19, $21, $41, $7c, $00, $00
		!byte $00, $00, $00, $08, $19, $29, $49, $7d
		!byte $09, $08, $00, $00, $00, $00, $00, $38
		!byte $45, $45, $39, $45, $45, $38, $00, $00
		!byte $00, $00, $00, $8c, $91, $a1, $b9, $a5
		!byte $a5, $98, $00, $00, $0c, $08, $08, $21
		!byte $00, $19, $00, $15, $0c, $21, $19, $08
		!byte $00, $15, $15, $00, $0d, $08, $08, $23
		!byte $00, $1a, $00, $16, $0d, $23, $1a, $08
		!byte $00, $16, $16, $00, $0c, $08, $08, $21
		!byte $00, $19, $00, $15, $0c, $21, $19, $08
		!byte $00, $15, $15, $00, $0c, $15, $16, $17
		!byte $00, $17, $19, $1a, $00, $1a, $1c, $1d
		!byte $00, $21, $21, $00, $8f, $61, $61, $87
		!byte $00, $1e, $00, $1f, $8f, $87, $1e, $61
		!byte $00, $1f, $1f, $00, $4e, $e1, $e1, $86
		!byte $00, $9c, $00, $60, $4e, $86, $9c, $e1
		!byte $00, $60, $60, $00, $8f, $61, $61, $87
		!byte $00, $1e, $00, $1f, $8f, $87, $1e, $61
		!byte $00, $1f, $1f, $00, $8f, $1f, $60, $b5
		!byte $00, $b5, $1e, $9c, $00, $9c, $31, $df
		!byte $00, $87, $87, $00, $6b, $9c, $77, $9c
		!byte $83, $9c, $8f, $9c, $19, $1a, $1c, $1d
		!byte $20, $23, $00, $23, $1d, $1a, $17, $15
		!byte $12, $00, $86, $4c, $93, $4c, $9b, $4c
		!byte $a3, $4c, $96, $a4, $62, $74, $82, $64
		!byte $62, $64, $62, $94, $52, $74, $96, $94
		!byte $a6, $74, $96, $54, $52, $b4, $be, $c4
		!byte $82, $44, $52, $a4, $b2, $b4, $82, $44
		!byte $52, $44, $be, $2c, $3a, $2c, $be, $c4
		!byte $3a, $c4, $00, $0b, $16, $21, $2c, $33
		!byte $3a, $41, $48, $53, $60, $6d, $80, $99
		!byte $b6, $c9, $3a, $3c, $3e, $3e, $40, $40
		!byte $42, $42, $46, $46, $4a, $4a, $4c, $4c
		!byte $4c, $4c, $4c, $4c, $4c, $4a, $4a, $48
		!byte $48, $44, $44, $40, $40, $3e, $3e, $3c
		!byte $3a, $00, $4e, $4f, $5f, $60, $00, $50
		!byte $51, $5f, $60, $00, $52, $53, $5f, $60
		!byte $00, $54, $55, $5f, $60, $56, $57, $5e
		!byte $5f, $60, $58, $59, $5e, $5f, $60, $5a
		!byte $5b, $5e, $5f, $60, $5c, $5d, $5e, $5f
		!byte $60, $00, $05, $0a, $0a, $0f, $0f, $14
		!byte $14, $19, $19, $1e, $1e, $23
; -------------------------------------------------------------------------------------------------
; $9dc6 User font tiles 0 - $3e
UserFontTiles:
		!byte $00, $01, $02, $03, $04, $05, $08, $0a
		!byte $0d, $0f, $10, $11, $14, $15, $20, $22
		!byte $28, $2a, $2b, $2e, $30, $33, $3a, $3c
		!byte $3e, $40, $41, $50, $51, $54, $55, $80
		!byte $82, $a0, $a2, $a8, $b0, $c0, $c3, $cc
		!byte $cf, $e0, $e8, $f0, $f3, $fc, $ff, $3f
		!byte $0c, $fe, $fb, $c1, $f8, $1f, $0e, $8f
		!byte $df, $81, $70, $9f, $87, $07, $bc
; -------------------------------------------------------------------------------------------------
; $9e05
DifficultyIndex:
		!byte $08, $08, $08, $0c, $10, $14, $14
; $9e0c
DifficultyTable:
		!byte $00, $04, $08, $08, $0c, $10, $10, $a8
		!byte $a9, $a7, $a8, $80, $b3, $a3, $af, $b2
		!byte $a5
; $9e1d
Table1:
		!byte $88, $a3, $89, $80, $a1, $b4, $a1, $b2
		!byte $a9, $80, $91, $99, $98, $93
; $9e2b
Table2:		
		!byte $b0, $b2, $a5, $b3, $b3, $80, $a6, $91, $80, $b4, $af, $80
; $9e37
Table3:	
		!byte $b0, $b2, $a5, $b3, $b3, $80, $a6, $93, $80, $a6, $af, $b2
; $9e43
Table4:
		!byte $b0, $b2, $a5, $b3, $b3, $80, $a6, $95, $80, $b4, $af, $80  
; $9e4f
Table5:
		!byte $b0, $ac, $a1, $b9, $80, $a7, $a1, $ad, $a5
; $9e58
Table6:
		!byte $b0, $ac, $a1, $b9, $a5, $b2, $80, $a7
		!byte $a1, $ad, $a5
; $9e63
Table7:
		!byte $a3, $a8, $a1, $ae, $a7, $a5, $80, $a4
		!byte $a9, $a6, $a6, $a9, $a3, $b5, $ac, $b4
		!byte $b9
; $9e74
Table8:
		!byte $3a, $3c, $3e, $3e, $40, $40, $44, $44
		!byte $48, $48, $4a, $4a, $4c, $4c
; -------------------------------------------------------------------------------------------------
; $9e82 compressed menu user font (bytes 0-$3f from FontData, bit 6+7 = count)
cUserFontMenu:
		!byte $40, $31, $ae, $32, $2e, $80, $1f, $25
		!byte $73, $26, $40, $19, $69, $6b, $34, $40
		!byte $03, $09, $75, $6f, $40, $29, $34, $2d
		!byte $34, $2b, $29, $c0, $80, $18, $40, $06
		!byte $30, $36, $89, $80, $01, $03, $3d, $37
		!byte $38, $40, $df, $1f, $39, $40, $0e, $7a
		!byte $74, $2d, $40, $06, $30, $36, $89, $40
		!byte $f5, $3b, $38, $6e, $31, $f4, $00, $26
		!byte $3c, $3d, $49, $75, $00, $34, $3e, $2d
		!byte $71, $6e, $00, $af, $75, $09, $03, $00
		!byte $25, $29, $2b, $34, $2d, $34, $29, $00
		!byte $58, $c0, $40, $c9, $89, $00, $ee, $ae
		!byte $00, $ff
; -------------------------------------------------------------------------------------------------
; $9ee4 
UserCharMenu:
		!byte $81, $83, $83, $87, $87, $8f, $8f, $00
		!byte $fc, $de, $fe, $ff, $ff, $ff, $ff, $00
		!byte $0f, $0f, $0f, $0f, $0f, $8f, $8f, $00
		!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $00
; -------------------------------------------------------------------------------------------------
; $9f04 sprite 0-4 MSB bit set table
SpriteSetMSBMask:
		!byte $01, $02, $04, $08, $10
; $9f09 sprite 0-4 MSB bit clear table
SpriteClearMSBMask:
		!byte $fe, $fd, $fb, $f7, $ef
; -------------------------------------------------------------------------------------------------
; $9f0e LookUp Table
cLookUpTable:
		!byte $67, $78, $88, $99, $9a, $ab, $ab, $ac
		!byte $ec, $6a, $ce, $c6, $bc, $fe, $dd, $ef
		!byte $c7, $9c, $79, $6a, $5b, $c5, $00, $3a
		!byte $dd, $63, $00, $cc, $f7, $00, $bf, $cc
		!byte $00, $3b, $cc, $73, $00, $ac, $fd, $6a
		!byte $df, $c6, $96, $be, $dd, $e7, $a5, $ad
		!byte $59, $6a, $59, $d6, $9c, $cc, $dd, $cc
		!byte $c5, $bb, $bb, $bb, $bb, $7b, $b7, $77
		!byte $b7, $17, $11, $44, $24, $11, $18, $88
		!byte $88, $f4, $42, $41, $11, $f8, $82, $22
		!byte $82, $f8, $82, $24, $44, $24, $2f, $24
		!byte $24, $14, $18, $88, $01, $88, $82, $24
		!byte $44, $10, $44, $22, $88, $81, $14, $08
		!byte $14, $14, $22, $28, $10, $82, $44, $18
		!byte $08, $22, $41, $10, $42, $88, $14, $04
		!byte $22, $81, $10, $18, $22, $24, $14, $18
		!byte $01, $82, $82, $44, $44, $18, $80, $14
		!byte $41, $42, $42, $88, $88, $01, $88, $24
		!byte $24, $24, $42, $41, $11, $88, $80, $14
		!byte $42, $82, $42, $88, $88, $14, $18, $14
		!byte $11, $42, $22, $01, $81, $44, $22, $88
		!byte $24, $44, $41, $81, $41, $81, $18, $22
		!byte $22, $80, $82, $42, $82, $24, $44, $11
		!byte $81, $41, $88, $02, $44, $18, $81, $88
		!byte $28, $18, $88, $82, $44, $42, $42, $41
		!byte $41, $40, $01, $03, $05, $05, $07, $07
		!byte $10, $10, $20, $20, $30, $30, $50, $33
		!byte $33, $32, $32, $22, $23, $22, $22, $22
		!byte $21, $12, $12, $0d, $e0, $70, $00, $b0
		!byte $21, $08, $00, $04, $57, $41, $42, $58
		!byte $9b, $4c
; ***************************************** ZONE P500 *********************************************
!zone p500
*= $a000
; P500 I/O pointer init
!ifdef 	P500{
InitP500:
		lda #$00
		sta ColorRAM
		sta VIC
		sta SID
		sta CIA
		sta TPI1
		sta TPI2
		sta CharROM0
		sta CharROM1
		lda #>ColorRAMbase
		sta ColorRAM+1
		lda #>VICbase
		sta VIC+1
		lda #>SIDbase
		sta SID+1
		lda #>CIAbase
		sta CIA+1
		lda #>TPI1base
		sta TPI1+1
		lda #>TPI2base
		sta TPI2+1
		ldx #>CharROMbase
		stx CharROM0+1
		inx
		stx CharROM1+1

		lda #<Irqp500
		sta $fffe
		lda #>Irqp500
		sta $ffff						; set IRQ vector to $8583

		rts
; -------------------------------------------------------------------------------------------------
; p500 irq sub
Irqp500:
		pha
		txa
		pha
		tya
		pha
		lda #SYSTEMBANK
		sta IndirectBank				; select bank 15
		ldy #$19
		lda (VIC),y						; load VIC interrupt reg and mask bit 1
		and #$01
		beq inoraster						; skip if source is not raster interrupt
		inc jiffy						; increase jiffy
		ldy #$12
		lda #$32
		sta (VIC),y						; set VIC raster reg again to $32 (start)
		ldy #$19
		lda #$81
		sta (VIC),y						; clear VIC raster interrupt
		dec $30							;
		lda #GAMEBANK
		sta IndirectBank				; select bank 15
		jsr l864f				; draw screen
;		lda $a4
;		bne iskpspr						; skip if $a4 is not 0
;		jsr l8b93				; sprite direction compare loop
;iskpspr:ldx #$ff
;		stx $dc02						; set CIA1 port A for output
;		dex
;		stx $dc00				; ignore all columns
;idebkey:lda $dc01						; load CIA1 port B
;		cmp $dc01
;		bne idebkey						; debounce key
;		sta pressed_key					; store pressed key
;		ldx #$00
;		stx $dc02						; reset CIA1 port B to input
inoraster:
		pla
		tay
		pla
		tax
		pla
		rti
; -------------------------------------------------------------------------------------------------
Test:	lda #SYSTEMBANK
		sta IndirectBank				; select bank 15
		lda #BLACK						; color
		ldy #$20						
		sta (VIC),y						; set VIC exterior color
		ldy #$21
		sta (VIC),y						; set VIC background color
		ldy #$06
		lda (TPI1),y					; load TRI1 control register
		and #$0f						; clear CA, CB control bits#4-7 vic bank 0/15 select 
		ora #$a0						; set bit#5,4=10 CA=low -> Video matrix in bank 0
		sta (TPI1),y					; set bit#7,6=10 CB=high -> Characterset in bank 0 
		ldy #$02
		lda (TPI2),y					; load TRI2 port c
		and #$3f						; clear bit#6,7 vic 16k select bank $0000-$3fff
		sta (TPI2),y					; store to TRI2 port c
		lda #$3a
		ldy #$18						; VIC reg $18 memory pointers
		sta (VIC),y						; set VM13-10=$3 screen at $0a00, CB13,12,11,x=1010 char at $2800

		lda #$7f						; bit#7=0 clears/mask out all 5 irq sources with bit#0-4 = 1
		ldy #$0d						; CIA interrupt control register
		sta (CIA),y						; disable all hardware interrupts
		lda #$00
		ldy #$05
		sta (TPI1),y					; set TPI1 reg $5 interrupt mask reg = $00 - disable all irq

		rts
}