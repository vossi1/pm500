; c64decodc.b
; C64 decompress, decode and data copy routines 
; $83b3 Copy and uncompress maze
		lda #<MazeData
		sta pixel_put_ptr
		lda #>MazeData
		sta pixel_put_ptr+1				; set target pointer = MazeData
		lda #<CompressedMazeData
		sta pixel_get_ptr
		lda #>CompressedMazeData
		sta pixel_get_ptr+1				; set source pointer = compressed MazeData
		ldy #$00
mazcopy:lda (pixel_get_ptr),y			; load byte from maze
		sta temp						; store for later bit#6 check
		bmi mazbit7						; skip if bit#7 = 1
		bpl mazchar						; branch if normal char
mazbit7:cmp #$ff						; check if $ff = end of data
		beq lookupt:					; exit loop
		bit temp
		bvs mazbit6						; branch if bit#6 = 1
		and #$7f						; clear ident bit#7
		tax								; use value in X as repeat counter
		jsr IncGetPtr					; read next byte
		jsr LoadIncGetPtr
mazrptb:jsr StoreIncPutPtr				; copy byte to maze
		dex
		bpl mazrptb						; repeat store byte X times
		bmi mazcopy						; read next byte
mazbit6:and #$3f						; clear ident bits#7,6
		jsr StoreIncPutPtr				; store byte to maze
		jsr IncGetPtr					; next byte
		jsr LoadHiNibbleGetPtr			; load and shift high nibble 4 bits right
		jsr StoreIncPutPtr				; store hi nibble
		lda (pixel_get_ptr),y
		and #$0f						; isolate low nibble
mazchar:jsr StoreIncPutPtr				; copy byte to maze
		jsr IncGetPtr
		jmp mazcopy						; read next byte
; $8401 Copy and decode Nibbles
lookupt:lda #<CompressedNibbles
		sta pixel_get_ptr
		lda #>CompressedNibbles
		sta pixel_get_ptr+1				; set source pointer = $9f0e
		lda #<NibbleTable
		sta pixel_put_ptr
		lda #>NibbleTable
		sta pixel_put_ptr+1				; set target pointer = NibbleTable
		ldy #$00
		ldx #$00
lutcopy:jsr LoadHiNibbleGetPtr			; load and shift high nibble 4 bits right
		jsr StoreIncPutPtr				; store high nibble
		jsr LoadIncGetPtr
		and #$0f
		jsr StoreIncPutPtr				; store low nibble
		inx
		bne lutcopy						; next byte

; $8426 C64
		lda $dc0e						; stop CIA1 timer A to prevent problems when switching CharROM 
		and #$fe
		sta $dc0e
		lda CPUPort64					; enable character ROM
		and #$fb
		sta CPUPort64
		ldx #$00
fontcpy:lda CharROM64+$100,x			; load from character ROM
		sta GameChar+$400,x				; store to game fontset from char $80
		sta OptionsChar+$400,x			; store to menu fontset from char $80
		lda CharROM64,x
		sta GameChar+$500,x
		sta OptionsChar+$500,x
		dex
		bne fontcpy
		lda CPUPort64					; disable character ROM
		ora #$04
		sta CPUPort64
		lda $dc0e						; start CIA1 timer A
		ora #$01
		sta $dc0e

; $8459 Copy and decode user fonts
		lda #<EncodedGameChar
		sta pixel_get_ptr
		lda #>EncodedGameChar
		sta pixel_get_ptr+1				; pixel_get_ptr = $8011
		lda #<GameChar
		sta pixel_put_ptr
		lda #>GameChar
		sta pixel_put_ptr+1				; pixel_put_ptr = $2000
		jsr UncompressChar				; copy game user font
		lda #<EncodedOptionsChar
		sta pixel_get_ptr
		lda #>EncodedOptionsChar
		sta pixel_get_ptr+1				; pointer1 = $9e82
		lda #<(OptionsChar+$08)
		sta pixel_put_ptr
		lda #>(OptionsChar+$08)
		sta pixel_put_ptr+1				; pixel_put_ptr = $2808
		jsr UncompressChar				; copy menu user font
		
; $847f Copy user chars to menu user font
		ldx #$1f
ucmcopy:lda UserOptionsChar,x			; copy 32 bytes to menu user font
		sta OptionsChar+$a8,x
		dex
		bpl ucmcopy						; next byte