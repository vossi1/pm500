; Pacman C64 compressed nibbles
; -------------------------------------------------------------------------------------------------
; $9f0e Nibble Table
CompressedNibbles:
; FizzieIndex
		!byte $67, $78, $88, $99, $9a, $ab, $ab
; HTab01-10
		!byte $ac, $ec, $6a, $ce, $c6
		!byte $bc, $fe, $dd, $ef, $c7 
		!byte $9c, $79, $6a, $5b, $c5
		!byte $00, $3a, $dd, $63, $00
		!byte $cc, $f7, $00, $bf, $cc
		!byte $00, $3b, $cc, $73, $00
		!byte $ac, $fd, $6a, $df, $c6
		!byte $96, $be, $dd, $e7, $a5
		!byte $ad, $59, $6a, $59, $d6
		!byte $9c, $cc, $dd, $cc, $c5
; Timer values for flashing monsters		
		!byte $bb, $bb, $bb, $bb, $7b
		!byte $b7, $77, $b7, $17, $11
; Startup paths for monsters leaving box in center (red,pink,green,yellow each ends with $f)
		!byte $44, $24, $11, $18, $88, $88, $f4, $42
		!byte $41, $11, $f8, $82, $22, $82, $f8, $82, $24, $44, $24, $2f
; Patterns for monsters to run Pattern01-16 (each ends with 0)
		!byte $24, $24, $14, $18, $88, $01, $88, $82
		!byte $24, $44, $10, $44, $22, $88, $81, $14
		!byte $08, $14, $14, $22, $28, $10, $82, $44
		!byte $18, $08, $22, $41, $10, $42, $88, $14
		!byte $04, $22, $81, $10, $18, $22, $24, $14
		!byte $18, $01, $82, $82, $44, $44, $18, $80
		!byte $14, $41, $42, $42, $88, $88, $01, $88
		!byte $24, $24, $24, $42, $41, $11, $88, $80
		!byte $14, $42, $82, $42, $88, $88, $14, $18
		!byte $14, $11, $42, $22, $01, $81, $44, $22
		!byte $88, $24, $44, $41, $81, $41, $81, $18
		!byte $22, $22, $80, $82, $42, $82, $24, $44
		!byte $11, $81, $41, $88, $02, $44, $18, $81
		!byte $88, $28, $18, $88, $82, $44, $42, $42
		!byte $41, $41, $40
; Fruit score table for computing score for active player - 2 #S
		!byte $01, $03, $05, $05, $07, $07, $10, $10
		!byte $20, $20, $30, $30, $50
; Speed sequencing values Speed1-6
		!byte $33, $33
		!byte $32, $32
		!byte $22, $23
		!byte $22, $22
		!byte $22, $21
		!byte $12, $12
; RevTab, BlueTab (each 9 nibbles)
		!byte $0d, $e0, $70, $00, $b0, $21, $08, $00, $04
; ?
		!byte $57, $41, $42, $58, $9b, $4c