; pm500dat.b
; pm500 C64 + P500 common data2
; -------------------------------------------------------------------------------------------------
; $9b10 Monster data
MonsterUp:
		!byte $38, $7c, $d6, $d6, $d6, $fe, $fe, $fe, $fe, $fe
MonsterDown:
		!byte $38, $7c, $fe, $fe, $fe, $fe, $d6, $d6, $d6, $fe
MonsterLeft:
		!byte $38, $7c, $fe, $fe, $ae, $ae, $ae, $fe, $fe, $fe
MonsterRight:
		!byte $38, $7c, $fe, $fe, $ea, $ea, $ea, $fe, $fe, $fe
MonsterFlight:
		!byte $38, $7c, $fe, $d6, $d6, $d6, $fe, $d6, $aa, $fe
MonsterEyes:
		!byte $00, $00, $00, $28, $28, $28, $00, $00, $00, $00
; $9b4c Pacman shape data
PacmanDot:
		!byte $38, $7c, $fe, $fe, $fe, $fe, $fe, $fe, $7c, $38	; pacman not moving
PacmanRight:
		!byte $38, $7c, $fe, $f8, $e0, $e0, $f8, $fe, $7c, $38	; pacman right
		!byte $38, $7c, $f8, $f0, $e0, $e0, $f0, $f8, $7c, $38		
PacmanLeft:
		!byte $38, $7c, $fe, $3e, $0e, $0e, $3e, $fe, $7c, $38	; pacmon left
		!byte $38, $7c, $3e, $1e, $0e, $0e, $1e, $3e, $7c, $38
PacmanTop:
		!byte $00, $44, $c6, $c6, $ee, $ee, $fe, $fe, $7c, $38	; pacmen up
PacmanDie:
		!byte $00, $00, $82, $c6, $ee, $ee, $fe, $fe, $7c, $38	; pacman dies
PacmanBottom:
		!byte $38, $7c, $fe, $fe, $ee, $ee, $c6, $c6, $44, $00	; pacman down
		!byte $38, $7c, $fe, $fe, $ee, $ee, $c6, $82, $00
; $9ba5
PacmanIndex:
		!byte $00, $0a
; $9ba7
FizzieData:
		!byte $00, $82, $00, $c6, $82, $00, $7c, $38, $10
		!byte $7c, $38, $38, $10, $10
; $9bb5 Pacman sprite data pointers
PacmanDataPointers:
		!word PacmanDot
		!word PacmanRight
		!word PacmanLeft
		!word PacmanTop
		!word PacmanBottom
; $9bbf Sprite init data: positions/directions -> copied to $3c-$4f
SpriteInitData:
		!byte $e3, (>GameScreen)+2 
		!byte $02, $a6, $7a, $64, $74, $74, $74, $a4
		!byte $7c, $7c, $70, $88, $7c, $04, $02, $01
		!byte $01, $04
; $9bd3 Pacman maze direction data
; 		This data is used to determine which direction is permitted
VTable:
		!byte $2c, $44, $54, $64, $74, $84, $94, $a4, $b4, $c4
; $9bdd
HTable:
		!byte $3a, $46, $52, $62, $76, $82, $96, $a6, $b2, $be
; $9be7 Horizontal table address pointers
HorizontalTablePointers:
		!word HTab01
		!word HTab02
		!word HTab03
		!word HTab04
		!word HTab05
		!word HTab06
		!word HTab07
		!word HTab08
		!word HTab09
		!word HTab10
; $9bfb HWALL1-5
HWall1:	!byte $7c, $ff
HWall2: !byte $58, $7c, $9e, $ff
HWall3: !byte $58, $9e, $ff
HWall4:	!byte $3c, $ac, $ff
HWall5: !byte $ff
; $9c08
HWalls:
		!byte $00, $0c, $02, $06, $00, $06, $00, $09, $02, $0c
;
VWall1:	!byte $64, $84, $ff
VWall2:	!byte $38, $4c, $64, $84, $9c, $bc, $ff
VWall3:	!byte $3c, $ff
VWall4:	!byte $38, $5c, $9c, $bc, $ff
VWall5:	!byte $4c, $74, $8c, $ac, $ff
; $9C28
VWalls:
		!byte $00, $03, $0a, $0c, $11, $11, $0c, $0a
		!byte $03, $00
; $9c32 SpriteColors Sprite color table
SpriteColors:
		!byte RED, LIGHTRED, GREEN, ORANGE	; Lightred=Pinky, Lightred=Pinky
		!byte YELLOW, BROWN, ORANGE, GRAY3	; Green=Inky, Orange=Clyde
; $9c3a	colors - not used	
		!byte $0f, $0a, $0f, $0d
; $9c3e Timer values for blue monsters
BlueStartValues:
		!byte $90, $60, $30, $04, $00, $00
; $9c44
BlueTimerValues:
		!byte $ff, $c0, $80, $40, $00
		!byte $c0, $00, $00, $00, $c0
		!byte $00, $00, $00, $40, $00
; $9c53 Pacman explosion
PacmanExplosion:
		!byte $00, $00, $00, $00, $00, $92, $54, $00
		!byte $c6, $00, $54, $92
; $9c5f Score data for Pacman eating blue monsters -> pacman sprite
BlueScore0:	; Right half of all scores
		!byte $00, $00, $00, $00, $c6, $29, $29, $29, $29, $29, $c6, $00
BlueScore1:	; Left half of 200
		!byte $00, $00, $00, $00, $38, $45, $05, $19, $21, $41, $7c, $00
BlueScore2:	; Left half of 400
		!byte $00, $00, $00, $00, $08, $19, $29, $49, $7d, $09, $08, $00
BlueScore3: ; Left half of 800
		!byte $00, $00, $00, $00, $38, $45, $45, $39, $45, $45, $38, $00
BlueScore4:	; Left half of 1600
		!byte $00, $00, $00, $00, $8c, $91, $a1, $b9, $a5, $a5, $98, $00, $00
; $9c9c Hi notes for intro
NotesHi:
		!byte $0c, $08, $08, $21, $00, $19, $00, $15
		!byte $0c, $21, $19, $08, $00, $15, $15, $00
		!byte $0d, $08, $08, $23, $00, $1a, $00, $16
		!byte $0d, $23, $1a, $08, $00, $16, $16, $00
		!byte $0c, $08, $08, $21, $00, $19, $00, $15
		!byte $0c, $21, $19, $08, $00, $15, $15, $00
		!byte $0c, $15, $16, $17, $00, $17, $19, $1a
		!byte $00, $1a, $1c, $1d, $00, $21, $21, $00
; $9cdc Lo notes for intro
NotesLo:
		!byte $8f, $61, $61, $87, $00, $1e, $00, $1f
		!byte $8f, $87, $1e, $61, $00, $1f, $1f, $00
		!byte $4e, $e1, $e1, $86, $00, $9c, $00, $60
		!byte $4e, $86, $9c, $e1, $00, $60, $60, $00
		!byte $8f, $61, $61, $87, $00, $1e, $00, $1f
		!byte $8f, $87, $1e, $61, $00, $1f, $1f, $00
		!byte $8f, $1f, $60, $b5, $00, $b5, $1e, $9c
		!byte $00, $9c, $31, $df, $00, $87, $87, $00
; $9d1c Blue Score Pointers -> pacman sprite
BlueScorePointers:
		!word BlueScore1	; 200
		!word BlueScore2	; 400
		!word BlueScore3	; 800
		!word BlueScore4	; 1600
; $9d24 Eating dots sounds data
EatingDotsSoundData1:
		!byte $19, $1a, $1c, $1d, $20, $23, $00
; $9d2b 
EatingDotsSoundData2:
		!byte $23, $1d, $1a, $17, $15, $12, $00
; $9d32
MonsterStartPointer:
		!word RedStart
		!word PinkStart
		!word GreenStart
		!word YellowStart
; $9d3a Pattern start H-Pposition & V-Position
PatternStartHV:
		!byte $96, $a4	; #1
		!byte $62, $74	; #2
		!byte $82, $64	; #3
		!byte $62, $64	; #4
		!byte $62, $94	; #5
		!byte $52, $74	; #6
		!byte $96, $94	; #7
		!byte $a6, $74	; #8
		!byte $96, $54	; #9
		!byte $52, $b4	; #10
		!byte $be, $c4	; #11
		!byte $82, $44	; #12
		!byte $52, $a4	; #13
		!byte $b2, $b4	; #14
		!byte $82, $44	; #15
		!byte $52, $44	; #16
; $9d5a Home corner H-position & V-position
HomePositionHV:
		!byte $be, $2c
		!byte $3a, $2c
		!byte $be, $c4
		!byte $3a, $c4
; $9d62 Pattern index values
PatternIndex:
		!byte $00, $0b, $16, $21, $2c, $33, $3a, $41
		!byte $48, $53, $60, $6d, $80, $99, $b6, $c9
; $9d72 Fruit chars for screen
FruitChars:
		!byte $3a, $3c, $3e, $3e, $40, $40	; first of two fruit chars: $3a,$3b = cherry
		!byte $42, $42, $46, $46, $4a, $4a
; Fruit data for fruit line when maze count > 5
		!byte $4c, $4c, $4c, $4c, $4c, $4c
		!byte $4c, $4a, $4a, $48, $48, $44
HighFruitChars:
		!byte $44, $40, $40, $3e, $3e, $3c, $3a
; $9d91 Fruit scores
FruitScores:
		!byte $00, $4e, $4f, $5f, $60	; $00 chars for 100 points
		!byte $00, $50, $51, $5f, $60	; $05 chars for 300 points
		!byte $00, $52, $53, $5f, $60	; $0a chars for 500 points
		!byte $00, $54, $55, $5f, $60	; $0f chars for 700 points
		!byte $56, $57, $5e, $5f, $60	; $14 chars for 1000 points
		!byte $58, $59, $5e, $5f, $60	; $19 chars for 2000 points
		!byte $5a, $5b, $5e, $5f, $60	; $1e chars for 3000 points
		!byte $5c, $5d, $5e, $5f, $60	; $23 chars for 5000 points
; $9db9 Index values for fruit scores
FruitScoresIndex:
		!byte $00, $05, $0a, $0a, $0f, $0f, $14, $14, $19, $19, $1e, $1e, $23
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
DifficultyTable1:
		!byte $08, $08, $08, $0c, $10, $14, $14
; $9e0c
DifficultyTable2:
		!byte $00, $04, $08, $08, $0c, $10, $10
; $9e13 unused
Text_HighScore
		!byte $a8, $a9, $a7, $a8, $80, $b3, $a3, $af, $b2, $a5
; $9e1d
Text_Atari1983:
		!byte $88, $a3, $89, $80, $a1, $b4, $a1, $b2
		!byte $a9, $80, $91, $99, $98, $93
; $9e2b
Text_PressF1To:		
		!byte $b0, $b2, $a5, $b3, $b3, $80, $a6, $91, $80, $b4, $af, $80
; $9e37
Text_PressF3To:	
		!byte $b0, $b2, $a5, $b3, $b3, $80, $a6, $93, $80, $a6, $af, $b2
; $9e43
Text_PressF5To:
		!byte $b0, $b2, $a5, $b3, $b3, $80, $a6, $95, $80, $b4, $af, $80  
; $9e4f
Text_PlayGame:
		!byte $b0, $ac, $a1, $b9, $80, $a7, $a1, $ad, $a5
; $9e58
Text_PlayerGame:
		!byte $b0, $ac, $a1, $b9, $a5, $b2, $80, $a7, $a1, $ad, $a5
; $9e63
Text_ChangeDifficulty:
		!byte $a3, $a8, $a1, $ae, $a7, $a5, $80, $a4
		!byte $a9, $a6, $a6, $a9, $a3, $b5, $ac, $b4
		!byte $b9
; $9e74
DifficultyFruitsOptions:
		!byte $3a, $3c, $3e, $3e, $40, $40, $44, $44 ; first of two fruit chars: $3a,$3b = cherry
		!byte $48, $48, $4a, $4a, $4c, $4c
; -------------------------------------------------------------------------------------------------
; $9e82 encoded options user chars (bytes 0-$3f from FontData, bit 6+7 = count)
EncodedOptionsChar:
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
!ifndef	P500{
; $9ee4 
UserOptionsChar:
		!byte $81, $83, $83, $87, $87, $8f, $8f, $00
		!byte $fc, $de, $fe, $ff, $ff, $ff, $ff, $00
		!byte $0f, $0f, $0f, $0f, $0f, $8f, $8f, $00
		!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $00
}
; -------------------------------------------------------------------------------------------------
; $9f04 sprite 0-4 MSB bit set table
SpriteSetMSBMask:
		!byte $01, $02, $04, $08, $10
; $9f09 sprite 0-4 MSB bit clear table
SpriteClearMSBMask:
		!byte $fe, $fd, $fb, $f7, $ef