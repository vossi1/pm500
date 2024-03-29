;
; PACMAN MONSTER SUBROUTINES
;
;
EYONLY LDA #0
 STA TWEETR
 LDX #3
EYONLP LDA M1STAT,X
 CMP #$44
 BEQ NXTEYD
 ASL A
 BPL NXTEYD
 CPX GULPED
 BEQ NXTEYD
 INC TWEETR
 LDA M1HPOS,X
 TAY
 LDA M1VPOS,X
 CPY #$7C
 BNE TSTEYV
 CMP #$64
 BNE TSTEYV
 LDA M1DIRT,X
 CMP #1
 BNE EYHOME
 LDA #4
 STA M1DIRT,X
 LDA M1VPOS,X
 BNE TSTEYV
EYHOME LDA #$44
 STA M1STAT,X
 LDA #$FF
 STA M1SSEQ,X
 BNE NXTEYD
TSTEYV JSR MAZHND
 BCC SAMEYD
 CLC
 LDA #$7C
 STA M1THPS,X
 LDA #$64
 STA M1TVPS,X
 JSR MDIRCT
SAMEYD LDA M1DIRT,X
 JSR MONHND
NXTEYD DEX
 BPL EYONLP
 LDA TWEETR
 BNE GTWEET
 LDA #0
 STA TWEETF
 BEQ CKINBX
GTWEET LDA VFREEZ
 BNE CKINBX
 JSR VTWEET
CKINBX LDX #3
NMEYLP LDA M1STAT,X
 CLC
 LSR A
 LSR A
 LSR A
 BCC NXNMEY
 LDA M1STAT,X
 ASL A
 BPL MONOUT
 JSR PNKMIN
 JMP NXNMEY
MONOUT LDA RTCLOK+2
 AND #3
 BNE NXNMEY
 CPX #3
 BNE GRNOUT
 JSR YELMOT
 JMP NXNMEY
GRNOUT CPX #2
 BNE REDOUT
 JSR GRNMOT
 JMP NXNMEY
REDOUT JSR PNKMOT
NXNMEY DEX
 BPL NMEYLP
EYONLX RTS
;
PNKMOT LDA M1VPOS,X
 CMP #$64
 BNE PNKOUT
 LDA M1STAT,X
 BNE PNKTST
 LDA #1
 BNE PNKCHS
PNKTST AND #$80
 ORA #2
 BPL PNKCHS
 STA CHASET
 LDY #8
 BNE STRDIR
PNKCHS LDY #4
STRDIR STY M1DIRT,X
STRSTS STA M1STAT,X
 RTS
PNKOUT LDA #1
PNKOT1 STA M1DIRT,X
 JMP MONHND
;
REINCR LDA M1STAT,X
 AND #$0F
 STA M1STAT,X
 LDA COLORS,X
 STA PCOLR0,X
 RTS
;
GRNMOT LDA M1HPOS,X
 CMP #$7C
 BEQ PNKMOT
GRNOT1 LDA #8
 BNE PNKOT1
;
YELMOT LDA M1HPOS,X
 CMP #$7C
 BEQ PNKMOT
YELOUT LDA #4
 BNE PNKOT1
;
PNKMIN LDA M1VPOS,X
 CMP #$74
 BNE PNKMDN
 CPX #2
 BEQ GRNMIN
 CPX #3
 BEQ YELMIN
 BNE REINCR
PNKMDN LDA #2
 BNE PNKOT1
;
GRNMIN LDA M1HPOS,X
 CMP #$70
 BNE YELOUT
 BEQ REINCR
;
YELMIN LDA M1HPOS,X
 CMP #$88
 BNE GRNOT1
 BEQ REINCR
;
STRTMN LDX #1
CKSSQL LDA M1STAT,X
 AND #$7F
 BNE NXCKSS
 LDA M1TIMR,X
 BEQ CKSMUP
 DEC M1TIMR,X
 JSR BOUNCE
 JMP NXCKSS
CKSMUP CPX #3
 BNE CKM2UP
 JSR YELMOT
 JMP NXCKSS
CKM2UP CPX #2
 BNE CKM1UP
 JSR GRNMOT
 JMP NXCKSS
CKM1UP JSR PNKMOT
NXCKSS INX
 CPX #4
 BNE CKSSQL
 RTS
;
; STARTUP SEQUENCE OF
; MONSTERS EXITING FROM
; BOX.
;
BOUNCE LDA M1DIRT,X
 CMP #1
 BNE TSUPSQ
 LDA M1VPOS,X
 CMP #$70
 BNE NXTBNC
 LDA #2
 BNE STSQST
TSUPSQ LDA M1VPOS,X
 CMP #$78
 BNE NXTBNC
 LDA #1
STSQST STA M1DIRT,X
NXTBNC LDA M1DIRT,X
 JMP MONHND
;
MONSTR LDA M1STAT,X
 BPL MNTST1
 LSR A
 LSR A
 LSR A
 BCC MNTSCH
 RTS
MNTST1 CMP #1
 BEQ MSTRTP; MONSTER START PATH
MNTST2 CMP #2
 BNE MNTST3
 LDA CHASET
 BNE MNTSCH
 JMP GOHOME
MNTSCH JMP MCHASE; MONSTER IS CHASING
MNTST3 CMP #8
 BNE MNTST4
 JMP SEEKPS; SEEK PATTERN START
MNTST4 CMP #$10
 BNE MNTST5
 JMP MPATRN; CONTINUE PATTERN
MNTST5 CMP #$20
 BNE MNTST6
 JMP GOHOME; SEEK HOME CORNER
MNTST6 RTS

MSTRTP JSR MAZHND
 BCC SAMPTH
 CLC
 INC M1SSEQ,X
SAMPTH TXA
 ASL A
 TAY
 LDA STRADD,Y
 STA PIXGET
 INY
 LDA STRADD,Y
 STA PIXGET+1
 LDA M1SSEQ,X
 TAY
 LDA (PIXGET),Y
 CMP #$0F
 BNE MNPATH
;
; - MONSTER HAS REACHED START POSIT
;   SET UP TO FIND PATTERN START
;   AND SET TIMER FOR 20 SECS
;
 CPX #0
 BNE SETPAT
 LDA #$50
 STA CHASET
 JMP SCHASE
SETPAT LDA #8
 STA M1STAT,X
 LDA #0
 STA M1PCNT,X
 LDA RANDOM
 AND #$0F
 STA M1PIDX,X
 ASL A
 TAY
 LDA PTRNHV,Y
 STA M1THPS,X
 INY
 LDA PTRNHV,Y
 STA M1TVPS,X
 LDA #$96
 STA M1TIMR,X
 BNE SEEKPS
MNPATH STA M1DIRT,X
 JMP MONHND
;
; GO FIND START OF PATTERN
;
SEEKPS LDA M1TIMR,X
 BNE SEKPS1
 JMP GOHOME
SEKPS1 JSR MAZHND
 BCS SEKPS2
FINDST JMP SAMEMD
SEKPS2 CLC
 JSR MDIRCT
 LDA M1HDIR,X
 BNE FINDST
 LDA M1VDIR,X
 BNE FINDST
 LDA #$10; REACHED PATTERN START
 STA M1STAT,X
 CLC
 BCC SMPTRN
;
MPATRN LDA M1TIMR,X
 BEQ GOHOME
MPTRN1 JSR MAZHND
 BCC SMPTRN
 CLC
 INC M1PCNT,X
SMPTRN LDA M1PIDX,X
 TAY
 LDA PTNIDX,Y
 ADC M1PCNT,X
 TAY
 LDA PTRN01,Y
 BNE MPTRN2
 STA M1PCNT,X
 LDA M1PIDX,X
 TAY
 LDA PTNIDX,Y
 TAY
 LDA PTRN01,Y
MPTRN2 STA M1DIRT,X
 JMP SAMEMD
;
GOHOME LDA #$20
 STA M1STAT,X
 TXA
 ASL A
 TAY
 LDA HOMEHV,Y
 STA M1THPS,X
 INY
 LDA HOMEHV,Y
 STA M1TVPS,X
;
 JSR MAZHND
 BCC SAMEMD
 CLC
 JSR MDIRCT
 LDA M1HDIR,X
 BNE SAMEMD
 LDA M1VDIR,X
 BNE SAMEMD
 JMP SETPAT
;
; TAKES CARE OF CHASE & FLIGHT
;
SCHASE LDA #2
 STA M1STAT,X
MCHASE LDA PMHPOS
 STA M1THPS,X
 LDA PMVPOS
 STA M1TVPS,X
 JSR MAZHND
 BCC SAMEMD
 CLC
 JSR MDIRCT
SAMEMD LDA M1DIRT,X
;
; MONSTER DISPLAY HANDLER
;
; X-REG VALUE 0-3 TO SELECT
; MONSTER 1 - MONSTER 4
;
; A-REG VALUE  1 EQUALS UP
;              2 EQUALS DOWN
;              4 EQUALS LEFT
;              8 EQUALS RIGHT
;
; SELECTED MONSTER WILL BE 
; INCREMENTED 1 PIXEL IN THE
; DIRECTION SPECIFIED.
;
; IF MSTILL > 0 THEN THE MONSTERS
; WILL BE DRAWN BUT NOT MOVED.
;
; BIT 7 OF STATUS = FLIGHT IMAGE
; BIT 6 OF STATUS = EYES IMAGE
;
MONHND CMP #1; DO REPOSITIONING
 BNE TSTMDN
 LDY MSTILL
 BNE MSTLUP
 DEC M1VPOS,X
 DEC M1VPOS,X
MSTLUP LDA #MONSUP-MONSUP
 BEQ TSTMSK
TSTMDN CMP #2
 BNE TSTMLF
 LDY MSTILL
 BNE MSTLDN
 INC M1VPOS,X
 INC M1VPOS,X
MSTLDN LDA #MONSDN-MONSUP
 BNE TSTMSK
TSTMLF CMP #4
 BNE TSTMRT
 LDY MSTILL
 BNE MSTLLF
 DEC M1HPOS,X
MSTLLF LDA #MONSLF-MONSUP
 BNE TSTMSK
TSTMRT CMP #8
 BNE NOMMOT
 LDY MSTILL
 BNE MSTLRT
 INC M1HPOS,X
MSTLRT LDA #MONSRT-MONSUP
 BNE TSTMSK
NOMMOT RTS
TSTMSK LDY M1STAT,X
 BPL TSTEYE
 LDA #MONSFL-MONSUP; FLIGHT IMAGE
TSTEYE TAY
 LDA M1STAT,X
 ASL A
 BMI MNEYES
 TYA
 LDY MSKIRT
 BEQ MSKL54
 LDY #$AA
 BNE MNSKRT
MSKL54 LDY #$54
 BNE MNSKRT
MNEYES LDA #MONSEY-MONSUP
 LDY #0
MNSKRT STY MONBUF+$0C
 CLC
 ADC #LOW MONSUP
 STA PIXGET
 LDA #0
 ADC #HIGH MONSUP
 STA PIXGET+1
 LDY #9
MNLDBL LDA (PIXGET),Y
 STA MONBUF+2,Y
 DEY
 BPL MNLDBL
 LDA #LOW MONBUF
 STA PIXGET
 LDA #HIGH MONBUF
 STA PIXGET+1
 LDA M1HPOS,X
 STA HPOSP0,X
 LDA M1VPOS,X
 STA PIXPUT
SECURE LDA #HIGH PMADDR+4
 STA PIXPUT+1
 TXA
 CLC
 ADC PIXPUT+1
 STA PIXPUT+1
 LDY #$0F
MWRITE LDA (PIXGET),Y
 STA (PIXPUT),Y
 DEY
 BPL MWRITE
 RTS
;
; DIRECTIONS COMPUTED FOR
; MONSTERS
;
; X REG = MONSTER #
;
; TARGET VERTICAL & HORIZONTAL
; COORDINATES MUST BE PLACED
; INTO VSAVER & HSAVER PRIOR
; TO ENTRY
;
MDIRCT LDA M1VPOS,X
 CMP M1TVPS,X
 BEQ VEQUAL
 BCC VGRATR;MONSTER IS ABOVE TARGET
;
; MONSTER IS BELOW TARGET
;
 LDA M1STAT,X
 BMI VBIGGR
VLESSR LDA #1;POINT TO UP
 BNE STRVRT
VGRATR LDA M1STAT,X
 BMI VLESSR
VBIGGR LDA #2;POINT TO DOWN
 BNE STRVRT
VEQUAL LDA #0
STRVRT STA M1VDIR,X
 LDA M1HPOS,X
 CMP M1THPS,X
 BEQ HEQUAL
 BCS HGRATR;MONSTER IS RT OF TARGET
;
; MONSTER IS LEFT OF TARGET
;
 LDA M1STAT,X
 BMI HBIGGR
HLESSR LDA #8;POINT TO RT
 BNE STRHRZ
HGRATR LDA M1STAT,X
 BMI HLESSR
HBIGGR LDA #4;POINT TO LEFT
 BNE STRHRZ
HEQUAL LDA #0
STRHRZ STA M1HDIR,X
;
 LDA M1VDIR,X
 BEQ MHONLY
 BIT TEMLOC
 BEQ MHONLY
 LDA M1HDIR,X
 BEQ MVRTOK
 BIT TEMLOC
 BEQ MVRTOK
;
; CHOICE OF DIRECTIONS
;
 LDA RANDOM
 BMI MHRZOK
MVRTOK LDA M1VDIR,X
 BNE SETMDR
MHRZOK LDA M1HDIR,X
 BNE SETMDR
MHONLY LDA M1HDIR,X
 BIT TEMLOC
 BNE MHRZOK
;
 LDA RANDOM
 AND TEMLOC
 BNE UPTEST
 LDA TEMLOC;RELOAD ORIGINAL
UPTEST LSR A
 BCC DNTEST
 LDA #1
 BNE SETMDR
DNTEST LSR A
 BCC LFTEST
 LDA #2
 BNE SETMDR
LFTEST LSR A
 BCC RTTEST
 LDA #4
 BNE SETMDR
RTTEST LDA #8
SETMDR STA M1DIRT,X
SAMMDR LDA M1DIRT,X
 RTS
;
SEEPAC LDA M1VPOS,X
 CMP PMVPOS
 BEQ SEEVRT
 LDA M1HPOS,X
 CMP PMHPOS
 BEQ SEEHRZ
 RTS
SEEVRT LDY #9
SEEVRL LDA VTABLE,Y
 CMP PMVPOS
 BEQ SEEVR1
 DEY
 BPL SEEVRL
SEEVRX RTS
SEEVR1 LDA M1HPOS,X
 CMP PMHPOS
 BCS SEELFT
SEERGT LDA M1DIRT,X
 CMP #8
 BNE SEEVRX
SEERT1 LDA HWALLS,Y
 CMP #$FF
 BEQ SEECHS
 CMP M1HPOS,X
 BCS SEERT2
 INY
 BNE SEERT1
SEERT2 CMP PMHPOS
 BCS SEECHS
 RTS
;
SEELFT LDA M1DIRT,X
 CMP #4
 BNE SEEVRX
SEELF1 LDA HWALLS,Y
 CMP #$FF
 BEQ SEECHS
 CMP PMHPOS
 BCS SEELF2
 INY
 BNE SEELF1
SEELF2 CMP M1HPOS,X
 BCS SEECHS
 RTS
;
SEEHRZ LDY #9
SEEHRL LDA HTABLE,Y
 CMP PMHPOS
 BEQ SEEHR1
 DEY
 BPL SEEHRL
SEEHRX RTS
SEEHR1 LDA M1VPOS,X
 CMP PMVPOS
 BCC SEEDWN
;
SEEUPP LDA M1DIRT,X
 CMP #1
 BNE SEEHRX
SEEUP1 LDA VWALLS,Y
 CMP #$FF
 BEQ SEECHS
 CMP PMVPOS
 BCS SEEUP2
 INY
 BNE SEEUP1
SEEUP2 CMP M1VPOS,X
 BCS SEECHS
 RTS
;
SEEDWN LDA M1DIRT,X
 CMP #2
 BNE SEEHRX
SEEDN1 LDA VWALLS,Y
 CMP #$FF
 BEQ SEECHS
 CMP M1VPOS,X
 BCS SEEDN2
 INY
 BNE SEEDN1
SEEDN2 CMP PMVPOS
 BCC SEECHX
;
SEECHS CPX #2
 BNE STRCHS
 LDA RTCLOK+2
 BMI SEECHX
STRCHS LDA #2
 STA M1STAT,X
 LDA #$A0
 STA CHASET
SEECHX RTS
;
; INTERMISSION CODE
;
INTMIS LDX PLYNUM
 LDA MAZCT1,X
 LDX #$00
 CMP #$01
 BEQ L7221
 INX 
 INX 
 CMP #$04
 BEQ L7221
 INX 
 INX 
 CMP #$08
 BEQ L7221
 CMP #$0C
 BEQ L7221
 CMP #$10
 BEQ L7221
 JMP L73AA
L7221 STX INTMOD
 LDA #$FF
 STA INTCNT
L7228 LDA INTCNT
 BNE L7228
 LDX #$1C
L722E LDA INTDLC,X
 STA INTRDL,X
 DEX 
 BPL L722E
 LDA #INDLI1&255
 STA VDSLST
 LDA #INDLI1/256
 STA VDSLST+1
 LDA #INTRDL&255
 STA SDLSTL
 STA DLISTL
 LDA #INTRDL/256
 STA SDLSTH
 STA DLISTH
 LDX #$03
L7251 STA HPOSM0,X
 DEX 
 BPL L7251
L7257 JSR INITPT
 LDX INTMOD
 LDA L756B,X
 TAY 
 LDX #$15
L7263 LDA L7571,Y
 STA PMADDR+$300,X
 DEY 
 DEX 
 BPL L7263
 LDA INTMOD
 CMP #$03
 BNE L727D
 LDA INTSEQ
 STA PMADDR+$31A
 JMP L72A1
L727D ROR A
 BCC L7288
 LDA #$2C
 LDX #$7C
 LDY #$CC
 BNE L7293
L7288 LDA #$FF
 STA PMADDR+$318
 LDA #$00
 LDX #$50
 LDY #$A0
L7293 STA INTRDL+$12
 STX INTRDL+$15
 STY INTRDL+$18
 LDA #$00
 STA HSCROL
L72A1 LDA #$FF
 STA INTCLK
L72A5 LDA #$FF
 STA INTCNT
L72A9 LDA INTCNT
 BNE L72A9
 LDA #$FF
 STA INTCNT
 LDA PMADDR+$312
 STA INTSEQ
 JSR L73E3
 LDA PMADDR+$317
 CMP PMADDR+$30D
 BCS L72C5
 JMP L7346
L72C5 LDA INTMOD
 CMP #$03
 BNE L72E9
 LDA PMADDR+$31E
 CMP #$13
 BEQ L72D6
 JMP L7365
L72D6 LDA #$8F
 STA PMADDR+$314
 LDA #$78
 STA PMADDR+$315
 LDA #$00
 TAX 
 JSR L746A
 JMP L72A5
L72E9 CMP #$02
 BNE L733E
 LDA PMADDR+$31F
 BEQ L7319
 JSR L74F7
 LDX #$01
 JSR L744B
 LDA PMADDR+$320
 CMP #$07
 BEQ L7365
 LDA PMADDR+$305
 CLC 
 ADC PMADDR+$307
 STA PMADDR+$307
 BCC L72A5
 JSR L741D
 JSR L7470
 INC PMADDR+$320
 JMP L72A5
L7319 LDA PMADDR+$31B
 CMP #$13
 BNE L733E
 LDA #$01
 STA PMADDR+$31F
 LDY #$18
L7327 LDA L75F5,Y
 STA PMADDR+$300,Y
 DEY 
 BPL L7327
 LDA #$00
 STA PMADDR+$31C
 LDA INTSEQ
 STA PMADDR+$312
 JMP L7346
L733E JSR L7411
 LDX #$00
 JSR L744B
L7346 LDA PMADDR+$310
 BEQ L735B
 LDA PMADDR+$317
 CMP PMADDR+$30E
 BCC L735B
 JSR L74F7
 LDX #$01
 JSR L744B
L735B LDA PMADDR+$317
 CMP #$FF
 BEQ L7365
 INC PMADDR+$317
L7365 LDA PMADDR+$31E
 CMP #$3C
 BEQ L736F
 JMP L72A5
L736F INC INTMOD
 LDA INTMOD
 CMP #$03
 BNE L739C
 JSR L741D
 JSR L741D
 LDA #L787A&255
 STA PMADDR+$314
 LDA #L787A/256
 STA PMADDR+$315
 LDA #$06
 STA PMADDR+$30F
 LDA #$00
 TAX 
 JSR L746A
 LDA #$01
 STA PMADDR+$31E
 JMP L72A5
L739C ROR A
 BCC L73A2
 JMP L7257
L73A2 LDA #$FF
 STA INTCNT
L73A6 LDA INTCNT
 BNE L73A6
L73AA JSR SETNUL
 JSR INITPT
 LDA #$FF
 STA INTCNT
L73B4 LDA INTCNT
 BNE L73B4
 LDA #$00
 STA NMIEN
 STA IRQEN
 LDA #DLIST&255
 STA SDLSTL
 STA DLISTL
 LDA #DLIST/256
 STA SDLSTH
 STA DLISTH
 LDA #DLIV&255
 STA VDSLST
 LDA #DLIV/256
 STA VDSLST+1
 LDA #$C0
 STA NMIEN
 LDA #$40
 STA IRQEN
 RTS 
;
L73E3 LDA INTCLK
 CMP #$04
 BCC L740E
 LDA PMADDR+$31E
 ASL A
 ASL A
 TAX 
 LDA IMUSIC,X
 STA AUDF1
 LDA IMUSIC+1,X
 STA AUDC1
 LDA IMUSIC+2,X
 STA AUDF2
 LDA IMUSIC+3,X
 STA AUDC2
 INC PMADDR+$31E
 LDA #$FF
 STA INTCLK
L740E INC INTCLK
 RTS 
L7411 LDA PMADDR+$303
 CLC 
 ADC PMADDR+$318
 STA PMADDR+$318
 BCC L744A
L741D LDA PMADDR+$31A
 AND #$03
 TAX 
 EOR PMADDR+$300
 STA HSCROL
 TXA 
 BNE L7447
 LDX #$06
L742E LDA INTRDL+$12,X
 CLC 
 ADC PMADDR+$302
 STA INTRDL+$12,X
 CMP #$FF
 BNE L743F
 DEC INTRDL+$13,X
L743F DEX 
 DEX 
 DEX 
 BPL L742E
 INC PMADDR+$31B
L7447 INC PMADDR+$31A
L744A RTS 
L744B LDA PMADDR+$307,X
 CLC 
 ADC PMADDR+$305,X
 STA PMADDR+$307,X
 BCC L74AC
 INC PMADDR+$309,X
 LDA PMADDR+$309,X
 CMP PMADDR+$30B,X
 BCC L746D
 LDA #$00
 STA PMADDR+$309,X
 TXA 
 BNE L746D
L746A STA PMADDR+$31C
L746D TXA 
 BNE L74AD
L7470 LDA #$2C
 BIT PMADDR+$301
 BMI L747D
 LDA #$28
 SEC 
 SBC PMADDR+$30F
L747D TAX 
 LDA PMADDR+$30F
 STA PMADDR+$316
 LDY PMADDR+$31C
 LDA PMADDR+$314
 STA PIXGET
 LDA PMADDR+$315
 STA PIXGET+1
L7491 LDA (PIXGET),Y
 STA PMADDR+$400,X
 INY 
 LDA (PIXGET),Y
 STA PMADDR+$450,X
 INY 
 LDA (PIXGET),Y
 STA PMADDR+$4A0,X
 INY 
 INX 
 DEC PMADDR+$316
 BNE L7491
 STY PMADDR+$31C
L74AC RTS 
L74AD LDA #PMADDR/256+5
 STA PIXPUT+1
 LDA PMADDR+$313
 STA PIXPUT
 LDA PMADDR+$30A
 ASL A
 TAY 
 LDX INTMOD
 CPX #$01
 BEQ L74CB
 LDA L760E,Y
 LDX L760E+1,Y
 JMP L74D1
L74CB LDA L761A,Y
 LDX L761A+1,Y
L74D1 STA PIXGET
 STX PIXGET+1
 LDX PMADDR+$310
L74D8 LDY PMADDR+$311
 DEY 
L74DC LDA (PIXGET),Y
 STA (PIXPUT),Y
 DEY 
 BPL L74DC
 DEX 
 BEQ L74AC
 LDA PIXGET
 CLC 
 ADC PMADDR+$311
 STA PIXGET
 BCC L74F2
 INC PIXGET+1
L74F2 INC PIXPUT+1
 JMP L74D8
L74F7 LDA PMADDR+$304
 CLC 
 ADC PMADDR+$319
 STA PMADDR+$319
 BCC L751B
 LDA PMADDR+$312
 SEC 
 SBC PMADDR+$302
 STA PMADDR+$312
 LDX #$00
L750F STA HPOSP1,X
 CLC 
 ADC #$08
 INX 
 CPX PMADDR+$310
 BNE L750F
L751B RTS 
;
INDLI1 PHA 
 TXA 
 PHA 
 LDA #ICHARS/256
 STA CHBASE
 LDA #$2A
 STA COLPM1
 STA COLPM2
 STA COLPM3
 LDX INTMOD
 LDA ICOLR0,X
 STA COLPF0
 LDA ICOLR1,X
 STA COLPF1
 LDA ICOLR2,X
 STA COLPF2
 LDA ICOLR3,X
 STA COLPF3
 PLA 
 TAX 
 PLA 
 RTI
;
SETNUL LDA #NULLDL&255
 STA SDLSTL
 STA DLISTL
 LDA #NULLDL/256
 STA SDLSTH
 STA DLISTH
 RTS
;
INITPT LDX #0
 TXA
 JMP INITPL
;

 LDA #$C0
TNMSL1 CMP TUNCNT
 BEQ STTMSK
 DEC TUNCNT
 ASL TUNMSK
 BCS TNMSL1
 LDA