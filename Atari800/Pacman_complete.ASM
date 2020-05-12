;************************************
;*                                  *
;*            PAC-MAN               *
;*                                  *
;*   Developed for Atari Inc. by    *
;*  Roklan Corp. This Information   *
;*  is confidential and is not for  *
;*  sale or distribution.           *
;*                                  *
;*                                  *
;*                                  *
;*            10/03/82              *
;*                                  *
;*          DISK VERSION            *
;*                                  *
;*          REVISION 3.0            *
;*                                  *
;************************************
;
 include d2:systext.asm
;
 list i
;
;*******  zero page variables  ******
;
 org $0018
;
dlicnt ds 1 ;display list int. count
option ds 1 ;option mode flag	; 07
numply ds 1 ;number of players	; 08
difopt ds 1 ;difficulty option	; 09
rstrtf ds 1 ;restart flag		; 0A
atclok ds 1 ;attract countdown clock
attimr ds 1 ;attract mode timer	; 0C
atsequ ds 1 ;attract mode sequence #
;
gmovrf ds 1 ;game over flag				; 0E
readyf ds 1 ;ready flag					; 0F
introf ds 1 ;intro flag					; 10
swappf ds 1 ;swap player flag			; 11
resetf ds 1 ;reset flag					; 12
resett ds 1 ;reset timer				; 13
rrflag ds 1 ;rerack flag
rrsequ ds 1 ;rerack sequence #
rrtimr ds 1 ;rerack timer
rrflct ds 1 ;rerack flash count
temloc ds 1 ;temporary storage			; 18
plynum ds 1 ;player # 0=1 1=2			; 19
xpacp1 ds 1 ;extra pacman player 1		; 1A
xpacp2 ds 1 ;extra pacman player 2		; 1B
bpacp1 ds 1 ;bonus pacman player 1		; 1C
bpacp2 ds 1 ;bonus pacman player 2		; 1D
mazct1 ds 1 ;maze count player 1		; 1E
mazct2 ds 1 ;maze count player 2		; 1F
bigdt1 ds 1 ;big dot status #1			; 20
bigdt2 ds 1 ;big dot status #2
dtctl1 ds 1 ;dots eaten ctr lsb #1		; 22
dtctl2 ds 1 ;dots eaten ctr lsb #2
dtctm1 ds 1 ;dots eaten ctr msb #1		; 24
dtctm2 ds 1 ;dots eaten ctr msb #2
scnsc1 ds 2 ;screen score 1 address		; 26
scnsc2 ds 2 ;screen score 2 address		; 28
pixget ds 2 ;p/m pix get address		; 2A
pixput ds 2 ;p/m pix put address		; 2C
pacclr ds 1 ;attract pacman color
;
 org $0043
;
frutp1 ds 1 ;fruit counter #1			; 2E
frutp2 ds 1 ;fruit counter #2			; 2F
bcount ds 1 ;bounce timer count			; 30
savcns ds 1 ;save consol data
tunloc ds 2 ;used for tunnel logic
;
 org $005a
m1dely ds 1 ;monster 1 delay			; 34
m2dely ds 1 ;monster 2 delay
m3dely ds 1 ;monster 3 delay
m4dely ds 1 ;monster 4 delay			; 37
acolr1 ds 1 ;attract color 1 - $3a
acolr2 ds 1 ;attract color 2 - $44
acolr3 ds 1 ;attract color 3 - $2a
acolr4 ds 1 ;attract color 4 - $da
intcnt ds 1 ;intermission counter
intclk ds 1 ;intermission clock
 org $0080
;
pacscn ds 2 ;pacman screen mem addr		; 3c
pacbyt ds 1 ;pacman byte counter		; 3e
pvsave ds 1 ;pacman vpos save			; 3f
phsave ds 1 ;pacman hpos save			; 40
m1vpos ds 1 ;monster 1 vertical posit	; 41
m2vpos ds 1 ;monster 2 vertical posit
m3vpos ds 1 ;monster 3 vertical posit
m4vpos ds 1 ;monster 4 vertical posit
pmvpos ds 1 ;pacman vertical position	; 45
m1hpos ds 1 ;monster 1 horiz position	; 46
m2hpos ds 1 ;monster 2 horiz position
m3hpos ds 1 ;monster 3 horiz position
m4hpos ds 1 ;monster 4 horiz position
pmhpos ds 1 ;pacman horizontal posit	; 4a
m1dirt ds 1 ;monster 1 direction		; 4b
m2dirt ds 1 ;monster 2 direction
m3dirt ds 1 ;monster 3 direction
m4dirt ds 1 ;monster 4 direction
pmodir ds 1 ;pacman old direction		; 4f
scorex ds 6 ;player score for text		; 50-55
carryb ds 1 ;carry bit for above		; 56
pausef ds 1 ;pause flag					; 57
whiney ds 1 ;chase whine delta			; 58
frutmr ds 2 ;fruit timer (10 secs)		; 59
fruflg ds 1 ;fruit display flag			; 5b
fruclr ds 1 ;fruit color				; 5c
frscrf ds 1 ;fruit score flag			; 5d
frscrt ds 1 ;fruit score timer			; 5e
notcnt ds 1 ;note count for tunes		; 5f
vsaver ds 1 ;vertical posit save		; 60
hsaver ds 1 ;horizontal posit save		; 61
pacmap ds 1 ;pacman vert map counter	; 62
paccnt ds 1 ;pacman motion count
pacadv ds 1 ;pacman advance-turning
pacdly ds 1 ;pacman delay-eating dots
pmstat ds 1 ;pacman status
pmsequ ds 1 ;pacman sequence (mouth)
pmndir ds 1 ;pacman new direction
chaset ds 1 ;chase timer
mstill ds 1 ;monsters still flag
mskirt ds 1 ;monster skirt flag
m1spsq ds 1 ;monster 1 speed sequence
m2spsq ds 1 ;monster 2 speed sequence
m3spsq ds 1 ;monster 3 speed sequence
m4spsq ds 1 ;monster 4 speed sequence
pmspsq ds 1 ;pacman speed sequence
m1spct ds 1 ;monster 1 speed count
m2spct ds 1 ;monster 2 speed count
m3spct ds 1 ;monster 3 speed count
m4spct ds 1 ;monster 4 speed count
pmspct ds 1 ;pacman  speed count
m1pidx ds 1 ;monster 1 pattern index
m2pidx ds 1 ;monster 2 pattern index
m3pidx ds 1 ;monster 3 pattern index
m4pidx ds 1 ;monster 4 pattern index
m1pcnt ds 1 ;monster 1 pattern count
m2pcnt ds 1 ;monster 2 pattern count
m3pcnt ds 1 ;monster 3 pattern count
m4pcnt ds 1 ;monster 4 pattern count
m1thps ds 1 ;monster 1 target hpos
m2thps ds 1 ;monster 2 target hpos
m3thps ds 1 ;monster 3 target hpos
m4thps ds 1 ;monster 4 target hpos
m1tvps ds 1 ;monster 1 target vpos
m2tvps ds 1 ;monster 2 target vpos
m3tvps ds 1 ;monster 3 target vpos
m4tvps ds 1 ;monster 4 target vpos
m1timr ds 1 ;monster 1 timer
m2timr ds 1 ;monster 2 timer
m3timr ds 1 ;monster 3 timer
m4timr ds 1 ;monster 4 timer
m1stat ds 1 ;monster 1 status
m2stat ds 1 ;monster 2 status
m3stat ds 1 ;monster 3 status
m4stat ds 1 ;monster 4 status
m1sseq ds 1 ;monster 1 start sequence
m2sseq ds 1 ;monster 2 start sequence
m3sseq ds 1 ;monster 3 start sequence
m4sseq ds 1 ;monster 4 start sequence
m1vdir ds 1 ;monster 1 vert choice
m2vdir ds 1 ;monster 2 vert choice
m3vdir ds 1 ;monster 3 vert choice
m4vdir ds 1 ;monster 4 vert choice
m1hdir ds 1 ;monster 1 horz choice
m2hdir ds 1 ;monster 2 horz choice
m3hdir ds 1 ;monster 3 horz choice
m4hdir ds 1 ;monster 4 horz choice
vchasf ds 1 ;chase freq
vchasd ds 1 ;direction 1=inc 2=dec
vchass ds 1 ;chase sound start
vflitf ds 1 ;flight freq
vflitd ds 1 ;direction same
vflitv ds 1 ;flight volume
vflits ds 1 ;flight sound start
vfreez ds 1 ;freeze flag
vglpc1 ds 1 ;gulp count1
vglpc2 ds 1 ;gulp count2
gulped ds 1 ;last monster eaten
glpcnt ds 1 ;gulp count
fizzle ds 1 ;fizzle flag 1=fizl
fizptr ds 1 ;fizzle addr pointer
fiztim ds 1 ;fizzle timer
vfizst ds 1 ;fizzle status
vfizsq ds 1 ;fizzle sequence #
vfizbs ds 1 ;fizzle freq base
vfizfq ds 1 ;fizzle current freq
vfizct ds 1 ;fizzle counter
tweetr ds 1 ;tweet sound flag
tweetf ds 1 ;tweet sound freq
eaterf ds 1 ;eat dot sound flag
eaterc ds 1 ;eat dot sound count
eatert ds 1 ;eat dot toggle
gobbld ds 1 ;gobble dirctn (fruits)
gobblf ds 1 ;gobble frequency
blinkt ds 1 ;blink timer
flshup ds 1 ;flash 1up 2up timer
flasht ds 1 ;flash timer
flashc ds 1 ;flash count
flitmr ds 1 ;flight timer
tunmsk ds 1 ;tunnel bit mask
tuncnt ds 1 ;tunnel iteration count
;
 eject
;
 org $0600
;
pacbuf ds 16 ; pacman image buffer
monbuf ds 16 ; monster image buffer
;
intrdl equ $0680;intermission dl
intmod equ $06c0;intermission mode
intseq equ $06c1;intermission seq
;
;
;****   system variables    ****
;
bdelay equ 1  ;bounce delay
fdelay equ $ff;fruit timer value
adelay equ $30;attract timer delay
;
gammem equ $0800; game memory
;
pacmaz equ gammem+$0c00
p1save equ gammem+$1000
p2save equ gammem+$1400
optchr equ gammem+$1800
optscn equ gammem+$1c00
pmaddr equ gammem+$2000
text   equ gammem+$2000
patscn equ gammem+$2800
amcset equ gammem+$2c00
;
 eject
;
 org $3ffd
;
 jmp init
;
 include d2:pacdat1.asm
;
 eject
;
init ldx #9
initl1 lda hisctx,x
 sta text+$0f,x
 dex
 bpl initl1
 lda #0
 ldx #$27
initl2 sta dlicnt,x
 dex
 bpl initl2
 ldx #$7f
inipzl sta $80,x
 dex
 bpl inipzl
;
 inc option
 ldx rtclok+2
 dex
 stx atclok
;
; relocate option char set
;
 ldx #0
ocloop lda pacttl,x
 sta optchr+8,x
 inx
 bne ocloop
;
; build attract mode char set
;
anchrl lda chrorg,x
 sta amcset,x
 lda chrorg+$100,x
 sta amcset+$100,x
 inx
 bne anchrl
 ldx #$1f
ancdtl lda atchrs,x
 sta amcset+$18,x
 dex
 bpl ancdtl
;
; enable keyscan & set vbi vector
;
pacini sei
 lda pokmsk
 ora #$40
 sta pokmsk
 sta irqen
 cli
 ldy #low vblank
 ldx #high vblank
 lda #7
 sta skctl
 jsr setvbv;go set vblank vector
 lda #$c0;enable dli and vbi
 sta nmien
 lda #$3e
 sta sdmctl;enable p/m graphics
 lda #$3
 sta gractl
 lda #high pmaddr
 sta pmbase
 lda #low init
 sta dosvec
 lda #high init
 sta dosvec+1
 lda #low initad
 sta dosini
 lda #high initad
 sta dosini+1
;
reinit lda #0;new game vector
 ldx #$1f
reinlp sta gmovrf,x
 dex
 bpl reinlp
 txs
 jsr clraud
;
; now initialize player screens
;
 jsr p1init
 jsr savep1
 jsr savep2
;
 lda option
 bne loop
;
pacgam lda rtclok+2
pacgml cmp rtclok+2
 beq pacgml
 lda #low dlist
 sta sdlstl
 sta dlistl
 lda #high dlist
 sta sdlsth
 sta dlisth
 lda #low dliv; dli vector
 sta vdslst
 lda #high dliv
 sta vdslst+1
 lda #$11
 sta gprior;let missiles use colpf3
setpac jsr setup
 jsr newgam
 lda numply
 beq p1scin
 jsr set2pl
 jmp godoit
p1scin jsr set1pl
godoit lda #2
 sta rstrtf
;
loop lda rrsequ
 cmp #3
 bne goloop
 jsr intmis
 jsr setup
 inc rrsequ
goloop ldx  #3
looplp lda acolor,x
 eor colrsh;attract colors that are
 and drkmsk;switched with dli's
 sta acolr1,x
 dex
 bpl looplp
 lda option
 bne pctrig
 lda keydel
 bne ignork
 lda ch;    test for space bar pause
 cmp #$21
 bne ignork
 lda pausef
 beq flipon
 lda #0
 beq stflip
flipon lda #1
stflip sta pausef
noflip lda #$ff
 sta ch
ignork lda gmovrf
 beq gamcns
 lda attimr;if game over, time out
 bne pctrig;and go to option screen
 lda #4
 bne pcext1
pctrig lda #$ff
 sta ch
 lda strig0
 beq cstart
 lda strig1;test trigger start
 beq cstart
gamcns lda consol
 cmp #7;    no buttons pressed
 beq loop
cnsswt sta savcns
 ldx #bdelay
 stx bcount
cloop1 ora consol
 ldx bcount;debounce logic
 bne cloop1
 cmp savcns
 bne gamcns
cloop2 cmp consol
 beq cloop2
 lda savcns
 cmp #6
 beq cstart
 ldx atsequ
 beq cpropt
 lda #0
 sta atsequ
 lda #4
 bne pcext1
cpropt ldx option;if option screen
 cpx #3;         then change params
 bcc pcexit
pressw cmp #3
 beq copton
 cmp #5
 beq cselec
 jmp loop
cstart lda #0
 sta gmovrf;start function-new game
 sta option
 sta atsequ
 lda #1
 sta rstrtf
 jmp reinit
cselec lda numply
 bne pcnse1;process select changes
 lda #1
 bne pcnse2
pcnse1 lda #0
pcnse2 sta numply
pcexit lda #3
pcext1 sta option
 sta atract
 lda #7
 sta atclok
 jmp loop
copton lda difopt
 cmp #2
 bcs pcnop1;process option changes
 inc difopt
 bne pcexit
pcnop1 cmp #$0c
 beq pcnop2
 inc difopt
 inc difopt
 bne pcexit
pcnop2 lda #0
 sta difopt
 beq pcexit
;
; display list interrupt vector
;
dliv pha
 txa
 pha
 ldx dlicnt;dli count
 bne dli2nd
 lda #high pacchr
 sta wsync
 sta chbase
 bne dlixit
dli2nd cpx #1
 bne dli3rd
 lda acolr4;-$da player 1-2 msg
 ldx acolr1;-$3a lt orange color
 sta wsync
 sta colpf1
 stx colpf2
 bne dlixit
dli3rd cpx #2
 bne dli4th
 lda fruclr; fruit color
 eor colrsh
 and drkmsk
 sta wsync
 sta colpf1
 bne dlixit
dli4th cpx #3
 bne dli5th
 lda acolr2
 sta wsync
 sta colpf1
 bne dlixit
dli5th cpx #4
 bne lstdli
 lda acolr3;yellow color
 ldx acolr4
 sta wsync
 stx colpf0
 sta colpf3
 bne dlixit
lstdli ldx #3
 lda #0
dliclp sta colpm0,x
 dex
 bpl dliclp
 sta colpf3
dlixit inc dlicnt
 pla
 tax
 pla
 rti
;
; dli vector for opt & title screens
;
opdliv pha
 txa
 pha
 tya
 pha
 ldx dlicnt
 bne opdli1
 ldy acolr2
 lda acolr1
 ldx #high optchr
 sta wsync
 stx chbase
 sta colpf2
 sty colpf1
 jmp opdlix
opdli1 lda ocindx,x
 sta wsync
 sta chbase
opdlix inc dlicnt
 pla
 tay
 pla
 tax
 pla
 rti
;
; vertical blank interrupt vector
;
vblank lda #0
 sta dlicnt
 dec bcount
 jsr vbsubs
 lda atsequ
 bne vbexit
 lda vfreez
 bne vbexit
 jsr tunnel
vbexit jmp xitvbv; exit vector
;
optttl lda #0
 ldx #7
chrdwr sta hposp0,x
 sta audf1,x
 dex
 bpl chrdwr
 lda #low odlist
 sta sdlstl
 sta dlistl
 lda #high odlist
 sta sdlsth
 sta dlisth
 lda #low opdliv
 sta vdslst
 lda #high opdliv
 sta vdslst+1
ocolrs ldy #3
oclrlp lda ocolor,y
 sta color0,y
 dey
 bpl oclrlp
initad rts
;
set1pl ldx #6
 lda #0
s1pll1 sta text+$20,x
 sta text+$47,x
 sta text+$2a,x
 dex
 bpl s1pll1
setxpl jsr fls1on
 lda #$10
 sta text+$2e
 sta text+$2f
 rts
;
set2pl ldx #6
 lda #0
s2pll1 sta text+$2a,x
 sta text+$47,x
 dex
 bpl s2pll1
 jsr fls2on
 lda #$10
 sta text+$4b
 sta text+$4c
 bne setxpl
;
; include d1:pac1.asm

;
vbsubs lda atsequ
 bne vbattr
 jmp ckopts
vbattr cmp #1
 bne ckats2
 ldx #0
 txa
clraop sta optscn+$50,x
 sta optscn+$150,x
 dex
 bne clraop
 ldx #$0f
bpmlp1 lda bpmsg1,x
 sta optscn+$11a,x
 dex
 bpl bpmlp1
 ldx #$11
bpmlp2 lda bpmsg2,x
 sta optscn+$141,x
 dex
 bpl bpmlp2
 lda #$ff
 sta attimr
 inc atsequ
 rts
ckats2 cmp #2
 bne ckats3
 lda attimr
 beq cas2er
 jmp decatm
cas2er lda #0
 ldx #$11
c2erlp sta optscn+$119,x
 sta optscn+$141,x
 dex
 bpl c2erlp
 lda #$7f
 sta attimr
 inc atsequ
 rts
ckats3 cmp #3
 bne ckats4
 lda attimr
 beq cs3scn
 jmp decatm
cs3scn ldx #0
 txa
clrasc sta patscn,x
 sta patscn+$100,x
 inx
 bne clrasc
 inc atsequ
 rts
ckats4 cmp #4
 bne ckats5
 lda #low adlist
 sta sdlstl
 sta dlistl
 lda #high adlist
 sta sdlsth
 sta dlisth
 lda #low adliv
 sta vdslst
 lda #high adliv
 sta vdslst+1
 ldx #7
setacl lda acolrs,x
 sta pcolr0,x
 dex
 bpl setacl
 lda #$7f
 sta attimr
 inc atsequ
 rts
ckats5 cmp #5
 bne ckats6
 lda attimr
 beq c4nick
 jmp decatm
c4nick ldx #19
astart lda chnktx,x
 sta patscn+$28,x
 dex
 bpl astart
 inc atsequ
 rts
ckats6 cmp #6
 bne ckatsq
 ldx #0
 txa
 jsr initpl
 ldx #$7f
iniap0 sta $80,x
 dex
 bpl iniap0
 jsr atinit
 inc atsequ
 lda #adelay
 sta attimr
 rts
ckatsq lda atsequ
 cmp #$16
 bcc ckatmr
 jmp seqatm
ckatmr lda attimr
 bne decatm
 jsr seqatr
 inc atsequ
 lda #adelay
 sta attimr
 rts
decatm dec attimr
 rts
;
ckopts ldy option
 bne tstopt
 jmp vbgame
tstopt cpy #1
 bne vbopt2
;
; build title screen
;
 jsr optttl
 ldx #0
 txa
opsccl sta optscn,x
 sta optscn+$100,x
 sta pacmaz+$300,x
 dex
 bne opsccl
 sta color1
 ldx #$0d
plogol lda oalogo,x
 sta optscn+$157,x
 dex
 bpl plogol
 inx
pttllp txa
 clc
 adc #$c1
 sta optscn+$18,x
 txa
 adc #$cd
 sta optscn+$2c,x
 inx
 cpx #$0c
 bne pttllp
vbop1i inc option
vbop1x rts
;
; time out title screen
;
vbopt2 cpy #2
 bne vbopt3
 lda atclok
 cmp rtclok+2
 bne vbop1x
 lda #7
 sta atclok
 bne vbop1i
;
; build option screen
;
vbopt3 lda rtclok+2
 bne optscr
 dec atclok
 bne optscr
 lda #1
 sta atsequ
optscr lda #0
 ldx #$11
optcl2 sta optscn+$119,x
 sta optscn+$141,x
 dex
 bpl optcl2
 ldx numply
 inx
 txa
 ora #$50
 sta optscn+$54
 lda numply
 bne pmopt3
 cpy #3
 bne same1p
 jsr set1pl
same1p lda #$12
 bne pmopt4
pmopt3 cpy #3
 bne same2p
 jsr set2pl
same2p lda #$11
pmopt4 sta optscn+$90
 ldx #$0a
pmo4lp lda oplgam,x
 sta optscn+$92,x
 ora #$40
 sta optscn+$56,x
 dex
 bpl pmo4lp
 jsr optttl
 ldx #$0f
pmo5lp lda oprsel,x
 sta optscn+$7a,x
 lda oprop1,x
 sta optscn+$f2,x
 lda oprst1,x
 sta optscn+$156,x
 dex
 bpl pmo5lp
 ldx difopt
 lda optfrt,x
 sta optscn+$c6
 clc
 adc #1
 sta optscn+$c7
 ldx #$10
pmo7lp lda oprop2,x
 sta optscn+$105,x
 dex
 bpl pmo7lp
 ldx #8
pmo9lp lda oprst2,x
 sta optscn+$16d,x
 dex
 bpl pmo9lp
 rts
;
vbgame lda attimr
 beq vbgam1
 lda rtclok+2
 and #7
 bne vbgam1
 dec attimr
vbgam1 lda pausef
 beq vbgam2
 jmp clraud
vbgam2 jsr flshxu
 lda rstrtf
 beq tstgmo
 cmp #2
 beq vtunes
tstgmo lda gmovrf
 beq tstrrk
 lda numply
 beq vfls1o
 jsr fls2on
vfls1o jmp fls1on
tstrrk lda rrflag
 beq tstvrd
 jmp rerack
tstvrd lda readyf
 bne vready
 lda introf
 bne vintro
vsquit rts
vtunes inc introf
 lda #0
 sta atract
 sta rstrtf
 jmp ready1
vintro lda rtclok+2
 and #3
 bne vsquit
vsongs ldx notcnt
 cpx #$40
 beq vstart
 lda hinot1,x
 sta audf1
 lda lonot1,x
 sta audf2
 lda #$a8
 sta audc1
 lda #$a4
 sta audc2
 inc notcnt
 cpx #$28
 bne vsquit
 jmp ready2
vstart inc readyf
vstrt1 inc m1stat
 jsr ready3
vready lda resetf
 beq vcontn
 cmp #1
 bne reset2
 jsr vreset
 lda swappf
 bne vsquit
vrstgm lda gmovrf
 bne vsquit
creset inc resetf
 lda #$40
 sta resett
crestx rts
;
reset2 lda resett
 beq vstrt2
 dec resett
vgone1 rts
vstrt2 lda #0
 sta resetf
 beq vstrt1
vcontn jsr blinkr
 lda vfizst
 beq vplayr
 jsr vfizzl
 jmp fizzie
vplayr jsr eyonly
 lda vfreez
 beq vfruit
 jmp vgulpr
vfruit jsr fruity
 jsr dottst
 lda rrflag
 bne vplyud
vcolls jsr colchk
 lda vfreez
 bne vgone2
 lda vfizst
 beq vplyud
vgone2 rts
vplyud jsr flitec
 lda vfreez
 bne vgone2
 jsr veater
 jsr gobble
 jsr skirts
;
; speed of pacman
;
spdpac lda pacadv
 beq pacreg
 lda #0
 beq spdpc1
pacreg lda pacdly
 bne spdpc1
 ldx #4
 jsr spdseq
spdpc1 sta paccnt
 lda #0
 sta pacadv
 jsr pmstik
 lda #0
 sta pacdly
 jsr munchy
 lda rtclok+2
 and #$03
 bne udmons
 jsr strtmn
udmons jsr chsseq
 lda pacadv
 bne vsubsx
 ldx #3
;
; speed handler for monsters
;
; x reg = 0-3 for monsters 1-4
;
spdmon lda m1stat,x
 and #$7f
 beq nxmspd
 lda rtclok+2
 and #7
 bne spmok1
 lda m1timr,x
 beq spmok1
 dec m1timr,x
spmok1 lda m1stat,x
 and #$3f
 beq nxmspd
spmok2 lda m1stat,x
 asl a
 bmi nxmspd
spmok3 lda m1vpos,x
 cmp #$74
 bne spdblu
 lda m1hpos,x
 cmp #$a7
 bcs spdslo
 cmp #$52
 bcc spdslo
spdblu lda m1stat,x
 bpl spdreg
spdslo jsr spdseq
 cmp #0
 bne nxmspd
 ldy m1dely,x
 cpy #1
 bne mondla
 sta m1dely,x
 beq spdupd
mondla lda #1
 sta m1dely,x
 bne nxmspd
spdreg jsr spdseq
 cmp #0
 bne nxmspd
spdupd jsr monstr
nxmspd dex
 bpl spdmon
vsubsx rts
;
; ***** pacman attract mode *****
;
atinit lda #$11
 sta gprior
 lda #$e0
 sta pmhpos
 lda #$ec
 sta m1hpos
 lda #$f5
 sta m2hpos
 lda #$fe
 sta m3hpos
 lda #$07
 sta m4hpos
 lda #$8c
 sta pmvpos
 sta m1vpos
 sta m2vpos
 sta m3vpos
 sta m4vpos
 lda #$2a
 sta pacclr
 rts
;
;
; sequencinc of attract display
;
seqatr lda atsequ
 cmp #7
 bne atseq8
 lda #$05
 sta patscn+61
 lda #$06
 sta patscn+81
 rts
;
atseq8 cmp #8
 bne atseq9
 ldx #0
 ldy #80
 jmp atshow
atseq9 cmp #9
 bne atseqa
 ldx #10
 ldy #90
 jmp atshow
atseqa cmp #$0a
 bne atseqb
 lda #$45
 sta patscn+101
 lda #$46
 sta patscn+121
 rts
atseqb cmp #$0b
 bne atseqc
 ldx #20
 ldy #120
 jmp atshow
atseqc cmp #$0c
 bne atseqd
 ldx #30
 ldy #130
 jmp atshow
atseqd cmp #$0d
 bne atseqe
 lda #$85
 sta patscn+141
 lda #$86
 sta patscn+161
 rts
atseqe cmp #$0e
 bne atseqf
 ldx #40
 ldy #160
 jmp atshow
atseqf cmp #$0f
 bne atsq10
 ldx #50
 ldy #170
 jmp atshow
atsq10 cmp #$10
 bne atsq11
 lda #$c5
 sta patscn+181
 lda #$c6
 sta patscn+201
 rts
atsq11 cmp #$11
 bne atsq12
 ldx #60
 ldy #200
 jmp atshow
atsq12 cmp #$12
 bne atsq13
 ldx #70
 ldy #210
 jmp atshow
atsq13 cmp #$13
 bne atsq14
 ldx #7
atsqdl lda am10pt,x
 sta patscn+325,x
 lda am50pt,x
 sta patscn+365,x
 dex
 bpl atsqdl
 rts
atsq14 cmp #$14
 bne atseqx
 lda #$84
 sta patscn+261
 ldx #19
acoplp lda copmsg,x
 sta patscn+400,x
 dex
 bpl acoplp
atseqx rts
;
seqatm jsr fls1dt
 lda atsequ
 cmp #$16
 bne atsq17
 lda rtclok+2
 and #$0f
 bne abnkon
 lda #0
 sta patscn+261
 sta patscn+365
 beq chasel
abnkon cmp #8
 bne chasel
 lda #$84
 sta patscn+261
 sta patscn+365
chasel jsr skirts
 lda rtclok+2
 and #1
 bne chasl1
 lda #1
 bne chasl2
chasl1 lda pmhpos
 cmp #$48
 beq chacls
 cmp #$70
 beq chacls
 cmp #$98
 beq chacls
 bne chslft
chacls ldx #3
chsclp dec m1hpos,x
 dex
 bpl chsclp
chslft ldx #3
chsllp lda #4
 jsr monhnd
 dex
 bpl chsllp
 lda #0
chasl2 sta paccnt
 lda pmhpos
 cmp #$38
 beq stoplf
 jsr paclf
 rts
stoplf inc atsequ
 rts
atsq17 cmp #$17
 bne atsq18
 lda #$80
 sta m1stat
 sta m2stat
 sta m3stat
 sta m4stat
 lda #$84
 sta pcolr0
 sta pcolr1
 sta pcolr2
 sta pcolr3
 lda #$14
 sta gprior
 lda #$ff
 sta glpcnt
 lda #0
 sta vfreez
 sta patscn+261
 sta hitclr
 inc atsequ
 lda atsequ
atsq18 cmp #$18
 bne atsq19
 lda vfreez
 bne decfrz
 ldx glpcnt
 cpx #3
 beq atdone
 inx
 jsr chaser
 jsr colchk
 rts
decfrz lda vglpc2
 cmp #$30
 bne dcglpf
 ldx gulped
 lda #0
 sta hposp0,x
 sta vfreez
 lda #$2a
 sta pacclr
 sec
 lda pmhpos
 sbc #4
 sta pmhpos
 rts
dcglpf dec vglpc2
 rts
atdone lda #0
 sta atsequ
 lda #7
 sta atclok
atsq19 rts
 ; 
chaser jsr skirts
 lda rtclok+2
 and #3
 bne chasrp
chsrml lda #8
 jsr monhnd
 inx
 cpx #4
 bne chsrml
chasrp lda rtclok+2
 and #1
 bne chasr1
 lda #1
 bne chasr2
chasr1 lda #0
chasr2 sta paccnt
 jsr pacrt
 rts

;
atshow lda #9
 sta temloc
atshwl lda chartx,x
 sta patscn,y
 inx
 iny
 dec temloc
 bpl atshwl
 rts
;
fls1dt lda rtclok+2
 and #$0f
 bne fls1do
 lda #0
 sta patscn+365
 rts
fls1do cmp #8
 bne fls1dx
 lda #$84
 sta patscn+365
fls1dx rts
;
; attract mode dli routine
;
adliv pha
 txa
 pha
 tya
 pha
 ldx dlicnt
 bne n1adli
 lda #high amcset
 sta wsync
 sta chbase
 bne adlixx
n1adli cpx #1
 bne n2adli
 lda #$4a
 eor colrsh
 and drkmsk
 tax
 lda #$d8
 eor colrsh
 and drkmsk
 sta wsync
 stx colpf1
 sta colpf2
 bne adlixx
n2adli cpx #2
 bne n3adli
 lda pacclr
 eor colrsh
 and drkmsk
 ldx acolr1; $3a
 sta wsync
 sta colpf3
 stx colpf2
 bne adlixx
n3adli cpx #3
 bne n4adli
 lda #$0c
 eor colrsh
 and drkmsk
 ldx acolr2; $44
 sta wsync
 sta colpf0
 stx colpf1
 bne adlixx
n4adli lda #high pacchr
 ldx acolr4; $da
 ldy acolr3; $2a
 sta wsync
 sta chbase
 stx colpf0
 sty colpf3
adlixx inc dlicnt
 pla
 tay
 pla
 tax
 pla
 rti
;
; collision check
;
colchk ldx #0
 ldy #1
colllp lda m1stat,x
 asl a
 bmi nxcoll
 tya
 bit m1pl
 beq nxcoll
 bit m2pl
 beq nxcoll
 lda pmvpos
 cmp m1vpos,x
sbcmxv bcs pmblmx
 sec
 lda m1vpos,x
 sbc pmvpos
 bne tstmxc
pmblmx sbc m1vpos,x
tstmxc cmp #5
 bcc strcol
nxcoll tya
 asl a
 tay
 inx
 cpx #4
 bne colllp
colcx1 jmp colckx
strcol lda m1stat,x
 bmi zapgst
 jmp pmdead
zapgst lda #$42
 sta m1stat,x
 stx gulped
 inc glpcnt
 sec
 lda pmhpos
 sbc #4
 sta hposm3
 clc
 ldy #2
reposl adc #2
 sta hposm0,y
 dey
 bpl reposl
 adc #2
 sta hposp0,x
 lda #$dc
 sta pacclr
 sta pcolr0,x
 ldy atsequ
 bne ckoffs
 sta color3
ckoffs lda pmvpos
;
 cmp m1vpos,x
 beq nooffs
 bcc offdwn
 lda #$fe
 bmi stroff
offdwn lda #2
stroff clc
 adc pmvpos
;
nooffs sta scnsc1
 sta scnsc2
 lda #high pmaddr+3
 sta scnsc1+1
 txa
 clc
 adc scnsc1+1
 adc #1
 sta scnsc2+1
 lda glpcnt
 asl a
 tay
 lda blsadd,y
 sta pixget
 iny
 lda blsadd,y
 sta pixget+1
 ldy #$0f
stbscl lda (pixget),y
 sta (scnsc1),y
 lda blusc0,y
 sta (scnsc2),y
 dey
 bpl stbscl
 lda #0
 sta audc2
 inc vfreez
 lda #$84
 sta vglpc2
 lda #2
 sta vglpc1
 lda #$14
 sta gprior
 lda atsequ
 beq ckgps0
 jmp clrhit
ckgps0 lda glpcnt
 bne ckgps1
 lda #2
 bne udgpsc
ckgps1 cmp #1
 bne ckgps2
 lda #4
 bne udgpsc
ckgps2 cmp #2
 bne ckgps3
 lda #8
 bne udgpsc
ckgps3 lda #1
 sta scorex+2
 lda #6
udgpsc sta scorex+3
 jmp pscore
pmdead lda pmstat
 ora #$80
 sta pmstat
 jsr clraud
 lda #1
 sta mstill
 sta vfizst
 lda #$60
 sta vfizsq
colckx lda #2
 bit m2pf
 beq clrhit
 ldx plynum
 lda mazct1,x
 cmp #$0c
 bcc lowfsi
 lda #$0c
lowfsi tax
 lda fsindx,x
 tax
 ldy #0
fsloop lda fs0100,x
 sta pacmaz+$1f1,y
 inx
 iny
 cpy #5
 bne fsloop
 lda #1
 sta frscrf
 lda #$40
 sta frscrt
 lda #0
 sta fruflg
 sta frutmr
 sta frutmr+1
 lda #1
 sta gobbld
 lda #$78
 sta gobblf
 ldx plynum
 lda mazct1,x
 cmp #$0c
 bcc lowfrs
 lda #$0c
lowfrs asl a
 tax
 lda frstab,x
 sta scorex+2
 inx
 lda frstab,x
 sta scorex+3
 jsr pscore
clrhit sta hitclr
 rts
;
vreset jsr blnkon
 lda swappf
 beq vrset1
 ldx resett
 beq vswap1
 dec resett
 rts
vswap1 cmp #1
 bne vswap2
;
 ldx #0
lodp1l lda p1save,x
 sta pacmaz,x
 lda p1save+$100,x
 sta pacmaz+$100,x
 lda p1save+$200,x
 sta pacmaz+$200,x
 lda p1save+$300,x
 sta pacmaz+$300,x
 inx
 bne lodp1l
;
 jsr fls2on
 lda #0
 sta plynum
 beq vswapx
vswap2 cmp #2
 bne vswap3
;
 ldx #0
lodp2l lda p2save,x
 sta pacmaz,x
 lda p2save+$100,x
 sta pacmaz+$100,x
 lda p2save+$200,x
 sta pacmaz+$200,x
 lda p2save+$300,x
 sta pacmaz+$300,x
 inx
 bne lodp2l
;
 jsr fls1on
 lda #1
 sta plynum
 bne vswapx
vswap3 cmp #3
 bne vswap4
 jsr blnkon
 jmp rst1pg
vswapx lda #3
vswpx1 sta swappf
 rts
vrset1 lda numply
 beq rst1pg
rst2pg lda plynum
 bne rst2p2
rst2p1 lda xpacp2
 beq rst1pg;player 2 dead
 lda xpacp1
 bne swap12;go swap players
 lda #2
 sta swappf;show #1 game over
 lda #$60
 sta resett
 bne vgmend
swap12 jsr savep1
 lda #$40
 sta resett
 lda #2
 bne vswpx1
rst2p2 lda xpacp1
 beq rst1pg;player 1 dead
 lda xpacp2
 bne swap21;go swap players
 lda #1
 sta swappf;show #2 game over
 lda #$60
 sta resett
 bne vgmend
swap21 jsr savep2
 lda #$40
 sta resett
 lda #1
 sta swappf
vggone rts
rst1pg ldx plynum
 lda xpacp1,x
 beq vgmend; game is over
 jsr setup
 jsr ready1
 lda numply
 beq vswap4
 lda #$40
 sta resett
 lda #4
 sta swappf
 rts
vswap4 lda #0
 sta swappf
 jmp ready2
vgmend lda #$ac
 ldx #0
gmovlp sta pacmaz+$1ed,x
 clc
 adc #1
 inx
 cpx #$0e
 bne gmovlp
 lda #$44
 sta color3
 sta colpf3
 jsr playrs
 lda swappf
 bne vggone
theend ldx #$2a
 jsr ckhigh
 ldx #$47
 jsr ckhigh
 lda #0
 sta resetf
 lda #1
 sta gmovrf
 lda #$e2
 sta attimr
 jsr fls1on
 jmp blnkon
;
ckhigh ldy #0
ckhilp lda text,x
 cmp text+$39,y
 beq chinxt
 bcc ckhigx
 bcs storhi
chinxt inx
 iny
 cpy #6
 bne ckhilp
 rts
storhi lda text,x
 sta text+$39,y
 inx
 iny
 cpy #6
 bne storhi
ckhigx rts
;
; tunnel logic
;
tunnel ldx #4
ctmskl lda m1vpos,x
 cmp #$74
 bne nxtmsk
 jsr msktun
nxtmsk dex
 bpl ctmskl
 rts
;
msktun sta tunloc
 txa
 cmp #4
 bne tunply
 lda #$ff
tunply clc
 adc #high pmaddr+4
 sta tunloc+1
 lda #$ff
 sta tunmsk
 lda m1hpos,x
 sta tuncnt
 cmp #$c0
 bcc tnmsk2
 lda #$c0
tnmsl1 cmp tuncnt
 beq sttmsk
 dec tuncnt
 asl tunmsk
 bcs tnmsl1
 lda m1hpos,x
 cmp #$ca
 bcc sttmsk
 lda m1dirt,x
 cmp #8
 bne sttmsk
 lda #$2a
 sta m1hpos,x
 cpx #4
 bne sttmsk
 lda #$8f
 bne tfxpac
tnmsk2 cmp #$39
 bcs tnmskx
tnmsl2 lda #$38
 cmp tuncnt
 beq sttmsk
 inc tuncnt
 lsr tunmsk
 bcs tnmsl2
 lda m1hpos,x
 cmp #$2a
 bne sttmsk
 lda m1dirt,x
 cmp #4
 bne sttmsk
 lda #$ca
 sta m1hpos,x
 cpx #4
 bne sttmsk
 lda #$b7
tfxpac sta pacscn
sttmsk ldy #$0f
sttmsl lda (tunloc),y
 and tunmsk
 sta (tunloc),y
 dey
 bpl sttmsl
tnmskx rts
;

; eject
; include d1:pac2.asm

;
; pac-man game subroutines
;
gobble ldx gobbld
 beq gobblx
 lda gobblf
 ldy #$a5
 cpx #7
 bcs gobbd2
 cmp #$b4
 beq samgob
 clc
 adc #5
 bne strgob
samgob inc gobbld
 ldy #$a3
 bne strgob
gobbd2 cpx #9
 bne gobbd3
 inc gobbld
 bne decgob
gobbd3 ldy #$a3
decgob sec
 cmp #$78
 bne gobdec
 lda #0
 sta gobbld
 tay
 beq strgob
gobdec sbc #5
strgob sta gobblf
 sta $d200
 sty $d201
gobblx rts
;
skirts lda rtclok+2
 and #$0f
 bne skirtx
 lda mskirt
 beq iskirt
 lda #0
 sta mskirt
 beq skirtx
iskirt inc mskirt
skirtx rts
;
; rerack will reset maze after
; a player has cleared all dots
;
rerack lda rrsequ
 bne tstrrs
 jsr clraud
 jsr drawit
 lda #$40
rrkxx1 sta rrtimr
rrkxx2 inc rrsequ
 rts
tstrrs cmp #1
 bne tstrr2
 lda rrtimr
 bne drrwtm; dec rerack wait timer
 lda #0
 ldx #3
tsrr1l sta hposp0,x
 dex
 bpl tsrr1l
 lda #$0c
 sta color0
 lda #$07
 sta rrflct; rerack flash count
 lda #$10
 bne rrkxx1
drrwtm dec rrtimr
 rts
tstrr2 cmp #2
 bne tstrr3
 lda rrtimr
 bne drrwtm; dec rerack color timer
 dec rrflct
 beq rrkxx2
altclr lda rrflct
 clc
 lsr a
 bcc altblu
 lda #$0c
 bne setrrc
altblu lda #$86
setrrc sta color0
 lda #$10
 sta rrtimr
 rts
tstrr3 cmp #3
 bne tstrr4
 lda #0
 sta intcnt
 lda #chrorg/256
 sta chbase
tstr3x rts
tstrr4 cmp #4
 bne tstrr5
 jsr newbrd
 ldx plynum
 inc xpacp1,x
 inc mazct1,x
 jsr ready1
 inc rrsequ
 lda numply
 beq tstr3x
 lda #$30
 sta rrtimr
 rts
tstrr5 cmp #5
 bne tstrr6
 lda rrtimr
 bne dtmrr5
 inc rrsequ
 rts
dtmrr5 dec rrtimr
 rts
tstrr6 jsr ready2
 lda #0
 sta rrflag
 sta rrsequ
 lda #2
 sta resetf
 lda #$40
 sta resett
 rts
;
; get ready to play
;
ready1 lda #$a2
 ldx #0
redylp sta pacmaz+$1ef,x
 clc
 adc #1
 inx
 cpx #$0a
 bne redylp
 jsr playrs
 ldx plynum
 lda mazct1,x
 cmp #6
 bcc setred
 cmp #$0a
 bcs setred
 ldy #$da; set up for green fruits
 bne setfrc
setred ldy #$44
setfrc sty fruclr
 ldy #0
 cmp #6
 bcs hfruts
 sta temloc
 lda #$92
 sta pixput
 lda #high pacmaz+3
 sta pixput+1
 ldx #0
frutlp lda fruchr,x
 sta (pixput),y
 inc pixput
 clc
 adc #1
 sta (pixput),y
 cpx temloc
 beq fsplit
fruts1 inx
 dec pixput
 dec pixput
 dec pixput
 bne frutlp
hfruts cmp #18
 bcc hfrut1
 lda #18
hfrut1 sec
 sbc #6
 sta temloc
 sec
 lda #low hifrut
 sbc temloc
 sta pixget
 lda #high hifrut
 sbc #0
 sta pixget+1
 ldx #0
hfrutl lda (pixget),y
 sta pacmaz+$386,x
 clc
 adc #1
 sta pacmaz+$387,x
 inx
 inx
 iny
 cpy #$7
 bne hfrutl
fsplit rts
;
ready2 ldx #$0b
 lda #0
red2lp sta pacmaz+$14e,x
 dex
 bpl red2lp
 jsr drawit
 ldx plynum
 dec xpacp1,x
;
udxpacs ldx plynum
 lda xpacp1,x
 ldx #0
 ldy #$9b
 cmp #3
 bne twopac
 sty pacmaz+$378
udxpc2 sty pacmaz+$376
udxpc1 sty pacmaz+$374
 rts
twopac cmp #2
 bne onepac
 jsr udnpc2
 jmp udxpc2
onepac cmp #1
 bne nopacs
 jsr udnpc1
 jmp udxpc1
nopacs stx pacmaz+$374
udnpc1 stx pacmaz+$376
udnpc2 stx pacmaz+$378
 rts
;
drawit lda #1
 sta mstill
 ldx #3
greadl lda m1dirt,x
 jsr monhnd
 dex
 bpl greadl
 lda #0
 sta mstill
 jmp pacstp
;
ready3 ldx #$0d
 lda #0
redy3l sta pacmaz+$14d,x
 sta pacmaz+$1ed,x
 dex
 bpl redy3l
 rts
;
flshxu lda rtclok+2
 and #$0f
 bne flshxx
 lda flshup
 bne flsrst
 inc flshup
 bne flsher
flsrst lda #0
 sta flshup
flsher lda plynum
 beq flspl1
 lda flshup
 bne fls2on
 tax
 tay
 beq f2stor
fls2on lda #$12
 ldx #$35
 ldy #$30
f2stor sta text+$21
 stx text+$22
 sty text+$23
 rts
flspl1 lda flshup
 bne fls1on
 tax
 tay
 beq f1stor
fls1on lda #$11
 ldx #$35
 ldy #$30
f1stor sta text+4
 stx text+5
 sty text+6
flshxx rts
;
; set up monster and pacman
; start positions
;
setup jsr initpm;clear p/m graphics
setup1 ldx #$7f
clrpgz sta $80,x
 dex
 bpl clrpgz
 jsr setclr;init color regs
;
; speed initialization
;
spdini ldx plynum
 lda mazct1,x
 cmp #6
 bcc lowini
 lda #6
lowini tay
 lda pacspd,y
 tax
 lda speed1,x
 sta pmspct
 lda monspd,y
 tay
 ldx #3
spinil lda speed1,y
 sta m1spct,x
 dex
 bpl spinil
;
 ldx #$13
indatl lda inidat,x
 sta pacscn,x
 dex
 bpl indatl
 ldy #0
 jsr mstimr
 sta hitclr
 rts
newgam lda #3
 sta xpacp1;do at game start
 sta xpacp2
 lda difopt
 sta mazct1
 sta mazct2
 ldx #1
 jsr newbd1
newbrd jsr p1init; do at screen start
 jsr udxpac
 ldx plynum
 lda mazct1,x
 tay
 bne newrk2
 lda rtclok+2
 bpl newrk2
newrk1 jsr mstimr
 jmp newbd0
newrk2 iny
 bne newrk1
newbd0 ldx plynum
newbd1 lda #$0f
 sta bigdt1,x
 lda #0
 sta frutp1,x
 sta dtctl1,x
 sta dtctm1,x
 rts
;
mstimr cpy #3
 bcc ldmstm
 ldy #3
ldmstm ldx #2
ldmstl lda startv,y
 sta m2timr,x
 iny
 dex
 bpl ldmstl
 rts
;
; vfizzl is the fold-up
; sequence for the pacman
;
; vfizst is the status
;
; 0 = no action
; 1 = wiggle skirts
; 2 = clear monsters & init sounds
; 3 = sound freq increasing
; 4 = sound freq decreasing
; 5 = fade out sound
; 6 = show blank screen
;
vfizzl lda vfizst
 cmp #1
 bne iniclr
 lda vfizsq
 beq nxtfsq
 jsr skirts
 ldx #3
udmflp lda m1dirt,x
 jsr monhnd
 dex
 bpl udmflp
 dec vfizsq
 rts
nxtfsq inc vfizst
 lda vfizst
iniclr cmp #2
 bne fizchk
 ldx #3
 lda #0
monclr sta hposp0,x
 dex
 bpl monclr
 lda #$35
 sta vfizfq
 sta vfizbs
 jsr clraud
 sta vfizct
 sta fizptr
 inc fizzle
 lda #$07
 sta fiztim
 lda #3
 sta vfizst
 bne vfizup
fizchk cmp #3
 beq vfizup
 cmp #4
 beq vfizdn
 cmp #5
 beq vfizfz
 cmp #6
 beq fzwait
vfizup lda #$a8
 sta audc1
 lda vfizfq
 sta audf1
 clc
 adc #8
 sta vfizfq
 inc vfizct
 lda vfizct
 cmp #4
 bne vbfizx
 beq svfizs
vfizdn lda #$a8
 sta audc1
 lda vfizfq
 sta audf1
 sec
 sbc #8
 sta vfizfq
 dec vfizct
 bne vbfizx
 lda vfizbs
 cmp #$6d
 beq vbfizi
vfizsw clc
 adc #4
 sta vfizbs
 sta vfizfq
 lda #3
svfizs sta vfizst
 rts
vfizfz lda #$a8
 sta audc1
 lda vfizbs
 sta audf1
 sec
 sbc #8
 sta vfizbs
 cmp #$25
 bne vbfizx
 jsr clraud
 lda #$80
 sta vfizct
vbfizi inc vfizst
vbfizx rts
;
fzwait lda vfizct
 bne decfcw
 inc resetf
 rts
decfcw dec vfizct
 rts
;
; fizzie will draw the pacman
; folding up in sequence
;
fizzie lda fizzle
 beq fizzix
 lda pmvpos
 sta pixput
 lda #high pmaddr+3
 sta pixput+1
 lda fiztim
 beq rstfiz
 dec fiztim
 bpl strfiz
rstfiz lda fizptr
 cmp #$0f
 beq clrfiz
 inc fizptr
 lda #$07
 sta fiztim
strfiz lda fizptr
 beq fizstr
 cmp #$0f
 beq exppac
 tax
 dex
 lda fizidx,x
 tay
 dey
 lda fizdat,x
 sta (pixput),y
 rts
fizstr ldy #$0c
 ldx #9
fizstl lda pacdie,x
 sta (pixput),y
 dey
 dex
 bpl fizstl
 rts
exppac ldy #$0f
 ldx #$0f
exppcl lda pacexp,x
 sta (pixput),y
 dey
 dex
 bpl exppcl
 rts
clrfiz ldy #$0f
 lda #0
clrfzl sta (pixput),y
 dey
 bpl clrfzl
 sta fizzle
fizzix rts
;
flitec lda flashc
 beq noflit
 lda tweetr
 bne ckfltm
 jsr vrverb
ckfltm lda flitmr
 beq flshsq
 dec flitmr
 jmp setflc
noflit lda tweetr
 bne fizzix
 jmp vchase
flshsq ldx plynum
 lda mazct1,x
 tax
 lda flstim,x
 cmp flashc
 bne nxtfls
 ldx #3
rscllp lda m1stat,x
 bpl nxtrsc
 lda colors,x
 sta pcolr0,x
nxtrsc dex
 bpl rscllp
 ldx #3
rstchl lda m1stat,x
 bpl nxrstc
 cmp #$80
 beq rstbox
 and #$bb
 beq nxrstc
 bne rstchs
rstbox lda #0
 beq rsstat
rstchs lda #$20
rsstat sta m1stat,x
nxrstc dex
 bpl rstchl
 lda #0
 sta flashc
 sta flasht
 sta vflits
 lda #$a0
 sta chaset
 rts
nxtfls lda flasht
 bne decftm
 inc flashc
 lda #$18
 sta flasht
decftm dec flasht
setflc lda flashc
 lsr a; alternate blue & white
 bcc fwhite
 ldy #$84; dark blue
 bne fstore
fwhite ldy #$0c; white
fstore ldx #3
fstorl lda m1stat,x
 bpl nxfstr
 tya
 sta pcolr0,x
nxfstr dex
 bpl fstorl
flitcx rts
;
;
vrverb lda vflits
 bne vflrdy
 lda #5
 sta vflitv
 lda #2
 sta vflitd
 inc vflits
 lda #$90
 sta vflitf
vflrdy lda vflitd
 cmp #1
 bne vflidn
 lda vflitf
 cmp #$90
 bne vfupok
 lda vflitv
 cmp #3
 bne vfvdec
 lda #5
 sta vflitv
vfvdec dec vflitv
 lda #2
 sta vflitd
 bne vfdnok
vfupok clc
 lda vflitf
 adc #$10
 bne vrbstr
vflidn lda vflitf
 cmp #$40
 bne vfdnok
 lda #1
 sta vflitd
 bne vfupok
vfdnok sec
 lda vflitf
 sbc #$10
vrbstr sta vflitf
 sta audf2
 lda vflitv
 ora #$a0
vrvbx1 sta audc2
vrvrbx rts
;
vtweet lda tweetf
 bne ctweet
 lda #$0c
 sta tweetf
 bne itweet
ctweet cmp #$17
 bne itweet
 lda #$0c
 sta tweetf
itweet inc tweetf
stweet lda tweetf
 sta audf2
 lda #$a4
 bne vrvbx1
;
vgulpr dec vglpc1
 beq distrt
 sec
 lda vglpc2
 sbc #4
 sta vglpc2
 cmp #$10
 beq glpoff
 sta audf1
 sta audf4
 lda #$84
 sta audctl
 lda #$a4
 bne gbrnch
distrt lda #2
 sta vglpc1
 sec
 lda vglpc2
 sbc #3
 sta vglpc2
 sta audf1
 sta audf3
 sta audf4
 lda #$a8
 sta audc3
gbrnch sta audc4
 bne veatrs
glpoff jsr clraud
 sta audctl
 sta vfreez
 sta paccnt
 sty gulped
 lda #$2a
 sta color3
 lda #$11
 sta gprior
 ldx #3
rstpcl lda pcolr0,x
 cmp #$dc
 beq rstplc
 dex 
 bpl rstpcl
 rts
rstplc lda #$0c
 sta pcolr0,x
 sta hitclr
 lda #0
 sta paccnt
 jsr pmstik
 inc pacadv
 jmp munchy
;
veater lda eaterf
 beq veatrx
 ldx eaterc
 cpx #6
 bne ceater
 lda #0
 sta eaterf
 sta eaterc
 beq veatrs
ceater lda eatert
 bne eater2
 lda e1data,x
 jmp streat
eater2 lda e2data,x
streat inc eaterc
 sta audf1
 lda #$a4
veatrs sta audc1
veatrx rts
;
clraud ldy #7
 lda #0
clraul sta audf1,y
 dey
 bpl clraul
clraux rts
;
playrs lda numply
 beq clraux
 lda #$65
 ldx #0
plyrlp sta pacmaz+$14e,x
 clc
 adc #1
 inx
 cpx #$0c
 bne plyrlp
 lda plynum
 beq clraux
 ldx #$71
 stx pacmaz+$158
 inx
 stx pacmaz+$159
 rts
;
apgst lda #$42
 sta m1stat,x
 stx gulped

; eject

; include d1:pac3.asm

;
; pac-man game subroutines
;
; initialize player/missile memory
;
; this subr clears p/m area
;
initpm ldx #0
 txa
 jsr initpl
 lda #$fe
 ldx #$f0
 jsr initpl
 txa
 rts
initpl sta pmaddr+$300,x
 sta pmaddr+$400,x
 sta pmaddr+$500,x
 sta pmaddr+$600,x
 sta pmaddr+$700,x
 inx
 bne initpl
 rts
;
; setcolor subroutine will
; initialize $2c0 - $2c7
; (color registers)
;
setclr ldx #7
setcll lda colors,x
 sta pcolr0,x
 dex
 bpl setcll
 rts
;
;
p1init ldx #0
p1inil lda datmaz,x
 sta pacmaz,x
 lda datmaz+$100,x
 sta pacmaz+$100,x
 lda datmaz+$200,x
 sta pacmaz+$200,x
 lda datmaz+$300,x
 sta pacmaz+$300,x
 inx
 bne p1inil
 rts
;
savep1 ldx #0
savp1l lda pacmaz,x
 sta p1save,x
 lda pacmaz+$100,x
 sta p1save+$100,x
 lda pacmaz+$200,x
 sta p1save+$200,x
 lda pacmaz+$300,x
 sta p1save+$300,x
 inx
 bne savp1l
 rts
;
savep2 ldx #0
savp2l lda pacmaz,x
 sta p2save,x
 lda pacmaz+$100,x
 sta p2save+$100,x
 lda pacmaz+$200,x
 sta p2save+$200,x
 lda pacmaz+$300,x
 sta p2save+$300,x
 inx
 bne savp2l
sav2px rts
;
; pmstik subroutine will test
; joytick input and determine
; if direction chosen is valid.
;   the pacman will then move
; in the proper direction with
; it's mouth opening and closing.
;
; this code is called during
; vblank and initiates motion
; during alternate occurances
; of vblank.  mouth animation
; is performed every vblank.
;
;
pmstik lda pmstat
 bmi sav2px
pudstk ldx #4
 jsr mazhnd
 clc
 lda pmndir;see if we change dir.
 bit temloc;is it valid ?
 beq pmudst;no
 cmp pmodir
 beq pudsam
 ora pmodir
 tay
 and #3
 beq pudsam
 tya
 and #$0c
 beq pudsam
 sty pacadv
pudsam lda pmndir
 sta pmodir
pmudst ldx plynum
 lda stick0,x
 eor #$0f
 beq pmsame
 pha
 ldx #3
 ldy #0
pmtst2 clc
 lsr a
 bcc nxpts2
 iny
nxpts2 dex
 bpl pmtst2
 pla
 cpy #2
 beq pmsame
pdrtst sta atract
 sta pmndir
 bit temloc
 beq pmsame
 sta pmodir
pmsame lda pmodir
 bit temloc
 bne pactst
 jmp pacopn
pactst cmp #1
 beq pacup
 cmp #2
 beq pacdn
 cmp #4
 bne pacrtv
 jmp paclf
pacrtv cmp #8
 beq pacrt
 jmp pacstp
pacup lda paccnt
 bne pacups
 dec pmvpos
 dec pmvpos
 lda pacmap
 bne decpmp
 lda #3
 sta pacmap
 bne pacups
decpmp dec pacmap
 bne pacups
 lda pmvpos
 cmp pvsave
 beq pacups
 sec
 lda pacscn
 sbc #$28
 sta pacscn
 lda pacscn+1
 sbc #0
 sta pacscn+1
pacups ldy #6;point to pactop
 jmp movpac
pacdn lda paccnt
 bne pacdns
 inc pmvpos
 inc pmvpos
 lda pacmap
 cmp #3
 bne incpmp
 lda #0
 sta pacmap
 lda pmvpos
 cmp pvsave
 beq pacdns
 clc
 lda pacscn
 adc #$28
 sta pacscn
 lda pacscn+1
 adc #0
 sta pacscn+1
 bne pacdns
incpmp inc pacmap
pacdns ldy #8;point to pacbot
 jmp movpac
pacrt lda paccnt
 bne pacrts
;
 lda pmhpos
 cmp #$ca
 bne norttn
 lda #$2a
 sta pmhpos
 lda #$8f
 sta pacscn
;
norttn inc pmhpos
 lda pacbyt
 cmp #3
 bne incpby
 lda #0
 sta pacbyt
 lda pmhpos
 cmp phsave
 beq pacrts
 inc pacscn
 bne pacrts
 inc pacscn+1
 bne pacrts
incpby inc pacbyt
pacrts ldy #2;point to pacrgt
 bne movpac
paclf lda paccnt
 bne paclfs
;
 lda pmhpos
 cmp #$2a
 bne nolftn
 lda #$ca
 sta pmhpos
 lda #$b7
 sta pacscn
;
nolftn dec pmhpos
 lda pacbyt
 bne decpby
 lda #3
 sta pacbyt
 bne paclfs
decpby dec pacbyt
 bne paclfs
 lda pmhpos
 cmp phsave
 beq paclfs
 sec
 lda pacscn
 sbc #1
 sta pacscn
 lda pacscn+1
 sbc #0
 sta pacscn+1
paclfs ldy #4;point to paclft
 bne movpac
pacopn lda pmodir
 cmp #1
 bne popndn
 ldy #6; up
 bne setopn
popndn cmp #2
 bne popnlf
 ldy #8; down
 bne setopn
popnlf cmp #4
 bne popnrt
 ldy #4; left
 bne setopn
popnrt cmp #8
 bne pacstp
 ldy #2
setopn lda #$0a
 bne strpac
pacstp ldy #0;point to pacdot
 tya
 beq strpac
movpac ldx pmsequ
 bne pacsid
 inc pmsequ
 bne pacstp
pacsid dex
 lda pacidx,x
 cpx #2
 bne incpsq
 ldx #$ff
 stx pmsequ
incpsq inc pmsequ
strpac tax
 lda pacadd,y
 sta pixget
 iny
 lda pacadd,y
 sta pixget+1
 txa
 clc
 adc pixget
 sta pixget
 lda #0
 adc pixget+1
 sta pixget+1
 ldy #9
pmbflp lda (pixget),y
 sta pacbuf+3,y
 dey
 bpl pmbflp
 lda #low pacbuf
 sta pixget
 lda #high pacbuf
 sta pixget+1
 lda pmvpos
 sta pixput
 lda #high pmaddr+3
 sta pixput+1
 clc
 lda pmhpos
 sta hposm3
 adc #2
 sta hposm2
 adc #2
 sta hposm1
 adc #2
 sta hposm0
 ldy #$0f
pploop lda (pixget),y
 sta (pixput),y
 dey
 bpl pploop
pmsixx rts
;
; munchy subroutine eats dots
;
munchy lda pacbyt
 bne munchx
 lda pacmap
 bne munchx
 lda pmhpos
 cmp phsave
 bne nwpref
 lda pmvpos
 cmp pvsave
 beq pmsixx
nwpref lda pmvpos
 sta pvsave
 lda pmhpos
 sta phsave
 ldy #0
 lda (pacscn),y
 cmp #1
 beq eatsml
 cmp #2
 bne munchx
 tya
 sta (pacscn),y
 rts
eatsml sta scorex+4
 jsr pscore
 lda #1
 sta eaterf
 sta pacdly
 lda #0
 sta eaterc
 lda eatert
 bne zeater
 lda #1
 bne seater
zeater lda #0
seater sta eatert
 lda #0
 tay
 sta (pacscn),y
incdot ldx plynum
 inc dtctl1,x
 bne chkmax
 inc dtctm1,x
chkmax ldx plynum
 lda dtctm1,x
 beq munchx
 lda dtctl1,x
 cmp #4
 bne munchx
 lda #1
 sta rrflag
munchx rts
;
dottst ldx plynum
 lda bigdt1,x
 sta temloc
 ldx pmvpos
 ldy pmhpos
 lda #1
 bit temloc
 beq dt2tst
 cpx #$3c
 bne dt2tst
 cpy #$3a
 beq dotfnd
dt2tst asl a
 bit temloc
 beq dt3tst
 cpx #$3c
 bne dt3tst
 cpy #$be
 beq dotfnd
dt3tst asl a
 bit temloc
 beq dt4tst
 cpx #$a4
 bne dt4tst
 cpy #$3a
 beq dotfnd
dt4tst asl a
 bit temloc
 beq munchx
 cpx #$a4
 bne munchx
 cpy #$be
 bne munchx
dotfnd eor #$0f
 and temloc
 ldx plynum
 sta bigdt1,x
 lda #5
 sta scorex+4
 jsr pscore
 lda #1; set up for blue monsters
 sta flashc
 lda #$ff
 sta glpcnt
 ldx plynum
 lda mazct1,x
 tax
 lda blutim,x
 sta flitmr
 ldx #3
setfll lda m1stat,x
 asl a
 bmi nxtfll
 lda m1stat,x
 ora #$80; set status = flight
 sta m1stat,x
 lsr a
 lsr a
 lsr a
 bcs nxtfll
;
 and #$3b
 beq nxtfll
 lda m1dirt,x
 tay
 lda blurev,y
 sta m1dirt,x
;
nxtfll dex
 bpl setfll
 jmp incdot
;
; pscore will add any points
; scored to the players' score
;
pscore lda #0;pscore subr
 sta carryb
 sed
 lda plynum
 beq pscor1
 ldx #$4c
 bne pscorx
pscor1 ldx #$2f
pscorx ldy #5
kscore clc
 lda text,x
 and #$0f
 adc carryb
 adc scorex,y
 pha
 and #$10
 beq nocary
 lda #1
nocary sta carryb
 pla
 ora #$10
 cmp #$10
 bne strscr
 cpy #0
 bne tesled
 tya
 beq strscr
tesled lda text-1,x
 bne nolead
 lda carryb
 beq strscr
nolead lda #$10
strscr sta text,x
 dex
 dey
 bpl kscore
 cld
 ldx #5
 lda #0
clrscr sta scorex,x
 dex
 bpl clrscr
ckbons ldx plynum
 lda bpacp1,x
 bne nobons
 cpx #0
 bne ckbon2
 lda text+$2b
 bne bonusp
nobons rts
ckbon2 lda text+$48
 beq nobons
bonusp inc bpacp1,x
 inc xpacp1,x
 jmp udxpacs
;
; maze handler subroutine
;
; entry:
; a reg value equals vpos
; y reg value equals hpos
;
; exit:
; a reg value equals permissible
; directions for any object from
; any position
;
; bit 0 set - up ok
; bit 1 set - dn ok
; bit 2 set - rt ok
; bit 3 set - lf ok
;
; carry bit is set if
; decision point was reached
; otherwise it is cleared.
;
mazhnd lda m1hpos,x
 sta hsaver
 lda m1vpos,x
 stx temloc
 ldx #9
mhorlp cmp vtable,x;search vpos
 beq vrtfnd;match found
 dex
 bpl mhorlp;keep looking
 lda hsaver;none found so try hpos
 ldy #9
mvrtlp cmp htable,y;search hpos
 beq horfnd;match found
 dey
 bpl mvrtlp
vrtfnd ldy #9;now we check hpos table
 lda hsaver;  to see if decision pt.
vrtfnl cmp htable,y
 beq choice;yes - make choice
 dey
 bpl vrtfnl
 lda #$0c;no - keep going
 clc
 bcc mazhnx
horfnd lda #3; only one match found
 clc
 bcc mazhnx
choice txa;now index into table
 asl a
 tax
 lda htbadd,x
 sta pixget
 inx
 lda htbadd,x
 sta pixget+1
 lda (pixget),y
 sec
mazhnx ldx temloc
 sta temloc
 php
 cpx #4
 beq mazhx1
 lda m1dirt,x
 tay
 lda temloc
 and revtab,y
 sta temloc
 lda m1vpos,x
 cmp #$64
 bne mazhx1
 lda m1hpos,x
 cmp #$76
 beq maskit
nextmu cmp #$82
 bne mazhx1
maskit lda m1stat,x
 bmi masksd
 lda temloc
 and #$0e
 bne maskup
masksd lda #1
maskup sta temloc
mazhx1 lda temloc
 plp
 rts
;
vchase lda #0
 sta vflits
 lda vchass
 bne vchdir
 lda #$28
 sta vchasf
 lda #1
 sta vchass
 sta vchasd
vchdir lda vchasd
 cmp #1
 bne vchadn
 lda vchasf
 cmp #$40
 bcc vcupok
 lda #2
 sta vchasd
 bne vcdnok
vcupok lda whiney
 clc
 adc vchasf
 bne strvch
vchadn lda vchasf
 cmp #$28
 bcs vcdnok
 lda #1
 sta vchasd
 bne vcupok
vcdnok sec
 lda vchasf
 sbc whiney
strvch sta vchasf
 sta audf2
 lda #$a1
 sta audc2
vchasx rts
;
blinkr lda rtclok+2
 and #$0f
 beq blnkon
 cmp #8
 beq blnkof
 rts
blnkon ldy #2
 ldx plynum
 lda bigdt1,x
 sta temloc
 lda #1
 bit temloc
 beq bkond2
 sty pacmaz+$7b
bkond2 asl a
 bit temloc
 beq bkond3
 sty pacmaz+$9c
bkond3 asl a
 bit temloc
 beq bkond4
 sty pacmaz+$283
bkond4 asl a
 bit temloc
 bne bkonx1
bkonxx rts
;
blnkof ldy #0
 sty pacmaz+$7b
 sty pacmaz+$9c
 sty pacmaz+$283
bkonx1 sty pacmaz+$2a4
 rts
;
;
; speed sequencing for objects
;
; x reg = index value for object
;
; x=0 to 3 for monsters 1 - 4
; x=4 for pacman
;
; on exit:
; a reg = 0 indicates update time
;
spdseq dec m1spct,x
 beq nxspsq
 lda #$ff
 rts
nxspsq lda m1spsq,x
 cmp #3
 bne incsps
 lda #$ff
 sta m1spsq,x
incsps inc m1spsq,x
 ldy plynum
 lda mazct1,y
 cmp #6
 bcc lowspd
 lda #6
lowspd tay
 cpx #4
 bne mxspsq
 lda pacspd,y
 bpl ldspsq
mxspsq lda monspd,y
ldspsq clc
 adc m1spsq,x
 tay
 lda speed1,y
 sta m1spct,x
 lda #0
 rts
;
; chase sequencing
;
chsseq lda rtclok+2
 and #7
 bne chssq1
 lda chaset
 beq chssq1
 dec chaset
chssq1 ldx plynum
 lda dtctm1,x
 beq chmad1
vrymad ldx #3
 jsr setmad
 lda #5
 bne stwhin
chmad1 lda dtctl1,x
 cmp #$f0
 bcs vrymad
 cmp #$e0
 bcc smlmad
 ldx #2
 jsr setmad
 lda #4
 bne stwhin
smlmad cmp #$b0
 bcc notmad
 lda #3
 bne stwhin
notmad lda #2
stwhin sta whiney
chssq2 ldx #3
chssql lda m1stat,x
 cmp #8
 beq tstchs
 cmp #$10
 beq tstchs
 bne nxchsq
tstchs jsr seepac
nxchsq dex
 bpl chssql
 rts
;
setmad lda m1stat,x
 cmp #8
 beq yesmad
 cmp #$10
 beq yesmad
 cmp #$20
 bne nxstmd
yesmad lda #2
 sta m1stat,x
nxstmd dex
 bpl setmad
 stx chaset
 rts
;
fruity lda frscrf
 beq tstfrt
 lda frscrt
 bne dcfrst
 lda #0
 sta frscrf
 ldx #4
clfrsl sta pacmaz+$1f1,x
 dex
 bpl clfrsl
 rts
dcfrst dec frscrt
dcfrsx rts
tstfrt lda fruflg
 bne dfrtmr
 ldx plynum
 lda dtctl1,x
 tay
 lda frutp1,x
 beq frtst1
 cmp #1
 beq frtst2
 rts
frtst1 cpy #$50
 beq setfrt
frtst2 cpy #$a0
 bne dcfrsx
setfrt inc frutp1,x
 lda mazct1,x
 cmp #$0c
 bcc lowfrc
 lda #$0c
lowfrc tax
 lda fruchr,x
 sta pacmaz+$1f3
 clc
 adc #1
 sta pacmaz+$1f4
 lda #1
 sta fruflg
 lda #fdelay
 sta frutmr
 lda #2
 sta frutmr+1
 rts
dfrtmr lda frutmr+1
 bne dcfrtm
 lda frutmr
 bne dcfrtm
 lda #0
 sta pacmaz+$1f3
 sta pacmaz+$1f4
 sta fruflg
 rts
dcfrtm dec frutmr
 bne fruitx
 dec frutmr+1
fruitx rts
;
 sta fr

; eject

; include d1:pac4.asm

;
; pacman monster subroutines
;
;
eyonly lda #0
 sta tweetr
 ldx #3
eyonlp lda m1stat,x
 cmp #$44
 beq nxteyd
 asl a
 bpl nxteyd
 cpx gulped
 beq nxteyd
 inc tweetr
 lda m1hpos,x
 tay
 lda m1vpos,x
 cpy #$7c
 bne tsteyv
 cmp #$64
 bne tsteyv
 lda m1dirt,x
 cmp #1
 bne eyhome
 lda #4
 sta m1dirt,x
 lda m1vpos,x
 bne tsteyv
eyhome lda #$44
 sta m1stat,x
 lda #$ff
 sta m1sseq,x
 bne nxteyd
tsteyv jsr mazhnd
 bcc sameyd
 clc
 lda #$7c
 sta m1thps,x
 lda #$64
 sta m1tvps,x
 jsr mdirct
sameyd lda m1dirt,x
 jsr monhnd
nxteyd dex
 bpl eyonlp
 lda tweetr
 bne gtweet
 lda #0
 sta tweetf
 beq ckinbx
gtweet lda vfreez
 bne ckinbx
 jsr vtweet
ckinbx ldx #3
nmeylp lda m1stat,x
 clc
 lsr a
 lsr a
 lsr a
 bcc nxnmey
 lda m1stat,x
 asl a
 bpl monout
 jsr pnkmin
 jmp nxnmey
monout lda rtclok+2
 and #3
 bne nxnmey
 cpx #3
 bne grnout
 jsr yelmot
 jmp nxnmey
grnout cpx #2
 bne redout
 jsr grnmot
 jmp nxnmey
redout jsr pnkmot
nxnmey dex
 bpl nmeylp
eyonlx rts
;
pnkmot lda m1vpos,x
 cmp #$64
 bne pnkout
 lda m1stat,x
 bne pnktst
 lda #1
 bne pnkchs
pnktst and #$80
 ora #2
 bpl pnkchs
 sta chaset
 ldy #8
 bne strdir
pnkchs ldy #4
strdir sty m1dirt,x
strsts sta m1stat,x
 rts
pnkout lda #1
pnkot1 sta m1dirt,x
 jmp monhnd
;
reincr lda m1stat,x
 and #$0f
 sta m1stat,x
 lda colors,x
 sta pcolr0,x
 rts
;
grnmot lda m1hpos,x
 cmp #$7c
 beq pnkmot
grnot1 lda #8
 bne pnkot1
;
yelmot lda m1hpos,x
 cmp #$7c
 beq pnkmot
yelout lda #4
 bne pnkot1
;
pnkmin lda m1vpos,x
 cmp #$74
 bne pnkmdn
 cpx #2
 beq grnmin
 cpx #3
 beq yelmin
 bne reincr
pnkmdn lda #2
 bne pnkot1
;
grnmin lda m1hpos,x
 cmp #$70
 bne yelout
 beq reincr
;
yelmin lda m1hpos,x
 cmp #$88
 bne grnot1
 beq reincr
;
strtmn ldx #1
ckssql lda m1stat,x
 and #$7f
 bne nxckss
 lda m1timr,x
 beq cksmup
 dec m1timr,x
 jsr bounce
 jmp nxckss
cksmup cpx #3
 bne ckm2up
 jsr yelmot
 jmp nxckss
ckm2up cpx #2
 bne ckm1up
 jsr grnmot
 jmp nxckss
ckm1up jsr pnkmot
nxckss inx
 cpx #4
 bne ckssql
 rts
;
; startup sequence of
; monsters exiting from
; box.
;
bounce lda m1dirt,x
 cmp #1
 bne tsupsq
 lda m1vpos,x
 cmp #$70
 bne nxtbnc
 lda #2
 bne stsqst
tsupsq lda m1vpos,x
 cmp #$78
 bne nxtbnc
 lda #1
stsqst sta m1dirt,x
nxtbnc lda m1dirt,x
 jmp monhnd
;
monstr lda m1stat,x
 bpl mntst1
 lsr a
 lsr a
 lsr a
 bcc mntsch
 rts
mntst1 cmp #1
 beq mstrtp; monster start path
mntst2 cmp #2
 bne mntst3
 lda chaset
 bne mntsch
 jmp gohome
mntsch jmp mchase; monster is chasing
mntst3 cmp #8
 bne mntst4
 jmp seekps; seek pattern start
mntst4 cmp #$10
 bne mntst5
 jmp mpatrn; continue pattern
mntst5 cmp #$20
 bne mntst6
 jmp gohome; seek home corner
mntst6 rts

mstrtp jsr mazhnd
 bcc sampth
 clc
 inc m1sseq,x
sampth txa
 asl a
 tay
 lda stradd,y
 sta pixget
 iny
 lda stradd,y
 sta pixget+1
 lda m1sseq,x
 tay
 lda (pixget),y
 cmp #$0f
 bne mnpath
;
; - monster has reached start posit
;   set up to find pattern start
;   and set timer for 20 secs
;
 cpx #0
 bne setpat
 lda #$50
 sta chaset
 jmp schase
setpat lda #8
 sta m1stat,x
 lda #0
 sta m1pcnt,x
 lda random
 and #$0f
 sta m1pidx,x
 asl a
 tay
 lda ptrnhv,y
 sta m1thps,x
 iny
 lda ptrnhv,y
 sta m1tvps,x
 lda #$96
 sta m1timr,x
 bne seekps
mnpath sta m1dirt,x
 jmp monhnd
;
; go find start of pattern
;
seekps lda m1timr,x
 bne sekps1
 jmp gohome
sekps1 jsr mazhnd
 bcs sekps2
findst jmp samemd
sekps2 clc
 jsr mdirct
 lda m1hdir,x
 bne findst
 lda m1vdir,x
 bne findst
 lda #$10; reached pattern start
 sta m1stat,x
 clc
 bcc smptrn
;
mpatrn lda m1timr,x
 beq gohome
mptrn1 jsr mazhnd
 bcc smptrn
 clc
 inc m1pcnt,x
smptrn lda m1pidx,x
 tay
 lda ptnidx,y
 adc m1pcnt,x
 tay
 lda ptrn01,y
 bne mptrn2
 sta m1pcnt,x
 lda m1pidx,x
 tay
 lda ptnidx,y
 tay
 lda ptrn01,y
mptrn2 sta m1dirt,x
 jmp samemd
;
gohome lda #$20
 sta m1stat,x
 txa
 asl a
 tay
 lda homehv,y
 sta m1thps,x
 iny
 lda homehv,y
 sta m1tvps,x
;
 jsr mazhnd
 bcc samemd
 clc
 jsr mdirct
 lda m1hdir,x
 bne samemd
 lda m1vdir,x
 bne samemd
 jmp setpat
;
; takes care of chase & flight
;
schase lda #2
 sta m1stat,x
mchase lda pmhpos
 sta m1thps,x
 lda pmvpos
 sta m1tvps,x
 jsr mazhnd
 bcc samemd
 clc
 jsr mdirct
samemd lda m1dirt,x
;
; monster display handler
;
; x-reg value 0-3 to select
; monster 1 - monster 4
;
; a-reg value  1 equals up
;              2 equals down
;              4 equals left
;              8 equals right
;
; selected monster will be 
; incremented 1 pixel in the
; direction specified.
;
; if mstill > 0 then the monsters
; will be drawn but not moved.
;
; bit 7 of status = flight image
; bit 6 of status = eyes image
;
monhnd cmp #1; do repositioning
 bne tstmdn
 ldy mstill
 bne mstlup
 dec m1vpos,x
 dec m1vpos,x
mstlup lda #monsup-monsup
 beq tstmsk
tstmdn cmp #2
 bne tstmlf
 ldy mstill
 bne mstldn
 inc m1vpos,x
 inc m1vpos,x
mstldn lda #monsdn-monsup
 bne tstmsk
tstmlf cmp #4
 bne tstmrt
 ldy mstill
 bne mstllf
 dec m1hpos,x
mstllf lda #monslf-monsup
 bne tstmsk
tstmrt cmp #8
 bne nommot
 ldy mstill
 bne mstlrt
 inc m1hpos,x
mstlrt lda #monsrt-monsup
 bne tstmsk
nommot rts
tstmsk ldy m1stat,x
 bpl tsteye
 lda #monsfl-monsup; flight image
tsteye tay
 lda m1stat,x
 asl a
 bmi mneyes
 tya
 ldy mskirt
 beq mskl54
 ldy #$aa
 bne mnskrt
mskl54 ldy #$54
 bne mnskrt
mneyes lda #monsey-monsup
 ldy #0
mnskrt sty monbuf+$0c
 clc
 adc #low monsup
 sta pixget
 lda #0
 adc #high monsup
 sta pixget+1
 ldy #9
mnldbl lda (pixget),y
 sta monbuf+2,y
 dey
 bpl mnldbl
 lda #low monbuf
 sta pixget
 lda #high monbuf
 sta pixget+1
 lda m1hpos,x
 sta hposp0,x
 lda m1vpos,x
 sta pixput
secure lda #high pmaddr+4
 sta pixput+1
 txa
 clc
 adc pixput+1
 sta pixput+1
 ldy #$0f
mwrite lda (pixget),y
 sta (pixput),y
 dey
 bpl mwrite
 rts
;
; directions computed for
; monsters
;
; x reg = monster #
;
; target vertical & horizontal
; coordinates must be placed
; into vsaver & hsaver prior
; to entry
;
mdirct lda m1vpos,x
 cmp m1tvps,x
 beq vequal
 bcc vgratr;monster is above target
;
; monster is below target
;
 lda m1stat,x
 bmi vbiggr
vlessr lda #1;point to up
 bne strvrt
vgratr lda m1stat,x
 bmi vlessr
vbiggr lda #2;point to down
 bne strvrt
vequal lda #0
strvrt sta m1vdir,x
 lda m1hpos,x
 cmp m1thps,x
 beq hequal
 bcs hgratr;monster is rt of target
;
; monster is left of target
;
 lda m1stat,x
 bmi hbiggr
hlessr lda #8;point to rt
 bne strhrz
hgratr lda m1stat,x
 bmi hlessr
hbiggr lda #4;point to left
 bne strhrz
hequal lda #0
strhrz sta m1hdir,x
;
 lda m1vdir,x
 beq mhonly
 bit temloc
 beq mhonly
 lda m1hdir,x
 beq mvrtok
 bit temloc
 beq mvrtok
;
; choice of directions
;
 lda random
 bmi mhrzok
mvrtok lda m1vdir,x
 bne setmdr
mhrzok lda m1hdir,x
 bne setmdr
mhonly lda m1hdir,x
 bit temloc
 bne mhrzok
;
 lda random
 and temloc
 bne uptest
 lda temloc;reload original
uptest lsr a
 bcc dntest
 lda #1
 bne setmdr
dntest lsr a
 bcc lftest
 lda #2
 bne setmdr
lftest lsr a
 bcc rttest
 lda #4
 bne setmdr
rttest lda #8
setmdr sta m1dirt,x
sammdr lda m1dirt,x
 rts
;
seepac lda m1vpos,x
 cmp pmvpos
 beq seevrt
 lda m1hpos,x
 cmp pmhpos
 beq seehrz
 rts
seevrt ldy #9
seevrl lda vtable,y
 cmp pmvpos
 beq seevr1
 dey
 bpl seevrl
seevrx rts
seevr1 lda m1hpos,x
 cmp pmhpos
 bcs seelft
seergt lda m1dirt,x
 cmp #8
 bne seevrx
seert1 lda hwalls,y
 cmp #$ff
 beq seechs
 cmp m1hpos,x
 bcs seert2
 iny
 bne seert1
seert2 cmp pmhpos
 bcs seechs
 rts
;
seelft lda m1dirt,x
 cmp #4
 bne seevrx
seelf1 lda hwalls,y
 cmp #$ff
 beq seechs
 cmp pmhpos
 bcs seelf2
 iny
 bne seelf1
seelf2 cmp m1hpos,x
 bcs seechs
 rts
;
seehrz ldy #9
seehrl lda htable,y
 cmp pmhpos
 beq seehr1
 dey
 bpl seehrl
seehrx rts
seehr1 lda m1vpos,x
 cmp pmvpos
 bcc seedwn
;
seeupp lda m1dirt,x
 cmp #1
 bne seehrx
seeup1 lda vwalls,y
 cmp #$ff
 beq seechs
 cmp pmvpos
 bcs seeup2
 iny
 bne seeup1
seeup2 cmp m1vpos,x
 bcs seechs
 rts
;
seedwn lda m1dirt,x
 cmp #2
 bne seehrx
seedn1 lda vwalls,y
 cmp #$ff
 beq seechs
 cmp m1vpos,x
 bcs seedn2
 iny
 bne seedn1
seedn2 cmp pmvpos
 bcc seechx
;
seechs cpx #2
 bne strchs
 lda rtclok+2
 bmi seechx
strchs lda #2
 sta m1stat,x
 lda #$a0
 sta chaset
seechx rts
;
; intermission code
;
intmis ldx plynum
 lda mazct1,x
 ldx #$00
 cmp #$01
 beq l7221
 inx 
 inx 
 cmp #$04
 beq l7221
 inx 
 inx 
 cmp #$08
 beq l7221
 cmp #$0c
 beq l7221
 cmp #$10
 beq l7221
 jmp l73aa
l7221 stx intmod
 lda #$ff
 sta intcnt
l7228 lda intcnt
 bne l7228
 ldx #$1c
l722e lda intdlc,x
 sta intrdl,x
 dex 
 bpl l722e
 lda #indli1&255
 sta vdslst
 lda #indli1/256
 sta vdslst+1
 lda #intrdl&255
 sta sdlstl
 sta dlistl
 lda #intrdl/256
 sta sdlsth
 sta dlisth
 ldx #$03
l7251 sta hposm0,x
 dex 
 bpl l7251
l7257 jsr initpt
 ldx intmod
 lda l756b,x
 tay 
 ldx #$15
l7263 lda l7571,y
 sta pmaddr+$300,x
 dey 
 dex 
 bpl l7263
 lda intmod
 cmp #$03
 bne l727d
 lda intseq
 sta pmaddr+$31a
 jmp l72a1
l727d ror a
 bcc l7288
 lda #$2c
 ldx #$7c
 ldy #$cc
 bne l7293
l7288 lda #$ff
 sta pmaddr+$318
 lda #$00
 ldx #$50
 ldy #$a0
l7293 sta intrdl+$12
 stx intrdl+$15
 sty intrdl+$18
 lda #$00
 sta hscrol
l72a1 lda #$ff
 sta intclk
l72a5 lda #$ff
 sta intcnt
l72a9 lda intcnt
 bne l72a9
 lda #$ff
 sta intcnt
 lda pmaddr+$312
 sta intseq
 jsr l73e3
 lda pmaddr+$317
 cmp pmaddr+$30d
 bcs l72c5
 jmp l7346
l72c5 lda intmod
 cmp #$03
 bne l72e9
 lda pmaddr+$31e
 cmp #$13
 beq l72d6
 jmp l7365
l72d6 lda #$8f
 sta pmaddr+$314
 lda #$78
 sta pmaddr+$315
 lda #$00
 tax 
 jsr l746a
 jmp l72a5
l72e9 cmp #$02
 bne l733e
 lda pmaddr+$31f
 beq l7319
 jsr l74f7
 ldx #$01
 jsr l744b
 lda pmaddr+$320
 cmp #$07
 beq l7365
 lda pmaddr+$305
 clc 
 adc pmaddr+$307
 sta pmaddr+$307
 bcc l72a5
 jsr l741d
 jsr l7470
 inc pmaddr+$320
 jmp l72a5
l7319 lda pmaddr+$31b
 cmp #$13
 bne l733e
 lda #$01
 sta pmaddr+$31f
 ldy #$18
l7327 lda l75f5,y
 sta pmaddr+$300,y
 dey 
 bpl l7327
 lda #$00
 sta pmaddr+$31c
 lda intseq
 sta pmaddr+$312
 jmp l7346
l733e jsr l7411
 ldx #$00
 jsr l744b
l7346 lda pmaddr+$310
 beq l735b
 lda pmaddr+$317
 cmp pmaddr+$30e
 bcc l735b
 jsr l74f7
 ldx #$01
 jsr l744b
l735b lda pmaddr+$317
 cmp #$ff
 beq l7365
 inc pmaddr+$317
l7365 lda pmaddr+$31e
 cmp #$3c
 beq l736f
 jmp l72a5
l736f inc intmod
 lda intmod
 cmp #$03
 bne l739c
 jsr l741d
 jsr l741d
 lda #l787a&255
 sta pmaddr+$314
 lda #l787a/256
 sta pmaddr+$315
 lda #$06
 sta pmaddr+$30f
 lda #$00
 tax 
 jsr l746a
 lda #$01
 sta pmaddr+$31e
 jmp l72a5
l739c ror a
 bcc l73a2
 jmp l7257
l73a2 lda #$ff
 sta intcnt
l73a6 lda intcnt
 bne l73a6
l73aa jsr setnul
 jsr initpt
 lda #$ff
 sta intcnt
l73b4 lda intcnt
 bne l73b4
 lda #$00
 sta nmien
 sta irqen
 lda #dlist&255
 sta sdlstl
 sta dlistl
 lda #dlist/256
 sta sdlsth
 sta dlisth
 lda #dliv&255
 sta vdslst
 lda #dliv/256
 sta vdslst+1
 lda #$c0
 sta nmien
 lda #$40
 sta irqen
 rts 
;
l73e3 lda intclk
 cmp #$04
 bcc l740e
 lda pmaddr+$31e
 asl a
 asl a
 tax 
 lda imusic,x
 sta audf1
 lda imusic+1,x
 sta audc1
 lda imusic+2,x
 sta audf2
 lda imusic+3,x
 sta audc2
 inc pmaddr+$31e
 lda #$ff
 sta intclk
l740e inc intclk
 rts 
l7411 lda pmaddr+$303
 clc 
 adc pmaddr+$318
 sta pmaddr+$318
 bcc l744a
l741d lda pmaddr+$31a
 and #$03
 tax 
 eor pmaddr+$300
 sta hscrol
 txa 
 bne l7447
 ldx #$06
l742e lda intrdl+$12,x
 clc 
 adc pmaddr+$302
 sta intrdl+$12,x
 cmp #$ff
 bne l743f
 dec intrdl+$13,x
l743f dex 
 dex 
 dex 
 bpl l742e
 inc pmaddr+$31b
l7447 inc pmaddr+$31a
l744a rts 
l744b lda pmaddr+$307,x
 clc 
 adc pmaddr+$305,x
 sta pmaddr+$307,x
 bcc l74ac
 inc pmaddr+$309,x
 lda pmaddr+$309,x
 cmp pmaddr+$30b,x
 bcc l746d
 lda #$00
 sta pmaddr+$309,x
 txa 
 bne l746d
l746a sta pmaddr+$31c
l746d txa 
 bne l74ad
l7470 lda #$2c
 bit pmaddr+$301
 bmi l747d
 lda #$28
 sec 
 sbc pmaddr+$30f
l747d tax 
 lda pmaddr+$30f
 sta pmaddr+$316
 ldy pmaddr+$31c
 lda pmaddr+$314
 sta pixget
 lda pmaddr+$315
 sta pixget+1
l7491 lda (pixget),y
 sta pmaddr+$400,x
 iny 
 lda (pixget),y
 sta pmaddr+$450,x
 iny 
 lda (pixget),y
 sta pmaddr+$4a0,x
 iny 
 inx 
 dec pmaddr+$316
 bne l7491
 sty pmaddr+$31c
l74ac rts 
l74ad lda #pmaddr/256+5
 sta pixput+1
 lda pmaddr+$313
 sta pixput
 lda pmaddr+$30a
 asl a
 tay 
 ldx intmod
 cpx #$01
 beq l74cb
 lda l760e,y
 ldx l760e+1,y
 jmp l74d1
l74cb lda l761a,y
 ldx l761a+1,y
l74d1 sta pixget
 stx pixget+1
 ldx pmaddr+$310
l74d8 ldy pmaddr+$311
 dey 
l74dc lda (pixget),y
 sta (pixput),y
 dey 
 bpl l74dc
 dex 
 beq l74ac
 lda pixget
 clc 
 adc pmaddr+$311
 sta pixget
 bcc l74f2
 inc pixget+1
l74f2 inc pixput+1
 jmp l74d8
l74f7 lda pmaddr+$304
 clc 
 adc pmaddr+$319
 sta pmaddr+$319
 bcc l751b
 lda pmaddr+$312
 sec 
 sbc pmaddr+$302
 sta pmaddr+$312
 ldx #$00
l750f sta hposp1,x
 clc 
 adc #$08
 inx 
 cpx pmaddr+$310
 bne l750f
l751b rts 
;
indli1 pha 
 txa 
 pha 
 lda #ichars/256
 sta chbase
 lda #$2a
 sta colpm1
 sta colpm2
 sta colpm3
 ldx intmod
 lda icolr0,x
 sta colpf0
 lda icolr1,x
 sta colpf1
 lda icolr2,x
 sta colpf2
 lda icolr3,x
 sta colpf3
 pla 
 tax 
 pla 
 rti
;
setnul lda #nulldl&255
 sta sdlstl
 sta dlistl
 lda #nulldl/256
 sta sdlsth
 sta dlisth
 rts
;
initpt ldx #0
 txa
 jmp initpl
;
 lda #$c0
tnmsl1 cmp tuncnt
 beq sttmsk
 dec tuncnt
 asl tunmsk
 bcs tnmsl1
 lda

; eject

; include d2:pacdat2.asm

;
; pac-man monster data
;
; monster up
;
monsup !byte $38,$7c,$d6,$d6,$d6,$fe
 !byte $fe,$fe,$fe,$fe
;
; monster down
;
monsdn !byte $38,$7c,$fe,$fe,$fe,$fe
 !byte $d6,$d6,$d6,$fe
;
; monster left
;
monslf !byte $38,$7c,$fe,$fe,$ae,$ae
 !byte $ae,$fe,$fe,$fe
;
; monster right
;
monsrt !byte $38,$7c,$fe,$fe,$ea,$ea
 !byte $ea,$fe,$fe,$fe
;
; monster flight
;
monsfl !byte $38,$7c,$fe,$d6,$d6,$d6
 !byte $fe,$d6,$aa,$fe
;
; monster eyes
;
monsey !byte $00,$00,$00,$28,$28,$28
 !byte $00,$00,$00,$00
;
; **** pacman shape data ****
;
; pacman not moving
;
pacdot !byte $38,$7c,$fe,$fe,$fe
 !byte $fe,$fe,$fe,$7c,$38
;
;pacman right
;
pacrgt !byte $38,$7c,$fe,$f8,$e0
 !byte $e0,$f8,$fe,$7c,$38
;
 !byte $38,$7c,$f8,$f0,$e0
 !byte $e0,$f0,$f8,$7c,$38
;
;pacman left
;
paclft !byte $38,$7c,$fe,$3e,$0e
 !byte $0e,$3e,$fe,$7c,$38
;
 !byte $38,$7c,$3e,$1e,$0e
 !byte $0e,$1e,$3e,$7c,$38
;
;pacman up
;
pactop !byte $00,$44,$c6,$c6,$ee
 !byte $ee,$fe,$fe,$7c,$38
;
pacdie !byte $00,$00,$82,$c6,$ee
 !byte $ee,$fe,$fe,$7c,$38
;
;pacman down
;
pacbot !byte $38,$7c,$fe,$fe,$ee
 !byte $ee,$c6,$c6,$44,$00
;
 !byte $38,$7c,$fe,$fe,$ee
 !byte $ee,$c6,$82,$00
;
pacidx !byte $00,$0a
;
fizdat !byte $00,$82,$00,$c6,$82,$00,$7c
 !byte $38,$10,$7c,$38,$38,$10,$10
;
;pacman data address pointers
;
pacadd dw pacdot,pacrgt,paclft,pactop,pacbot
;
; initialize screen data
;
inidat !byte $93,high pacmaz+2
 !byte $02,$a6,$7a,$64,$74,$74,$74,$a4
 !byte $7c,$7c,$70,$88,$7c,$04,$02,$01
 !byte $01,$04
;
; pac-man maze direction data
;
; this data is used to determine
; which directions are permitted
;
vtable !byte $2c,$44,$54,$64,$74,$84,$94,$a4,$b4,$c4
htable !byte $3a,$46,$52,$62,$76,$82,$96,$a6,$b2,$be
;
; horizontal table addr pointers
;
htbadd dw htab01,htab02,htab03,htab04,htab05
 dw htab06,htab07,htab08,htab09,htab10
;
hwall1 !byte $7c,$ff
hwall2 !byte $58,$7c,$9e,$ff
hwall3 !byte $58,$9e,$ff
hwall4 !byte $3c,$ac,$ff
hwall5 !byte $ff
;
hwalls !byte hwall1-hwall1
 !byte hwall5-hwall1
 !byte hwall2-hwall1
 !byte hwall3-hwall1
 !byte hwall1-hwall1
 !byte hwall3-hwall1
 !byte hwall1-hwall1
 !byte hwall4-hwall1
 !byte hwall2-hwall1
 !byte hwall5-hwall1
;
vwall1 !byte $64,$84,$ff
vwall2 !byte $38,$4c,$64,$84,$9c,$bc,$ff
vwall3 !byte $3c,$ff
vwall4 !byte $38,$5c,$9c,$bc,$ff
vwall5 !byte $4c,$74,$8c,$ac,$ff
;
vwalls !byte vwall1-vwall1
 !byte vwall2-vwall1
 !byte vwall3-vwall1
 !byte vwall4-vwall1
 !byte vwall5-vwall1
 !byte vwall5-vwall1
 !byte vwall4-vwall1
 !byte vwall3-vwall1
 !byte vwall2-vwall1
 !byte vwall1-vwall1
;
colors !byte $44,$4a,$d8,$28,$86,$0c,$00,$2a
;
ocolor !byte $da,$0c,$00,$2a
;
acolor !byte $3a,$44,$2a,$da
;
; timer values for blue monsters
startv !byte $90,$60,$30,$04,$00,$00
;
blutim !byte $ff,$c0,$80,$40,$00
 !byte $c0,$00,$00,$00,$c0
 !byte $00,$00,$00,$40,$00
;
; pacman explosion
;
pacexp !byte $00,$00,$00,$00,$00,$92,$54,$00
 !byte $c6,$00,$54,$92
;
; score data for pacman eating
; blue monsters 200-400-800-1600
;
; right half of all scores
blusc0 !byte $00,$00,$00,$00,$c6,$29,$29,$29,$29,$29,$c6,$00
; left half of "200"
blusc1 !byte $00,$00,$00,$00,$38,$45,$05,$19,$21,$41,$7c,$00
; left half of "400"
blusc2 !byte $00,$00,$00,$00,$08,$19,$29,$49,$7d,$09,$08,$00
; left half of "800"
blusc3 !byte $00,$00,$00,$00,$38,$45,$45,$39,$45,$45,$38,$00
; left half of "1600"
blusc4 !byte $00,$00,$00,$00,$8c,$91,$a1,$b9,$a5,$a5,$98,$00,$00
;
; hi notes for intro
;
hinot1 !byte $00,$00,$00,$3c,$00,$51,$00,$60
 !byte $00,$3c,$51,$00,$00,$60,$60,$00
 !byte $00,$00,$00,$39,$00,$4c,$00,$5b
 !byte $00,$39,$4c,$00,$00,$5b,$5b,$00
 !byte $00,$00,$00,$3c,$00,$51,$00,$60
 !byte $00,$3c,$51,$00,$00,$60,$60,$00
 !byte $00,$60,$5b,$55,$00,$55,$51,$4c
 !byte $00,$4c,$48,$44,$00,$3c,$3c,$00
;
; lo notes for intro
;
lonot1 !byte $a2,$f3,$f3,$f3,$00,$00,$00,$00
 !byte $a2,$f3,$f3,$f3,$00,$00,$00,$00
 !byte $99,$e6,$e6,$e6,$00,$00,$00,$00
 !byte $99,$e6,$e6,$e6,$00,$00,$00,$00
 !byte $a2,$f3,$f3,$f3,$00,$00,$00,$00
 !byte $a2,$f3,$f3,$f3,$00,$00,$00,$00
 !byte $a2,$c1,$00,$00,$00,$ad,$00,$00
 !byte $00,$99,$00,$00,$00,$79,$79,$00
;
; address pointers for blue scores
;
blsadd dw blusc1,blusc2,blusc3,blusc4
;
;
; eating dots sound freq data
;
e1data !byte $52,$4d,$48,$43,$3e,$39,$00
e2data !byte $39,$43,$4d,$57,$61,$6b,$00
;
stradd dw redstr,pnkstr,grnstr,yelstr
;
; pattern start hpos & vpos
;
; #1
ptrnhv !byte $96,$a4
; #2
 !byte $62,$74
; #3
 !byte $82,$64
; #4
 !byte $62,$64
; #5
 !byte $62,$94
; #6
 !byte $52,$74
; #7
 !byte $96,$94
; #8
 !byte $a6,$74
; #9
 !byte $96,$54
; #10
 !byte $52,$b4
; #11
 !byte $be,$c4
; #12
 !byte $82,$44
; #13
 !byte $52,$a4
; #14
 !byte $b2,$b4
; #15
 !byte $82,$44
; #16
 !byte $52,$44
;
; home corner hpos and vpos
;
homehv !byte $be,$2c
 !byte $3a,$2c
 !byte $be,$c4
 !byte $3a,$c4
;
; pattern index values
;
ptnidx !byte ptrn01-ptrn01
 !byte ptrn02-ptrn01
 !byte ptrn03-ptrn01
 !byte ptrn04-ptrn01
 !byte ptrn05-ptrn01
 !byte ptrn06-ptrn01
 !byte ptrn07-ptrn01
 !byte ptrn08-ptrn01
 !byte ptrn09-ptrn01
 !byte ptrn10-ptrn01
 !byte ptrn11-ptrn01
 !byte ptrn12-ptrn01
 !byte ptrn13-ptrn01
 !byte ptrn14-ptrn01
 !byte ptrn15-ptrn01
 !byte ptrn16-ptrn01
;
; fruit chars for screen
;
fruchr !byte $3a,$3c,$3e,$3e,$40,$40
 !byte $42,$42,$46,$46,$4a,$4a
;
; fruit data for fruit line
; when mazcnt > 5
;
 !byte $4c,$4c,$4c,$4c,$4c,$4c
 !byte $4c,$4a,$4a,$48,$48,$44
hifrut !byte $44,$40,$40,$3e,$3e,$3c,$3a
;
; pacman speeds - indexed to speed1
;
pacspd !byte $08
 !byte speed3-speed1
 !byte speed3-speed1
 !byte speed4-speed1
 !byte speed5-speed1
 !byte speed6-speed1
 !byte speed6-speed1
;
; monster speeds - indexed to speed1
;
monspd !byte speed1-speed1
 !byte speed2-speed1
 !byte speed3-speed1
 !byte speed3-speed1
 !byte speed4-speed1
 !byte speed5-speed1
 !byte speed5-speed1
;
; high score text
;
hisctx !byte $28,$29,$27,$28,$00,$33,$23,$2f,$32,$25
;
ocindx !byte high optchr,high chrorg,high pacchr,high chrorg,high pacchr
; player game (color0)
oplgam !byte $30,$2c,$21,$39,$25,$32,$00,$27,$21,$2d,$25
; press select for (color0)
oprsel !byte $30,$32,$25,$33,$33,$00,$f3,$e5,$ec,$e5
 !byte $e3,$f4,$00,$26,$2f,$32
; press option to (color0)
oprop1 !byte $30,$32,$25,$33,$33,$00,$ef,$f0,$f4,$e9
 !byte $ef,$ee,$00,$34,$2f,$00
; change difficulty (color0)
oprop2 !byte $23,$28,$21,$2e,$27,$25,$00,$24,$29,$26
 !byte $26,$29,$23,$35,$2c,$34,$39
; press start to (color0)
oprst1 !byte $30,$32,$25,$33,$33,$00,$f3,$f4,$e1,$f2
 !byte $f4,$00,$34,$2f,$00,$00
; play game (color0)
oprst2 !byte $30,$2c,$21,$39,$00,$27,$21,$2d,$25
; (c) atari 1982; color 3
oalogo !byte $c8,$e3,$c9,$00,$e1,$f4,$e1,$f2,$e9,$00
 !byte $d1,$d9,$d8,$d2
bpmsg1 !byte $22,$2f,$2e,$35,$33,$00,$30,$21,$23,$2d
 !byte $21,$2e,$00,$26,$2f,$32
bpmsg2 !byte $33,$23,$2f,$32,$29,$2e,$27,$00,$11,$10
 !byte $0c,$10,$10,$10,$00,$30,$34,$33
;
optfrt !byte $3a,$3c,$3e,$3e,$40,$40
 !byte $44,$44,$48,$48,$4a,$4a,$4c,$4c
;
; top of pacman title
;
pacttl !byte $00,$00,$fe,$ff,$ff,$ff,$fb,$ff
 !byte $00,$00,$00,$80,$c0,$c1,$c1,$c3
 !byte $00,$00,$40,$e0,$e0,$f0,$f0,$f8
 !byte $00,$00,$03,$0f,$1f,$1f,$3f,$3f
 !byte $00,$00,$e0,$f8,$fc,$f8,$f0,$e0
 !byte $00,$00,$00,$00,$00,$00,$00,$3e
 !byte $00,$00,$08,$0c,$0e,$0f,$0f,$0f
 !byte $00,$00,$00,$01,$03,$07,$8f,$df
 !byte $00,$00,$80,$80,$80,$80,$80,$81
 !byte $00,$00,$20,$70,$70,$f8,$f8,$fc
 !byte $00,$00,$08,$0c,$0e,$0f,$0f,$0f
 !byte $00,$00,$1f,$1f,$1f,$1f,$9f,$df
;
; bottom of pacman title
;
 !byte $ff,$ff,$fe,$f8,$f8,$f8,$f8,$00
 !byte $c3,$87,$07,$0f,$0f,$1f,$1f,$00
 !byte $f8,$bc,$fc,$fe,$fe,$ff,$ff,$00
 !byte $3f,$3f,$3f,$1f,$1f,$0f,$03,$00
 !byte $c0,$e0,$f0,$f8,$fc,$f8,$e0,$00
 !byte $3e,$3e,$00,$00,$00,$00,$00,$00
 !byte $0f,$0f,$0f,$0f,$0f,$0f,$0f,$00
 !byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$00
 !byte $81,$83,$83,$87,$87,$8f,$8f,$00
 !byte $fc,$de,$fe,$ff,$ff,$ff,$ff,$00
 !byte $0f,$0f,$0f,$0f,$0f,$8f,$8f,$00
 !byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$00
;
;
; nibbles uncompressed
;
fizidx !byte $06,$07,$07,$08,$08,$08,$09
 !byte $09,$09,$0a,$0a,$0b,$0a,$0b
;
htab01 !byte $0a,$0c,$0e,$0c,$06,$0a,$0c,$0e,$0c,$06
htab02 !byte $0b,$0c,$0f,$0e,$0d,$0d,$0e,$0f,$0c,$07
htab03 !byte $09,$0c,$07,$09,$06,$0a,$05,$0b,$0c,$05
htab04 !byte $00,$00,$03,$0a,$0d,$0d,$06,$03,$00,$00
htab05 !byte $0c,$0c,$0f,$07,$00,$00,$0b,$0f,$0c,$0c
htab06 !byte $00,$00,$03,$0b,$0c,$0c,$07,$03,$00,$00
htab07 !byte $0a,$0c,$0f,$0d,$06,$0a,$0d,$0f,$0c,$06
htab08 !byte $09,$06,$0b,$0e,$0d,$0d,$0e,$07,$0a,$05
htab09 !byte $0a,$0d,$05,$09,$06,$0a,$05,$09,$0d,$06
htab10 !byte $09,$0c,$0c,$0c,$0d,$0d,$0c,$0c,$0c,$05
;
; timer values for flashing monsters
;
flstim !byte $0b,$0b,$0b,$0b,$0b
 !byte $0b,$0b,$0b,$07,$0b
 !byte $0b,$07,$07,$07,$0b
 !byte $07,$01,$07,$01,$01
;
; start up paths for monsters
; leaving box in center
;
redstr !byte $04,$04,$02,$04,$01,$01,$01,$08,$08,$08,$08,$08,$0f
pnkstr !byte $04,$04,$02,$04,$01,$01,$01,$0f
grnstr !byte $08,$08,$02,$02,$02,$08,$02,$0f
yelstr !byte $08,$08,$02,$02,$04,$04,$04,$02,$04,$02,$0f
;
; patterns for monsters to run
;
ptrn01 !byte 2,4,2,4,1,4,1,8,8,8,0
ptrn02 !byte 1,8,8,8,2,2,4,4,4,1,0
ptrn03 !byte 4,4,2,2,8,8,8,1,1,4,0
ptrn04 !byte 8,1,4,1,4,2,2,2,8,1,0
ptrn05 !byte 8,2,4,4,1,8,0
ptrn06 !byte 8,2,2,4,1,1,0
ptrn07 !byte 4,2,8,8,1,4,0
ptrn08 !byte 4,2,2,8,1,1,0
ptrn09 !byte 1,8,2,2,2,4,1,4,1,8,0
ptrn10 !byte 1,8,2,8,2,4,4,4,4,1,8,8,0
ptrn11 !byte 1,4,4,1,4,2,4,2,8,8,8,8,0
ptrn12 !byte 1,8,8,2,4,2,4,2,4,4,2,4,1,1,1,8,8,8,0
ptrn13 !byte 1,4,4,2,8,2,4,2,8,8,8,8,1,4,1,8,1,4,1,1,4,2,2,2,0
ptrn14 !byte 1,8,1,4,4,2,2,8,8,2,4,4,4,4,1,8,1,4,1,8,1,1,8,2,2,2,2,8,0
ptrn15 !byte 8,2,4,2,8,2,2,4,4,4,1,1,8,1,4,1,8,8,0
ptrn16 !byte 2,4,4,1,8,8,1,8,8,2,8,1,8,8,8,8,2,4,4,4,2,4,2,4,1,4,1,4,0

;
; fruit score table for computing
; score for active player - 2 #s
;
frstab !byte $00,$01,$00,$03,$00,$05,$00,$05
 !byte $00,$07,$00,$07,$01,$00,$01,$00
 !byte $02,$00,$02,$00,$03,$00,$03,$00
 !byte $05,$00
;
; speed sequencing values
;
speed1 !byte 3,3,3,3
speed2 !byte 3,2,3,2
speed3 !byte 2,2,2,3
speed4 !byte 2,2,2,2
speed5 !byte 2,2,2,1
speed6 !byte 1,2,1,2
;
revtab !byte $00,$0d,$0e,$00,$07,$00,$00,$00,$0b
blurev !byte $00,$02,$01,$00,$08,$00,$00,$00,$04
;
; fruit scores
;
; fruit score 100
fs0100 !byte $00,$4e,$4f,$5f,$60
; fruit score 300
fs0300 !byte $00,$50,$51,$5f,$60
; fruit score 500
fs0500 !byte $00,$52,$53,$5f,$60
; fruit score 700
fs0700 !byte $00,$54,$55,$5f,$60
; fruit score 1000
fs1000 !byte $56,$57,$5e,$5f,$60
; fruit score 2000
fs2000 !byte $58,$59,$5e,$5f,$60
; fruit score 3000
fs3000 !byte $5a,$5b,$5e,$5f,$60
; fruit score 5000
fs5000 !byte $5c,$5d,$5e,$5f,$60
;
; index values for above scores
;
fsindx !byte $00,$05,$0a,$0a,$0f,$0f
 !byte $14,$14,$19,$19,$1e,$1e,$23
;
; attract mode data
;
;
; attract mode colors
;
acolrs !byte $44,$4a,$d8,$28,$44,$0c,$00,$28
; small dot char 3
atchrs !byte $00,$00,$00,$18,$18,$00,$00,$00
; energizer dot char 4
 !byte $18,$18,$3c,$3c,$3c,$3c,$18,$18
; top of monster char 5
 !byte $00,$00,$00,$00,$00,$38,$7c,$fe
; body of monster char 6
 !byte $fe,$ea,$ea,$ea,$fe,$fe,$fe,$aa
;
; text for char/nickname (color 1)
chnktx !byte $00,$63,$68,$61,$72,$61,$63,$74,$65,$72
 !byte $4f,$6e,$69,$63,$6b,$6e,$61,$6d,$65,$00
; text for w shadow (color 0)
chartx !byte $00,$06,$00,$33,$28,$21,$24,$2f,$37,$00
; text for "blinky" (color 0)
 !byte $00,$02,$22,$2c,$29,$2e,$2b,$39,$02,$00
; text for x speedy (color 1)
 !byte $00,$46,$00,$73,$70,$65,$65,$64,$79,$00
; text for "pinky"  (color 1)
 !byte $00,$42,$70,$69,$6e,$6b,$79,$42,$00,$00
; text for y bashful (color 2)
 !byte $00,$86,$00,$a2,$a1,$b3,$a8,$a6,$b5,$ac
; text for "inky"    (color 2)
 !byte $00,$82,$a9,$ae,$ab,$b9,$82,$00,$00,$00
; text for z pokey   (color 3)
 !byte $00,$c6,$00,$f0,$ef,$eb,$e5,$f9,$00,$00
; text for "clyde"   (color 3)
 !byte $00,$c2,$e3,$ec,$f9,$e4,$e5,$c2,$00,$00
; text for a 10 pts
; text is color 0 dot is color 2
am10pt !byte $83,$00,$11,$10,$00,$30,$34,$33
; text for b 50 pts
am50pt !byte $84,$00,$15,$10,$00,$30,$34,$33
; (c) atari 1982     (color 2)
copmsg !byte $00,$00,$00,$88,$a3,$89,$00,$a1,$b4,$a1
 !byte $b2,$a9,$00,$91,$99,$98,$92,$00,$00,$00
;
;
; data for intermissions
;
l756b = *; y index value
;
 !byte $15,$2b,$41,$57,$6d,$83
;
l7571 = *
;
 !byte $03,$80,$01,$c0,$ba,$20,$50,$00
 !byte $00,$02,$05,$02,$05,$1d,$00,$04
 !byte $02,$13,$d0,$84
 dw l780b
;
 !byte $00,$00,$ff,$c0,$ff,$20,$50,$00
 !byte $00,$02,$03,$02,$03,$00,$70,$04
 !byte $03,$27,$10,$72
 dw l77f3
;
 !byte $03,$80,$01,$c0,$ba,$20,$50,$00
 !byte $00,$02,$05,$02,$05,$30,$00,$04
 !byte $02,$13,$d0,$84
 dw l780b
;
 !byte $03,$80,$01,$00,$00,$04,$10,$00
 !byte $00,$04,$00,$04,$00,$00,$00,$07
 !byte $00,$00,$00,$00
 dw l787a
;
 !byte $03,$80,$01,$c0,$ba,$20,$50,$00
 !byte $00,$02,$05,$02,$05,$30,$00,$04
 !byte $02,$13,$d0,$84
 dw l7823
;
 !byte $00,$00,$ff,$b8,$00,$20,$00,$00
 !byte $00,$03,$00,$03,$00,$00,$00,$07
 !byte $00,$00,$00,$00
 dw l783b
;
l75f5 = *
;
 !byte $03,$80,$01,$30,$ba,$30,$50,$00
 !byte $00,$02,$05,$02,$05,$00,$00,$06
 !byte $02,$13,$d0,$84
 dw l78a4

 !byte $00,$00,$00
;
l760e = *
;
 dw l7622,l7648,l766e
 dw l7622,l7648,l766e
;
l761a = *
;
 dw l7694,l7709,l777e,l7709
;
l7622 = *
;
 !byte $07,$1f,$3f,$1f,$0f,$07,$03,$01
 !byte $00,$00,$00,$01,$03,$07,$0f,$1f
 !byte $3f,$1f,$07,$00,$c0,$e0,$e0,$f0
 !byte $f0,$f0,$f8,$f8,$78,$f8,$f8,$f0
 !byte $f0,$f0,$e0,$e0,$c0,$00
;
l7648 = *
;
 !byte $07,$1f,$3f,$3f,$7f,$7f,$7f,$0f
 !byte $01,$00,$01,$0f,$7f,$7f,$7f,$3f
 !byte $3f,$1f,$07,$00,$c0,$e0,$e0,$f0
 !byte $f0,$f0,$f8,$f8,$78,$f8,$f8,$f0
 !byte $f0,$f0,$e0,$e0,$c0,$00
;
l766e = *
;
 !byte $07,$1f,$3f,$3f,$7f,$7f,$7f,$ff
 !byte $ff,$ff,$ff,$ff,$7f,$7f,$7f,$3f
 !byte $3f,$1f,$07,$00,$c0,$e0,$e0,$f0
 !byte $f0,$f0,$f8,$f8,$f8,$f8,$f8,$f0
 !byte $f0,$f0,$e0,$e0,$c0,$00
;
l7694 = *
;
 !byte $00,$01
 !byte $03,$07,$0f,$0f,$1f,$1f,$3f,$3f
 !byte $3f,$7f,$7f,$7f,$7f,$ff,$ff,$ff
 !byte $ff,$ff,$ff,$ff,$ff,$ff,$7f,$7f
 !byte $7f,$7f,$3f,$3f,$3f,$1f,$1f,$0f
 !byte $0f,$07,$03,$01,$00,$78,$fe,$ff
 !byte $ff,$ff,$ff,$ff,$ff,$ff,$fe,$fe
 !byte $fc,$f8,$f8,$f0,$e0,$e0,$c0,$c0
 !byte $80,$c0,$c0,$e0,$e0,$f0,$f8,$f8
 !byte $fc,$fe,$fe,$ff,$ff,$ff,$ff,$ff
 !byte $ff,$ff,$fe,$78,$00,$00,$00,$80
 !byte $c0,$c0,$80,$80,$00,$00,$00,$00
 !byte $00,$00,$00,$00,$00,$00,$00,$00
 !byte $00,$00,$00,$00,$00,$00,$00,$00
 !byte $00,$00,$00,$80,$80,$c0,$c0,$80
 !byte $00,$00,$00
;
l7709 = *
;
 !byte $00,$01,$03,$07,$0f
 !byte $0f,$1f,$1f,$3f,$3f,$3f,$7f,$7f
 !byte $7f,$7f,$ff,$ff,$ff,$ff,$ff,$ff
 !byte $ff,$ff,$ff,$7f,$7f,$7f,$7f,$3f
 !byte $3f,$3f,$1f,$1f,$0f,$0f,$07,$03
 !byte $01,$00,$78,$fe,$ff,$ff,$ff,$ff
 !byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
 !byte $ff,$ff,$fe,$f8,$e0,$80,$e0,$f8
 !byte $fe,$ff,$ff,$ff,$ff,$ff,$ff,$ff
 !byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$fe
 !byte $78,$00,$00,$00,$80,$c0,$c0,$e0
 !byte $e0,$f0,$f0,$f0,$f8,$f8,$f8,$e0
 !byte $80,$00,$00,$00,$00,$00,$00,$00
 !byte $80,$e0,$f8,$f8,$f8,$f0,$f0,$f0
 !byte $e0,$e0,$c0,$c0,$80,$00,$00,$00
;
l777e = *
;
 !byte $00,$01,$03,$07,$0f,$0f,$1f,$1f
 !byte $3f,$3f,$3f,$7f,$7f,$7f,$7f,$ff
 !byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
 !byte $7f,$7f,$7f,$7f,$3f,$3f,$3f,$1f
 !byte $1f,$0f,$0f,$07,$03,$01,$00,$78
 !byte $fe,$ff,$ff,$ff,$ff,$ff,$ff,$ff
 !byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
 !byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
 !byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
 !byte $ff,$ff,$ff,$ff,$fe,$78,$00,$00
 !byte $00,$80,$c0,$c0,$e0,$e0,$f0,$f0
 !byte $f0,$f8,$f8,$f8,$f8,$fc,$fc,$fc
 !byte $fc,$fc,$fc,$fc,$fc,$fc,$f8,$f8
 !byte $f8,$f8,$f0,$f0,$f0,$e0,$e0,$c0
 !byte $c0,$80,$00,$00,$00
;
l77f3 = *
;
 !byte $01,$04,$08,$02,$05,$09,$03,$06
 !byte $0a,$00,$07,$0b,$01,$04,$0c,$02
 !byte $05,$0d,$03,$06,$0e,$00,$07,$0f
;
l780b = *
;
 !byte $01,$10,$14,$02,$11,$15,$03,$12
 !byte $16,$00,$13,$17,$01,$10,$18,$02
 !byte $11,$19,$03,$12,$1a,$00,$13,$1b
;
l7823 = *
;
 !byte $01,$10,$14,$02,$11,$15,$03,$54
 !byte $d6,$00,$d5,$57,$01,$10,$18,$02
 !byte $11,$19,$03,$58,$da,$00,$d9,$5b
;
l783b = *
;
 !byte $00,$00,$3a,$00,$00,$3b,$00,$00
 !byte $3c,$00,$36,$3d,$00,$b7,$3e,$00
 !byte $b8,$3f,$00,$b9,$00,$00,$00,$44
;
 !byte $00,$00,$45
 !byte $00,$00,$46,$00,$40,$47,$00,$c1
 !byte $48,$00,$c2,$49,$00,$c3,$00,$00
 !byte $00,$4e,$00,$00,$4f,$00,$00,$50
 !byte $00,$4a,$51,$00,$cb,$52,$00,$cc
 !byte $53,$00,$cd,$00
;
l787a = *
 !byte $00,$2b,$14,$29
 !byte $2c,$15,$2a,$2d,$af,$00,$ae,$b0
 !byte $00,$00,$00,$00,$00,$31,$00,$00
 !byte $32,$00,$33,$14,$29,$34,$15,$2a
 !byte $35,$af,$00,$ae,$b0,$00,$00,$00
 !byte $00,$00,$31,$00,$00,$32
;
l78a4 = *
 !byte $01,$10
 !byte $14,$02,$11,$15,$03,$12,$16,$00
 !byte $13,$1c,$00,$00,$00,$00,$00,$00
 !byte $01,$10,$18,$02,$11,$19,$03,$12
 !byte $1d,$00,$13,$1e,$00,$00,$00,$00
 !byte $00,$00,$01,$10,$14,$02,$11,$15
 !byte $03,$12,$16,$00,$13,$1f,$00,$00
 !byte $00,$00,$00,$00,$01,$10,$18,$02
 !byte $11,$19,$03,$12,$1d,$00,$13,$20
 !byte $00,$00,$21,$00,$00,$00,$01,$10
 !byte $14,$02,$11,$15,$03,$12,$16,$00
 !byte $22,$23,$00,$00,$24,$00,$00,$00
 !byte $01,$10,$18,$02,$11,$19,$03,$12
 !byte $1d,$00,$25,$26,$00,$00,$1f,$00
 !byte $00,$00,$01,$10,$18,$02,$11,$19
 !byte $03,$12,$1d,$00,$27,$26,$00,$00
 !byte $20,$00,$00,$28
;
icolr0 !byte $32,$82,$32,$32,$32,$0f
icolr1 !byte $0f,$0f,$0f,$0f,$0f,$fa
icolr2 !byte $63,$00,$63,$64,$64,$33
icolr3 !byte $00,$00,$fa,$fa,$fa,$83
;
;
imusic = *
;
 !byte $48,$aa,$90,$a8,$00,$aa,$90,$a8
 !byte $48,$aa,$00,$a8,$00,$aa,$90,$a8
 !byte $48,$aa,$90,$a8,$00,$aa,$90,$a8
 !byte $55,$aa,$00,$a8,$60,$aa,$90,$a8
 !byte $48,$aa,$90,$a8,$48,$aa,$90,$a8
 !byte $48,$aa,$00,$a8,$48,$aa,$90,$a8
 !byte $39,$aa,$72,$a8,$39,$aa,$6c,$a8
 !byte $39,$aa,$66,$a8,$39,$aa,$60,$a8
 !byte $48,$aa,$90,$a8,$48,$a0,$90,$a8
 !byte $48,$aa,$90,$a0,$48,$a0,$90,$a8
 !byte $48,$aa,$90,$a8,$48,$a0,$90,$a8
 !byte $55,$aa,$00,$a8,$60,$aa,$90,$a8
 !byte $48,$aa,$90,$a8,$48,$aa,$90,$a8
 !byte $55,$aa,$00,$a8,$72,$aa,$90,$a8
 !byte $72,$aa,$6c,$a8,$72,$aa,$66,$a8
 !byte $72,$aa,$60,$a8,$48,$aa,$90,$a8
 !byte $00,$aa,$90,$a8,$48,$aa,$00,$a8
 !byte $00,$aa,$90,$a8,$48,$aa,$90,$a8
 !byte $55,$a0,$90,$a8,$55,$aa,$1d,$a0
 !byte $60,$aa,$90,$a8,$48,$aa,$90,$a8
 !byte $48,$aa,$90,$a8,$39,$aa,$90,$a0
 !byte $39,$aa,$90,$a8,$35,$aa,$72,$a8
 !byte $35,$aa,$6c,$a8,$32,$aa,$66,$a8
 !byte $32,$aa,$60,$a8,$2f,$aa,$5b,$a8
 !byte $2f,$aa,$60,$a8,$35,$aa,$6c,$a8
 !byte $35,$aa,$6c,$a8,$39,$aa,$72,$a8
 !byte $39,$aa,$72,$a8,$48,$aa,$90,$a8
 !byte $48,$aa,$90,$a8,$39,$aa,$60,$a8
 !byte $39,$aa,$60,$a8,$48,$aa,$72,$a8
 !byte $48,$aa,$72,$a8,$00,$00,$00,$00
;

; eject
;
 org $7400
;
 include d2:pacdat3.asm
;
 end init