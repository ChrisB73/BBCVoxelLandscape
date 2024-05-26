; Voxel Asm bit

oswrch=&ffee
osbyte=&fff4
osword=&fff1
osfile=&ffdd

screenheight=200

scale=&70
left_x=&72
dx=&74
oy=&76          ; 2 bytes - allow 4
ZX0_src=&78
plotposition=&78
value=&7a
ZX0_dst=&7b
iscaled=&7b         ; 2 bytes
yb=&a00
cb=&a44
lb=&a88
horizon=&7d
height=&7e
colour=&7f
multiplier=&80
topposition=&80
multresult=&81  ; 2 bytes
linelength=&83
temp=&84            ; 2 bytes
pntr=&84           ; ZX02 - 2 bytes
offset=&86      ; ZX02
player_x=&86
bitr=&88        ; ZX02
player_y=&88
drawdistance=&8a
scaleshift=&8b
zpointer=&8c
swtype=&8d
swrambank=&8e
screenstart=&8f
plotposition2=&a8

lastheight1=&aa
lastheight2=&ab
currentheight=&ac

size=128
screenwidth=64
sqrLow=&400
sqrHigh=&600

org &1100
guard &1C00
include "voxlookup.asm"

.voxelstart


; Set screen mode and background colour
    lda #22
    jsr oswrch
    lda #2
    jsr oswrch

    lda #&09
    sta crtcregs+12
    lda #&c0
    sta crtcregs+13

; Resize screen mode
   ; Set CRTC
    ldx #13
.crtcloop
    jsr setcrtc

    dex
    bpl crtcloop

; Set initial variables
    lda #16:sta player_x+1
    lda #0:sta player_x:sta player_y
    sta player_y+1
    sta lastheight1:sta lastheight2

    lda #223:sta height
    lda #32:sta horizon
    lda #48:sta drawdistance            ;160 mark

    lda #&4e
    sta screenstart

    ; Values for screenstart are &4e and &1c 

; Main draw procedure
.mainloop

;  Reset yb  
    ldx #screenwidth
    lda #screenheight          ; 200 in lowest line (25*8)
.ybresetlp
    sta yb-1,x
    sta lb-1,X
    dex
    bne ybresetlp

;  Reset yb  
    ldx #screenwidth
    lda #0
.cbresetlp
    sta cb-1,x
    dex
    bne cbresetlp

    ldx #4 ; Starting location for scale. Setting this too high means
           ; some of the very closest rows might not be drawn
           ; Too low and unnecessary calculations.

;Z loop
.rowloop
    ;     left_x% = (player_x - z)*256     
    stx zpointer
    sec
    lda player_x
    sbc zlo,X
    sta left_x
    lda player_x+1
    sbc zhi,X
    sta left_x+1
    ;dx%=(right_x% - left_x%)/(SW%/2)
    lda zlo,X
    sta dx
    lda zhi,X
    ; Divide by 32
    lsr a:ror dx
    lsr a:ror dx
    lsr a:ror dx
    lsr a:ror dx
    lsr a:ror dx
    sta dx+1
;    lda scalelo,x
;    sta scale
;    lda scalehi,X
;    sta scale+1

    ;left_y = player_y + z
    ; Store into temp
    clc
    lda player_y
    adc zlo,X
    ;sta temp           ; Don't care about low result
    lda player_y+1
    adc zhi,X
    ;sta temp+1          ; ???

    ;      oy%=((INT(left_y/2)MOD size%)*size%)+outpic
    ; A contains Y hi
;    lsr A       ; lst_y/2 MOD side% : Size%=128
    and #127
    ldy #0
    sty temp
    lsr A
    ror temp
    sta temp+1  ; * 128

    ; Carry is clear
    lda temp
;    adc #lo(outpic)
    sta oy
    lda temp+1
    ora #&80
;    adc #hi(outpic)
    sta datasource+1

    ldy #0
; Restore Scale every time
    ldx zpointer
    lda scalelo,x
    sta scale
    lda scalehi,X
    sta scale+1

    beq noshiftdown
.shiftdownlp
    lsr a
    ror scale
    iny
    cmp #0
    bne shiftdownlp
    sta scale+1
.noshiftdown
    sty scaleshift
 
    ;FOR I%=1 TO SW%
    ldx #0
    stx iscaled

    lda screenstart            ; Screen start
    sta iscaled+1       ; iscaled = i*8 (x*8) + screenstart

.columnloop
    ;      B%=(INT(left_x%DIV256)MODsize%)+oy%
    lda left_x+1
    and #127
    ora oy
    sta datasource
datasource=P%+1
    lda datasource
    ;      V%=?B%
    sta value

    stx temp
    ;      L% = ((H%-V%) * scale%)DIV256 + Z%
    lda height
    sec
    sbc value
    php
    bcs notneg
    eor #&FF
    sec
    adc #0              ; Negate for multiply.

.notneg
    sta multiplier
 ; *scale


; Do multiply
    ldx scale
    lda multiplier
    sta getLow+1
    sta getHigh+1

    stx msubpointer+1
    sec
.msubpointer
    sbc #0
    bcs doMult
    sbc #0
    eor #255
.doMult
    tay
.getLow
    lda sqrLow,x
    sbc sqrLow,y
    sta multresult
.getHigh
    lda sqrHigh,x
    sbc sqrHigh,y

    ldy scaleshift
    beq noshiftup

    ; Shift result back up
.shiftuplp
    asl multresult
    rol a
    bcs clearstack              ; Overflow so no draw
    dey
    bne shiftuplp

.noshiftup
    sta multiplier+1
    plp
    bcs adddiff             ; Positive from before multiply

    ; Subtract from horizon
    sec
    lda horizon
    sbc multiplier+1
    bcs drawcarryon     ; No overflow
    lda #0               ; Set to min
    bcc drawcarryon     ; Always taken

.clearstack
    lda #255                          ; Set to max
    bne noshiftup                   ; Always taken 

; DIV 256 means just using multiplier+1 = length
.adddiff
    ; Add Add to horizon
    clc
    lda horizon
    adc multiplier+1                 ; Add Z
    bcc drawcarryon                        ; no Overflow 
    lda #255                          ; Set to max

.drawcarryon
    ldx temp
    cmp yb,x 
    bcs nodraw
    sta multiplier+1

;    sta drawheight
    lda cb,x
    bne notfirstcolour                ; Not first colour
    ldy value
    lda &900,y
    sta cb,X
    bne nodrawthistime              ; Always taken 

.notfirstcolour
    ldy value
    lda &900,y
    cmp cb,X
    beq nodrawthistime ; Colour is same as previous line - don't doraw yet

.linetodraw
    tay
    lda cb,X    ; Get previous colour
    sta colour
    tya
    sta cb,x    ; Store new colour

    ; Have a line to draw
        ; Calculate vertical position
;    lda multiplier+1    ; Value
    lda yb,x
    and #%11111000              ; Remove bottom bits
    lsr a:lsr a;:lsr a         ; Convert to 512*lines
    ;clc                    ; Not necessary because bottom bits cleared
    adc iscaled+1
    sta plotposition+1       ; Iscaled is now top of line
;    lda multiplier+1
    lda yb,x
    and #%111                   ; Bottom bits
    ora iscaled
;    lda iscaled
    sta plotposition

    lda lb,x
    sec
;    sbc multiplier+1            ; Total line length
    sbc yb,x
    ;tay
;    lda multiplier+1
;    sta lb,x                    ; Store new line bottom
    ;dey
    ;tya
    sta linelength
    jsr drawline                 ; Line length

    ldy yb,x
    iny
    tya
    sta lb,x                    ; Store new start position
    
.nodrawthistime
    lda multiplier+1
    sta yb,X        ; Store new max height

;      IF L% < yb%?I% THEN PROCdraw_vline(I%, L%, yb%?I%):yb%?I%=L%
.nodraw
;      left_x%=left_x%+ dx%


    lda iscaled
    clc
    adc #8
    sta iscaled
    lda iscaled+1
    adc #0
    sta iscaled+1

    clc
    lda left_x
    adc dx
    sta left_x
    lda left_x+1
    adc dx+1
    sta left_x+1        

    inx
    cpx #screenwidth             ; Screen width
    beq columnloopfinished
    jmp columnloop      ; Branch > 127
.columnloopfinished

    ;  UNTIL z>=distance

    ldx zpointer
halfzpos=P%
    nop:inx
    cpx drawdistance
    bcs doneframe
    jmp rowloop

.doneframe

.drawsky
    ; now do final cyan sky
    ldx #screenwidth-1
;    lda #%00111100      ; Colour 6
;    sta colour

    lda #lo(512-8)
    sta iscaled
    lda screenstart
    clc
    adc #hi(512-8)
    sta iscaled+1

    lda #screenheight
    sta currentheight

.skyloop
    ; Draw final line
    lda cb,X    ; Get prebious colour
    sta colour
    lda yb,x
    cmp currentheight
    bcs skipupdatecurrent
    sta currentheight
.skipupdatecurrent
    and #%11111000              ; Remove bottom bits
    lsr a:lsr a;:lsr a         ; Convert to 512*lines
    ;clc                    ; Not necessary because bottom bits cleared
    adc iscaled+1
    sta plotposition+1
    lda yb,x
    and #%111                   ; Bottom bits
    ora iscaled
    sta plotposition

    lda lb,x
    sec
    sbc yb,x
    sta linelength
    jsr drawline

.skynofinaldraw
    lda yb,X
    beq skipsky1

; Start pos =0
;    lda #0                  ; Top position
;    jsr oswrch
    lda lastheight2
    cmp yb,X
    bcs skipsky1
    ; calculate plotposition
    and #%1111000
    lsr a:lsr a
    adc iscaled+1
    sta plotposition+1

    lda lastheight2
    and #%111
    ora iscaled
    sta plotposition

    lda #%00111100
    sta colour

    lda yb,X
    sec
    sbc lastheight2
    beq skipsky1
    sta linelength
    jsr drawline

.skipsky1
    sec
    lda iscaled
    sbc #8
    sta iscaled
    lda iscaled+1
    sbc #0
    sta iscaled+1

    dex
    bpl skyloop

    ldx lastheight1
    stx lastheight2
    ldx currentheight
    stx lastheight1

; Swap screens and update screen starts
    lda #19
    jsr osbyte          ; Wait vsync

    sei
    ldx #13
    jsr setcrtc
    ldx #12
    jsr setcrtc
    cli
    lda screenstart
    eor #&52            ; Change between &4e &1c
    sta screenstart
    lda crtcregs+12
    eor #&A             ; Swap between 09 and 03
    sta crtcregs+12
    lda crtcregs+13
    eor #&40            ; Swap between &C and &9
    sta crtcregs+13

    ; Process keys
.checkkeylp
    lda &Ed
    bmi gotkey
    lda &Ec
    bpl checkkeylp          ; Wait for key.
.gotkey
    lda #33+128             ;W
    jsr keycheck
    bne notw
    inc player_y+1
.notw
    lda #81+128             ;s
    jsr keycheck
    bne nots
    dec player_y+1
.nots
    lda #65+128             ;A
    jsr keycheck
    bne nota
    dec player_x+1
.nota
    lda #50+128             ;D
    jsr keycheck
    bne notd
    inc player_x+1
.notd

    lda #69+128             ;J
    jsr keycheck
    bne notj
    ldx horizon
    inx:inx:inx
    bmi notj
    stx horizon
.notj
    lda #85+128             ;N
    jsr keycheck
    bne notn
    ldx horizon
    dex:dex:dex
    bmi notn
    stx horizon
.notn


    lda #70+128             ;K
    jsr keycheck
    bne notk
    lda height
    clc
    adc #8
    cmp #8
    bcc notk
    sta height
.notk
    lda #101+128             ;M
    jsr keycheck
    bne notm
    lda height
    sec
    sbc #8
    cmp #8
    bcc notm
    sta height
.notm

    lda #84+128             ;H
    jsr keycheck
    bne noth
    lda drawdistance
    cmp #128-4
    bcs noth
    clc
    adc #4
    sta drawdistance

.noth
    lda #100+128             ;B
    jsr keycheck
    bne notb
    lda drawdistance
    cmp #16+4
    bcc notb
    sec
    sbc #4
    sta drawdistance
.notb

    lda #99+128             ;V
    jsr keycheck
    bne notv
    lda halfzpos
    eor #2                   ; INX/NOP
    sta halfzpos
.notv


    lda #86+128             ;L
    jsr keycheck
    bne notl
    jmp loadnew

.notl

;    inc player_y+1
;    dec height:dec height
;    inc horizon
    jmp mainloop

.keycheck
    cmp &Ec
    beq keycheckend
    cmp &Ed
.keycheckend
    rts

.docolour
    ldy #%00110000           ;4
    lda value
    beq dogcol
    ldy #%00001111  ;3
    cmp #5
    bcc dogcol
    ldy #%00001100         ;2
    cmp #19
    bcc dogcol
    ldy #%00111111           ;7
    cmp #&57
    bcs dogcol
    ldy #%00110011            ;5
    cmp #&37
    bcs dogcol
    ldy #%00000011               ;1
.dogcol
    tya
    sta colour
    rts

.drawline
    ; Takes Multiplier as line length
    ; Multiplier+1 as start location
    ; colour as colour 
    ; iscaled as start location
;    lda topposition
;    and #7              ; Invert 

    lda plotposition
    and #7
    sta topposition                 ; save start offset
    lda plotposition
    and #%11111000
    sta plotposition                ; Strip bottom bits

    ; Draw first group of 8
    ; Work out if there are less bytes than a single group
    lda linelength
    cmp #8
    bcs dlmorethantop    ; More than 8 length so must be to end of first group

    lda topposition
    adc linelength
    cmp #8              ; More than 8 so is to end of first group
    bcs dlmorethantop
    ; Not more than top

    ldy linelength
    lda branchtabler,y
    sta topbranch+1     ; Store as branch
    ldy topposition

    lda colour
.topbranch
    bpl dllessstart     ; Always taken
.dllessstart
    ; Line drawing. Code is branched into to save loop checking 
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y
    rts                 ; Finish as it's only single byte

.dlmorethantop
    ; Write a "complete" top byte

    ldy topposition
    lda branchtable,y
    sta topwholebranch+1     ; Write start branch

    lda colour          ; Get colour to plot
.topwholebranch
    bpl dlwholestart     ; Always taken
.dlwholestart
    ; Line drawing. Code is branched into to save loop checking
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y

    ldy topposition
    lda linelength  
    sec
    sbc revtable,y
    sta linelength      ; Reduce by number we just plotted.
    bne dlcarryon
    rts                 ; If zero then finished

.dlcarryon
    cmp #8              ; Do we have whole block to finish?
    bcc lastblock

    lda plotposition
    ora #4              ; Always byte aligned so we can assume or for +4
    sta plotposition2    ; Multresult is used here to save zp usage - not necessary
    lda plotposition+1
    sta plotposition2+1

.dlmainlp
    ldy plotposition+1
    iny:iny
    sty plotposition+1
    sty plotposition2+1

    ldy #0
    lda colour
    ; Line drawing. Draw a block of 8
    sta (plotposition2),Y
    sta (plotposition),Y:iny
    sta (plotposition2),Y
    sta (plotposition),Y:iny
    sta (plotposition2),Y
    sta (plotposition),Y:iny
    sta (plotposition2),Y
    sta (plotposition),Y

    lda linelength
    sec
    sbc #8
    sta linelength      ; Store value
    cmp #8
    bcs dlmainlp        ; More than 8 loop whole block

.lastblock
    ; Draw remaining lines

    ldy linelength      ; Remainder
    beq dlfinished

    inc plotposition+1  ; Increment screen position
    inc plotposition+1  ; Increment screen position

    lda branchtabler,y
    sta bottombranch+1     ; Write last branch

    ldy #0              ; Start at top of block
    lda colour          ; Get colour to plot
.bottombranch
    bpl dlbottomblock     ; Always taken
.dlbottomblock
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y

.dlfinished
    rts
;    txa:pha
;    lda #19:jsr osbyte
;    pla:tax

;    rts

; Table for branch calculation - Reversed
.branchtabler
FOR i, 0, 7
equb 3*(7-i)
NEXT    

; Table for branch calculation - Normal
.branchtable
FOR i, 0, 7
equb 3*i
NEXT    


.revtable
equb 8,7,6,5,4,3,2,1

.setcrtc
    ; sets crtc variable X
    lda #23
    jsr oswrch
    lda #0
    jsr oswrch
    txa
    jsr oswrch
    lda crtcregs,X
    jsr oswrch

    ldy #6
    lda #0
.sendzerolp
    jsr oswrch
    dey
    bne sendzerolp
    rts

.swsetrom
;Routine to set SWRAM Bank
;Expects bank to set in X
;Returns current bank in Y
    LDY &F4
.swsetromnostore
;entry without storing y

;Need to set using type of swram.
;swtype is 0,2,4,6,8 etc.
    tya:pha
    ldy swtype
    lda swtypetable,Y
    sta jmpswtype+1
    lda swtypetable+1,Y
    sta jmpswtype+2
    STX &F4
    STX &FE30

.jmpswtype
    jmp &ffff

.swtype0
;Romsel based selection
    pla:tay
    rts

.swtype2
;Ramsel 
    STX &FE32
    pla:tay
    rts

.swtype4
    STA &FF30,X
    pla:tay
    rts

.swtypetable
equw swtype0
equw swtype2
equw swtype4

.crtcregs
equb &7F ; R0  horizontal total
equb 64 ; R1  horizontal displayed
equb 90 ; R2  horizontal position
equb &28 ; R3  sync width
equb &28 ; R4  vertical total
equb 0 ; R5  vertical total adjust
equb 25 ; R6  vertical displayed
equb 33 ; R7  vertical position
equb &00 ; R8  interlace
equb &7 ; R9  scanlines per row
equb &32 ; R10 cursor start
equb &0 ; R11 cursor end
equb &0c  ;R12 Screen start hi 
equb &e0 ;R13 Screen Start lo

.loadnew
    lda #lo(loadprompt)
    ldx #hi(loadprompt)
    jsr printtext

.loadkeylp2
    ldx #2
    ldy #0
    lda #129
    jsr osbyte
    bcc loadkeylp2

.loadkeylp
    ldx #255
    ldy #120
    lda #129
    jsr osbyte
    bcs loadkeylp
    cpx #'A'
    bcc loadkeylp
    cpx #'N'
    bcs loadkeylp
    txa
    jsr oswrch
    sta filename+2

    lda #lo(loadingtext)
    ldx #hi(loadingtext)
    jsr printtext       ; Print loading message 

    jsr loadfile
    jmp voxelstart

.printtext
    ; Print text - takes A,X as text loction
    ; Zero terminated
    sta printtextlp+1
    stx printtextlp+2
    ldx #0
.printtextlp
    lda &3000,x
    jsr oswrch
    inx
    cmp #0
    bne printtextlp
    rts

.loadfile
; Load and de-compress file
    lda #&30
    sta ZX0_src+1
    sta fileblock+3
    lda #&80
    sta ZX0_dst+1
    lda #0
    sta ZX0_dst
    sta ZX0_src
    sta fileblock+2
    sta fileblock+4
    sta fileblock+5
    sta fileblock+6

    lda #lo(filename)
    sta fileblock
    lda #hi(filename)
    sta fileblock+1

    ldx #lo(fileblock)
    ldy #hi(fileblock)
    lda #&ff
    jsr &ffdd           ; Osfile - Load

    ldx swrambank
    jsr swsetromnostore
;Do decompression
    include "zx02.asm" ; Includes RTS

.fileblock
equw filename
equd &1c00          ; Load address
equd 0              ; Execution Addres
equd 0
equd 0

.filename
equb "M.A",13

.loadprompt
equb 22,7,13,10,"Which Landscape (A-M)?",0
.loadingtext
equb 13,10,10,"Loading...",0

print "Space",&1c00-P%,"End",~P%,"Length ",P%-&1100

clear &1c00,8000

.squarecopy

; Detect sideways ram

    jsr swdetect
    lda swrambank
    bpl foundswram
    rts
.foundswram
    lda #lo(instructions)
    ldx #hi(instructions)
    jsr printtext
    lda #lo(instructions2)
    ldx #hi(instructions2)
    jsr printtext


    ldx #0
.squarecopylp
    lda squaresource,X
    sta &400,X
    lda squaresource+256,X
    sta &500,X
    lda squaresource+512,X
    sta &600,X
    lda squaresource+768,X
    sta &700,X
    lda colourtable,x
    sta &900,x
    dex
    bne squarecopylp

    jsr loadfile

    lda #lo(presskeytxt)
    ldx #hi(presskeytxt)
    jsr printtext

    cli

    ldx #0
    ldy #25
    lda #129
    jsr osbyte
    jmp voxelstart

.instructions
equs 22,7,10,"Voxel Landscape Demo",13,10
equs         "====================",13,10
equs "Mode 2 SWRam Version",13,10,13,10
equs "WASD : Move horizontally",13,10
equs "K/M : Move Up/Down",13,10
equs "J/N : Pitch up/Down",13,10
equs "H/B : Draw Distance Further/Closer",13,10,0
.instructions2
equs "V : Full/Half Z Resolution",13,10
equs "L : Load New Landscape",13,10,10
equs "Loading initial landscape...",0


.presskeytxt
equb "Done!",13,10,10,"Press a key to start.",0

.squaresource
FOR i, 0, 511
equb lo((i*i)/4)
NEXT    
FOR i, 0, 511
equb hi((i*i)/4)
NEXT    

; Colour table for quick access
.colourtable
; Right hand pixels
FOR i, 0, 127
IF i=0
equb %00110000  ; Blue
elif i<5
equb %00001111  ; Yellow
elif i<19
equb %00001100  ; Green
elif i<&37
equb %00000011  ;Red
elif i<&57
equb %00110011 ; Magenta
else
equb %00111111 ; White
ENDIF
NEXT    

.swdetect
    lda #0
    sta swtype
    lda #&ff
    sta swrambank
.swtypeloop

    LDX #0
.swt
    jsr swsetrom
    LDA &8008:EOR #&AA:STA &8008
    CMP &AAAA:CMP &8008:BNE notswr
    LDA #&AA:STA &8008
    STX swrambank
.notswr
    STY &F4
    STY &FE30
    CPX swrambank
    BEQ finishdetect
    INX
    CPX #15
    BNE swt
.swtestfinish
    inc swtype
    inc swtype
    lda swtype
    cmp #6
    bne swtypeloop
.finishdetect
    RTS

save "!Boot",&1100,P%,squarecopy
putfile "maps\map128x128_11.bin.zx02","M.A",&3000
putfile "maps\map128x128_9.bin.zx02","M.B",&3000
putfile "maps\map128x128_16.bin.zx02","M.C",&3000
putfile "maps\map128x128_15.bin.zx02","M.D",&3000
putfile "maps\map128x128_22.bin.zx02","M.E",&3000
putfile "maps\map128x128_21.bin.zx02","M.F",&3000
putfile "maps\map128x128_1.bin.zx02","M.G",&3000
putfile "maps\map128x128_2.bin.zx02","M.H",&3000
putfile "maps\map128x128_5.bin.zx02","M.I",&3000
putfile "maps\map128x128_6.bin.zx02","M.J",&3000
putfile "maps\map128x128_7.bin.zx02","M.K",&3000
putfile "maps\map128x128_10.bin.zx02","M.L",&3000
putfile "maps\map128x128_25.bin.zx02","M.M",&3000
