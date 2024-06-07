; Voxel Asm
; By Chris Bradburne
; Based on https://github.com/s-macke/VoxelSpace/tree/master?tab=readme-ov-file
; Stores lines to be drawn as colour and length from screen top
; Allows lines to always be drawn as whole bytes 

codestart=&1200     ; Code load address
maxpos=&2e00        ; Position of landscape data

oswrch=&ffee
osbyte=&fff4
osword=&fff1
osfile=&ffdd

yb=&50             ; Buffer for max hight drawn
lb=&a20             ; Buffer for last drawn
cb=&a40             ; Buffer for colour
yb2=&70             ; Buffer for max hight drawn
lb2=&a80             ; Buffer for last drawn
cb2=&aa0             ; Buffer for colour

storeoffset=&ac0     ; Offset of next buffer storage
storeoffset2=&ae0   

screenheight=200

scale=&0
left_x=&2
dx=&4
oy=&6          ; 2 bytes - allow 4
;datasource=&8  ; Overwritten by oy
value=&a
iscaled=&b         ; 2 bytes
horizon=&d
height=&e
colour=&f
multiplier=&10
multresult=&11  ; 2 bytes
player_x=&16
player_y=&18
drawdistance=&2a
zpointer=&2b
screenstart=&2c ; 2 bytes
drawloopvalue=&2e
maskedcolour=&2f
plotposition=&30    ; 2 Bytes
scalefactor=&32

vpos=&33
savedlength=&34
colourl=&35
colourr=&36
lengthl=&37
lengthr=&38
offsetl=&39
offsetr=&3a

linetop=&3b
linelength=&3c
plotposition2=&3d

linepos=&40
colourpos=&42
linepos2=&44
colourpos2=&46

lastvalue=&48
lastmultiplier=&49

lastheight1=&4a
lastheight2=&4b
currentheight=&4c

temp=&4d            ; 2 bytes

; Locations &50-&90 are yb buffer

size=128
screenwidth=32

sqrLow=&400
sqrHigh=&600

org codestart
guard maxpos

;Lookup data
include "voxlookup8.asm"

; Table for branch calculation - Right hand side
.branchtabler
FOR i, 0, 7
equb 3*(7-i)
NEXT    

; Table for branch calculation - Left hand side
.branchtable
FOR i, 0, 7
equb 3*i
NEXT    

.revtable
equb 8,7,6,5,4,3,2,1

.voxelstart
; Main entry point after initialisation.

; Set screen mode and background colour
    lda #22
    jsr oswrch
    lda #5
    jsr oswrch          ; Change to mode 5

    ldy #0
    ldx #224
    lda #154
    jsr osbyte          ; ULA for Mode 8

    lda #&f
    sta &360
    lda #&1
    sta &361
    lda #20
    jsr oswrch          ; Mode 8 setup.

    lda #&0c
    sta crtcregs+12     ; Reset crtc screen start
    lda #&e0
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
    lda #0:sta player_y+1
    lda #0:sta player_x:sta player_y
    sta lastheight1:sta lastheight2

    lda #223:sta height
    lda #32:sta horizon
    lda #48:sta drawdistance            ;160 mark

    lda #&67
    sta screenstart             ; Initialise screen location in memory


; Temporary line tests

    lda screenstart
    sta iscaled+1
    lda #0
    sta iscaled
    ldx #0

; Main draw procedure
.mainloop
;  Reset heigh buffer
    ldx #screenwidth-1
.resetlp
    lda #screenheight            ; 200 in lowest line (25*8)
    sta yb,x
    sta lb,x
    sta yb2,x
    sta lb2,x
    lda #0
    sta cb,x                    ; No colour
    sta cb2,x                    ; No colour
    lda #255
    sta storeoffset,X           ; Negative start.
    sta storeoffset2,X               
    dex
    bpl resetlp


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
    ; Divide by 16
    lsr a:ror dx
    lsr a:ror dx
    lsr a:ror dx
    lsr a:ror dx
    sta dx+1

    ;left_y = player_y + z
    ; Store into temp
    lda zlo,X
    asl A
    sta temp
    lda zhi,X
    rol A
    sta temp+1

    clc
    lda player_y
    adc temp            ; Not stored as we don't care about low result
    lda player_y+1
    adc temp+1

    ;      oy%=((INT(left_y/2)MOD size%)*size%)+outpic
    ; A contains Y hi
;    lsr A       ; lst_y/2 MOD side% : Size%=128
    and #127
    ldy #0
    sty temp
    lsr A
    ror temp
    lsr A
    ror temp
    sta temp+1  ; * 32 by moving right 3

    lda temp
    sta oy      ; data offset start page aligned
    lda temp+1
    clc
    adc #hi(maxpos)    ; Location of landscape data
    sta datasource+1

    ldy #0
; Shift scale down to allow 8x8 multiply
    ldx zpointer
    lda scalelo,x
    sta scale
    lda scalehi,X

    beq noshiftdown
.shiftdownlp
    lsr a
    ror scale
    iny
    cmp #0
    bne shiftdownlp
.noshiftdown
    sta scale+1
    sty scalefactor

    lda screenstart            ; Screen start
    sta iscaled+1       ; iscaled = i*8 (x*8) + screenstart
    ldx #0
    stx iscaled

    ; Initialise datastore
    stx linepos
    stx colourpos
    stx linepos2
    stx colourpos2

    lda #datastore
    sta linepos+1           ; Each data block is 32*32 bytes - &400
    lda #datastore+&4
    sta colourpos+1
    lda #datastore+&4*2
    sta linepos2+1           ;
    lda #datastore+&4*3
    sta colourpos2+1

    clc                     ; Ensure carry clear
    ;FOR I%=1 TO SW%
;    ldx #0
.columnloop
    ;      B%=(INT(left_x%DIV256)MODsize%)+oy%
    lda left_x+1
    and #63
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

    ldy scalefactor
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
    bne noshiftup 

; DIV 256 means just using multiplier+1 = length
.adddiff
    ; Add Add to horizon
    clc
    lda horizon
    adc multiplier+1                 ; Add Z
    bcc drawcarryon                        ; no Overflow 
    lda #255                          ; Set to max

.drawcarryon
    ldx temp                    ; Restore X
    cmp yb,x 
    sta multiplier+1
    bcs nodraw

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

    pha         ; Save colour
    ; Store line and colour
    inc storeoffset,X
    lda storeoffset,X
    tay

    lda cb,X    ; Get previous colour
    sta (colourpos),Y
    pla
    sta cb,x    ; Store new colour

    lda lb,x
    sec
    sbc yb,x        ; Calculate line length
    sta (linepos),y

    lda yb,x
    sta lb,x                    ; Store new start position

.nodrawthistime
    lda multiplier+1
    sta yb,X        ; Store new max height

;      IF L% < yb%?I% THEN PROCdraw_vline(I%, L%, yb%?I%):yb%?I%=L%
.nodraw
; Now do odd value
    lda value           ; Load currnt value and store
    tay                
    clc
    adc lastvalue           ; Add to previous value
    sty lastvalue       ; Store current to last
    ror A               ; Average
    sta value           ; Store to new value

    lda multiplier+1    ; Current top position
    tay
    clc
    adc lastmultiplier  ; Last top position
    sty lastmultiplier  ; Store current to last
    ror a
    sta multiplier+1

    cmp yb2,x 
    bcs nodraw2

    lda cb2,x
    bne notfirstcolour2                ; Not first colour
    ldy value
    lda &980,y
    sta cb2,X
    bne nodrawthistime2              ; Always taken 

.notfirstcolour2
    ldy value
    lda &980,y
    cmp cb2,X
    beq nodrawthistime2 ; Colour is same as previous line - don't draw yet

    pha         ; Save colour
    ; Store line and colour
    inc storeoffset2,X
    lda storeoffset2,X
    tay

    lda cb2,X    ; Get previous colour
    sta (colourpos2),Y          ; Store
    pla
    sta cb2,x    ; Store new colour

    lda lb2,x
    sec
    sbc yb2,x
    sta (linepos2),y

    lda yb2,x
    sta lb2,x                    ; Store new start position


.nodrawthistime2
    lda multiplier+1
    sta yb2,X        ; Store new max height

;      IF L% < yb%?I% THEN PROCdraw_vline(I%, L%, yb%?I%):yb%?I%=L%
.nodraw2

    lda linepos
    clc
    adc #32
    sta linepos
    sta colourpos           ; Will be same offset
    sta linepos2
    sta colourpos2           ; Will be same offset
    bcc nolcinc
    inc linepos+1
    inc colourpos+1
    inc linepos2+1
    inc colourpos2+1
    clc
.nolcinc

;    clc
    lda left_x
    adc dx
    sta left_x
    lda left_x+1
    adc dx+1
    sta left_x+1        

    inx
    cpx #32             ; Screen width
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
    ; now do final cyan sky
    ldx #0

    stx linepos
    stx colourpos
    stx linepos2
    stx colourpos2

    lda #datastore
    sta linepos+1           ; Each data block is 32*32 bytes - &400
    lda #datastore+&4
    sta colourpos+1
    lda #datastore+&4*2
    sta linepos2+1           ;
    lda #datastore+&4*3
    sta colourpos2+1

    lda #screenheight
    sta currentheight

; Go through and add any partial final lines then the sky lines
.skyfillloop    
    lda storeoffset,x
    tay                 ; Use Y as storeoffset

    lda lb,x
    sec
    sbc yb,x
    beq notopline
    iny                 ; Point to next position
    
    sta (linepos),y
    lda cb,X
    sta (colourpos),Y   ; Store top line

.notopline
    lda yb2,x
    cmp currentheight           ; Check if this is the highest point
    bcs skipupdatecurrent1
    sta currentheight           ; Update the highest point
.skipupdatecurrent1
    lda yb,x
    cmp currentheight           ; Check if the odd pixel is the highest point
    bcs skipupdatecurrent
    sta currentheight           ; update highest pizxel
.skipupdatecurrent
    lda currentheight           ; check if current heght less than previous
    cmp lastheight2
    bcs skipcurrenttolast
    sta lastheight2             ; Update previous
.skipcurrenttolast
    lda yb,X
    cmp lastheight2 
    bcc nofinalsky      ; Skip if less than last height
    beq nofinalsky      ; Or no sky

    iny
;    lda yb,X
    sec
    sbc lastheight2
    sta (linepos),Y     ; Write sky size
    lda #%00010100      ; Cyan right pixel
    sta (colourpos),y

    lda yb,X            ; This is top position
    bcc nofinalskyb                ; Always taken

.nofinalsky
    lda lastheight2
.nofinalskyb
    iny
    sta (linepos),Y     ; Store the remainder of the sky as a skip value
;No need to store colour

.noblankspace
    tya
    sta storeoffset,x

    ; Now do odd column

    lda storeoffset2,x
    tay                 ; Use Y as storeoffset

    lda lb2,x
    sec
    sbc yb2,x
    beq notopline2      ; No last line to plot
    
    iny                 ; Point to next position
    sta (linepos2),y
    lda cb2,X
    sta (colourpos2),Y  ; Store top linie colour

.notopline2
    lda yb2,X
    cmp lastheight2
    bcc nofinalsky2
    beq nofinalsky2
    iny                 ; Point to next position
    sec
    sbc lastheight2
    sta (linepos2),Y     ; Write sky size
    lda #%00101000      ; Cyan left pixel
    sta (colourpos2),y

.nofinalsky2
; No need to put a top secion as both the same
    tya
    sta storeoffset2,x  ; Store the position of last data

; Update pointers

    lda linepos
    clc
    adc #32
    sta linepos
    sta colourpos           ; Will be same offset
    sta linepos2
    sta colourpos2           ; Will be same offset
    bcc nolcinc2
    inc linepos+1
    inc colourpos+1
    inc linepos2+1
    inc colourpos2+1
.nolcinc2

    inx
    cpx #screenwidth
    beq skyfillloopend      ; Loop for screen width
    jmp skyfillloop
.skyfillloopend

    lda lastheight1         ; Update values
    sta lastheight2
    lda currentheight
    sta lastheight1

    ; Write zero to left column
    lda #screenheight
    sta 256*(datastore+&4*2)
    lda #0
    sta 256*(datastore+&4*3)        ; Black
    sta storeoffset2            ; 1 line.

    ldx #0
    stx iscaled
    stx plotposition
    lda screenstart
    sta iscaled+1           ; Pointer for plot
    sta plotposition+1

    stx linepos
    stx colourpos
    stx linepos2
    stx colourpos2

    lda #datastore
    sta linepos+1           ; Each data block is 32*32 bytes - &400
    lda #datastore+&4
    sta colourpos+1
    lda #datastore+&4*2
    sta linepos2+1           
    lda #datastore+&4*3
    sta colourpos2+1

.drawloop
    ; Draw final lines
    lda storeoffset,x   ; Get skip amount
    tay
    lda (linepos),y
    sta vpos            ; Store start position

    lsr a:lsr a:lsr a         ; Convert to 256*lines
    clc                    ;  necessary because bottom bits not cleared
    adc iscaled+1
    sta plotposition+1      ; Store initial plot position.

    ; Get first line lengths
    dey                     ; Move to actual data
    sty offsetr             ; Store for easy reference
    lda (linepos),Y
    sta lengthr             ; Store current right length
    lda (colourpos),Y
    sta colourr             ; Store current right colour

    lda storeoffset2,X
    sta offsetl             ; Store left for easy reference
    tay
    lda (colourpos2),Y
    sta colourl             ; Store current left colour
    lda (linepos2),Y

.drawinnerloop
    sta lengthl             ; Store current left length

    lda colourr
    ora colourl             ; Combine pixels
    sta colour      ; Write colours to be drawn

    lda vpos
    and #7
    sta linetop             ; Store offset in byte

    ; Work out which line is shorter
    lda lengthl
    cmp lengthr
    bcc leftshorter
;Right is shorter
    lda lengthr    
.leftshorter
    sta linelength          ; Line length to draw
    sta savedlength         ; Store for later

    jsr drawline

    lda vpos                ; Get current vertical position
    clc
    adc savedlength         ; Add previous length
    cmp #screenheight                    ; Finished column?
    bcs finishedinner       
    sta vpos

    lda lengthr             ; Reduce right length
    sec
    sbc savedlength
    bne nonnewright             ; Have we drawn all of this line segment

    dec offsetr
    ldy offsetr                 ; Get new line data
    lda (colourpos),Y
    sta colourr
    lda (linepos),Y
.nonnewright
    sta lengthr

    lda lengthl
    sec
    sbc savedlength         ; Reduce left length
    bne drawinnerloop       ; Have we drawn all line segment?

    dec offsetl
    ldy offsetl             ; Get ne left data
    lda (colourpos2),Y
    sta colourl
    lda (linepos2),Y
    bne drawinnerloop       ; Always taken

.finishedinner

; Update pointers

    lda linepos
    clc
    adc #32
    sta linepos
    sta colourpos           ; Will be same offset
    sta linepos2
    sta colourpos2           ; Will be same offset
    bcc nolcinc3
    inc linepos+1
    inc colourpos+1
    inc linepos2+1
    inc colourpos2+1
    clc
.nolcinc3

    lda iscaled             ; Update horizontal start position
    adc #8
    sta iscaled
    sta plotposition
    ; No need to update vertical position

    inx
    cpx #screenwidth
    beq finishdraw
    jmp drawloop

.finishdraw
print "endframe",~P%
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
    eor #&29            ; Change between &4e &1c
    sta screenstart
    lda crtcregs+12
    eor #&05             ; Swap between 09 and 03
    sta crtcregs+12
    lda crtcregs+13
    eor #&20            ; Swap between &C and &9
    sta crtcregs+13
.skipcrtc

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

.drawline
    ; Takes Multiplier as line length
    ; Multiplier+1 as start location
    ; colour as colour 
    ; iscaled as start location

    ; Draw first group of 8
    ; Work out if there are less bytes than a single group
    lda linelength
    cmp #8
    bcs dlmorethantop    ; More than 8 length so must be to end of first group

    lda linetop
    adc linelength
    cmp #8              ; More than 8 so is to end of first group
    bcs dlmorethantop
    ; Not more than top

    ldy linelength
    lda branchtabler,y
    sta topbranch+1     ; Store as branch
    ldy linetop

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

    ldy linetop
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

    ldy linetop
    lda linelength  
    sec
    sbc revtable,y
    sta linelength      ; Reduce by number we just plotted.
    bne dlcarryon
    inc plotposition+1
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
    iny
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

    inc plotposition+1  ; Increment screen position

    ldy linelength      ; Remainder
    beq dlfinished
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
;    lda #19
;    jsr osbyte
;    pla:tax
;    rts

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

.crtcregs
equb &3F ; R0  horizontal total
equb 32 ; R1  horizontal displayed
equb 45 ; R2  horizontal position
equb &24 ; R3  sync width
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
    lda &Ec
    bne loadkeylp2
    lda &Ed
    bne loadkeylp2

    ; Clear buffers
    ldx #0
    lda #15
    jsr osbyte

.loadkeylp
    ldx #255
    ldy #120
    lda #129
    jsr osbyte          ; inkey
    bcs loadkeylp
    cpx #'A'
    bcc loadkeylp
    cpx #'N'
    bcs loadkeylp       ; Chek range
    txa
    jsr oswrch          ; Print character
    sta filename+2      ; Store in filename

    lda #lo(loadingtext)
    ldx #hi(loadingtext)
    jsr printtext       ; Print loading message 

    jsr loadfile        ; Load file and decompress
    jmp voxelstart      ; Go to start

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
    lda #hi(maxpos)
    sta fileblock+3
    lda #0
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
    jmp &ffdd           ; Osfile - Load

.filename
equb "M.A",13
.fileblock
equw filename
equd &4e00          ; Load address
equd 0              ; Execution Addres
equd 0
equd 0

.loadprompt
equb 22,7,13,10,"Which Landscape (A-M)?",0
.loadingtext
equb 13,10,10,"Loading...",0

print "Space",maxpos-P%,"End",~P%,"Length ",P%-codestart

datastore=hi(P%)+1

print "Data storage from",~datastore*256

clear maxpos,&8000

.entrypoint
    ldx #1
    ldy #0
    lda #200
    jsr osbyte

    lda #lo(instructions)
    ldx #hi(instructions)
    jsr printtext

    jsr loadfile

    lda #lo(presskeytxt)
    ldx #hi(presskeytxt)
    jsr printtext

    ldx #0
.sqcopy
    lda sqrLows,x
    sta sqrLow,X
    lda sqrHighs,X
    sta sqrHigh,X
    lda sqrLows+256,x
    sta sqrLow+256,X
    lda sqrHighs+256,X
    sta sqrHigh+256,X
    lda colourtable,x
    sta &900,x
    dex
    bne sqcopy

    cli

    ldx #0
    ldy #25
    lda #129
    jsr osbyte

    jmp voxelstart

.instructions
equs 22,7,10,"Voxel Landscape Demo",13,10
equs      "====================",13,10
equs "Mode 8 32K Version",13,10,13,10
equs "WASD : Move Horizontally",13,10
equs "K/M : Move Up/Down",13,10
equs "J/N : Pitch Up/Down",13,10
equs "H/B : Draw Distance Further/Closer",13,10
equs "V : Full/Half Z Resolution",13,10
equs "L : Load New Landscape",13,10,10
equs "Loading initial landscape...",0
.presskeytxt
equb "Done!",13,10,10,"Press a key to start.",0

; Square table for multiply 
.sqrLows
FOR i, 0, 511
equb lo((i*i)/4)
NEXT    
.sqrHighs
FOR i, 0, 511
equb hi((i*i)/4)
NEXT    

; Colour table for quick access
.colourtable
; Right hand pixels
FOR i, 0, 127
IF i=0
equb %00010000  ; Blue
elif i<5
equb %00000101  ; Yellow
elif i<19
equb %00000100  ; Green
elif i<&37
equb %00000001  ;Red
elif i<&57
equb %00010001 ; Magenta
else
equb %00010101 ; White
ENDIF
NEXT    

; Left hand pixels
FOR i, 0, 127
IF i=0
equb %00100000  ; Blue
elif i<5
equb %00001010  ; Yellow
elif i<19
equb %00001000  ; Green
elif i<&37
equb %00000010  ;Red
elif i<&57
equb %00100010 ; Magenta
else
equb %00101010 ; White
ENDIF
NEXT    


save "!Boot",codestart,P%,entrypoint
putfile "maps/map64x128_11.bin","M.A",&3000
putfile "maps/map64x128_9.bin","M.B",&3000
putfile "maps/map64x128_16.bin","M.C",&3000
putfile "maps/map64x128_15.bin","M.D",&3000
putfile "maps/map64x128_22.bin","M.E",&3000
putfile "maps/map64x128_21.bin","M.F",&3000
putfile "maps/map64x128_1.bin","M.G",&3000
putfile "maps/map64x128_2.bin","M.H",&3000
putfile "maps/map64x128_5.bin","M.I",&3000
putfile "maps/map64x128_6.bin","M.J",&3000
putfile "maps/map64x128_7.bin","M.K",&3000
putfile "maps/map64x128_10.bin","M.L",&3000
putfile "maps/map64x128_25.bin","M.M",&3000