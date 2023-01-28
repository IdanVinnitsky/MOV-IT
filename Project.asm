IDEAL

MACRO NEW_LINE
	mov dl,13   ; CR = Caridge Return - go to row start position
	mov ah,2   
	int 21h
	mov dl,10   ;  LF = Line Feed - go down to the next line
	int 21h
ENDM LINE

MODEL small
STACK 256
DATASEG
 
	RndCurrentPos dw start
	Score db 13,"Score : ",'$'
	Score1 db 13,"Your score : ",'$'
	Number db '0','0','$'
	SquareY dw 0
	SquareX dw 155
	Color db ?
	count db 0 
;================================
filename1 db 'start.bmp',0

filename2 db 'end.bmp',0

filerules db 'rules.bmp',0

filehandle dw ?

Header db 54 dup (0)

Palette db 256*4 dup (0)

ScrLine db 320 dup (0)

ErrorMsg db 'Error', 13, 10,'$'
;================================	
CODESEG
    ORG 100h
start:
	mov ax, @data
	mov ds,ax
	
    ; here your code
begining:
	call Graphic_mode
	
	mov dx, offset filename1 ; קריאה למסך פתיחה
	call Process_BMP_file
	
	call cursor1 
	
	call rules1
	
	call Continue1	
	
	
NewGame:
	mov dx,offset filename2 ; מסך סיום
	call Process_BMP_file
	call EndPage
	call Cursor2
	
	
EXIT:
    call Text_mode
	mov ax, 4C00h ; returns control to dos
  	int 21h
	

  
;---------------------------
; Procudures area
;---------------------------
proc Continue1
	;איפוס
	mov [Number],'0'
	mov [Number+1],'0'
	mov [count],0
	mov [SquareY],0
	mov [SquareX],155
	
	call Graphic_mode

	call animation

	ret
endp Continue1
	
proc rules1
	push ax
	push dx
	
	mov ax,2 ; הסתרת עכבר
	int 33h
	
	RulesPage:
	mov dx, offset filerules  ;rules
	call Process_BMP_file
	
	mov ah,0
	int 16h
	
	cmp ah,10h	
	je begining
	
	jmp RulesPage
	
	pop dx
	pop ax
	ret
endp rules1

proc Cursor1
	push ax
	push bx
	push cx
	push dx
	
	mov ax,0   ; resets mouse cursor
	int 33h   
	mov ax, 1   ;shows mouse cursor
	int 33h
	
	Next:
	mov ax, 3   ;get cursor positon in cx,dx
	int 33h
	
	cmp bx,01h
	jne Next
	
	shr cx,1
	cmp cx,100
	jb Next
	cmp cx,200
	ja Next
	cmp dx,160
	jb Next
	cmp dx,180
	ja Next
	cmp cx,140
	jbe moveToGame
	ja RulesPage

moveToGame:
	call Continue1
	
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp Cursor1

proc Cursor2
	push ax
	push bx
	push cx
	push dx
	
	mov ax,0   ; resets mouse cursor
	int 33h   
	mov ax, 1   ;shows mouse cursor
	int 33h
	
	Next2:
	mov ax, 3   ;get cursor positon in cx,dx
	int 33h
	
	cmp bx,01h
	jne Next2
	
	shr cx,1
	cmp cx,100
	jb Next2
	cmp cx,200
	ja Next2
	cmp dx,160
	jb Next2
	cmp dx,180
	ja Next2
	cmp cx,140
	jbe returnToGame
	ja ExitGame
	
returnToGame:
		call Continue1
ExitGame:
	call Text_mode
	mov ax, 4C00h ; returns control to dos
  	int 21h
	
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp Cursor2
	
;פעולה הבודקת היכן נמצא הריבוע ואם הוא מחוץ לגבולות אתה ;נפסל	
proc Error1 
	push ax
	push bx
	push cx 
	push dx 
	
	cmp al,10
	je ErrorG
	
	cmp al,11
	je ErrorC
	
	cmp al,12
	je ErrorR
	
	cmp al,14
	je ErrorY
	
ErrorG:	
	cmp [SquareX],7
	jbe exit2
	cmp [SquareY],185
	jae exit2
	jmp ContinueGame1
	
ErrorC:
	cmp [SquareX],305
	jae exit2
	cmp [SquareX],7
	jbe exit2
	
	cmp [SquareY],180
	jae Chek2
	jmp ContinueGame1
	
Chek2:
	cmp [SquareX],160
	ja exit2
	jmp ContinueGame1
	
ErrorR:
	cmp [SquareX],305
	jae exit2
	cmp [SquareX],7
	jbe exit2
	
	cmp [SquareY],180
	jae Chek3
	jmp ContinueGame1
	
Chek3:
	cmp [SquareX],160
	jb exit2
	jmp ContinueGame1	
	
ErrorY:
	cmp [SquareX],305
	jae exit2
	cmp [SquareY],185
	jae exit2
	jmp ContinueGame1		
	
exit2:
	jmp NewGame
	

ContinueGame1:
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp Error1

proc EndPage
	push ax
	push bx
	push cx 
	push dx 
	
	mov dx, offset Score1
	mov ah,9
	int 21h
	
	NEW_LINE
	
	mov dx, offset Number
	mov ah,9
	int 21h



	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp EndPage


;--------------------------------------
;הפעולה הראשית - game play
;--------------------------------------
proc animation 
	push ax
	push bx
	push cx
	push dx
	

	call RandomColor
	
Draw_squre1:

	call background
	call caption
	
	
    ;cx = col dx= row al = color si = height di = width 
	mov al,[Color]
	
	call Cheking
	call Error1
	
	mov cx,[SquareX]
	mov dx,[SquareY]
	mov si,10
	mov di,10
	call Rect
	
	call WaitSecs
	
	mov al,0
	mov cx,[SquareX]
	mov dx,[SquareY]
	mov si,10
	mov di,10
	call Rect
	
	
	call Move	
	
	call harder  
  
	jmp Draw_squre1
	
	
	pop dx
	pop cx	
	pop bx
	pop ax
	ret 
endp animation
;--------------------------------------
;סיום פעולה ראשית
;--------------------------------------

;כל חמש נקודות מורידה את הריבוע בפיקסל יותר
proc harder

	cmp [count],5
	jbe easy
	
	cmp [count],10
	jbe hard
	
	cmp [count],15
	jbe dificult
	
easy:
	add [SquareY],2
	jmp Cun
	
hard:
	add [SquareY],3
	jmp Cun
	
dificult:
	add [SquareY],4
	jmp Cun
	
	
Cun:
	
	
	ret
endp harder


;הפעולה המאפסת את הריבוע
;דואגת על הניקוד
proc Cheking 
	push ax
	push bx
	push cx 
	push dx 
	
	cmp al,10
	je chekG
	
	cmp al,11
	je chekC
	
	cmp al,12
	je chekR
	
	cmp al,14
	je chekY
	
chekG:	
	cmp [SquareX],305
	jae zeroing
	jmp ContinueGame
	
chekC:
	cmp [SquareY],185
	jae zeroing
	jmp ContinueGame
	
chekR:
	cmp [SquareY],185
	jae zeroing
	jmp ContinueGame	
	
chekY:
	cmp [SquareX],5
	jbe zeroing
	jmp ContinueGame		
	

zeroing:
	mov [SquareY],0
	mov [SquareX],155
	call RandomColor
	inc [Number+1]
	inc [count]
	cmp [Number+1],'9'
	ja AddTen
	jmp ContinueGame
	AddTen:
		inc [Number]
		mov [Number+1],'0'

	
ContinueGame:
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp Cheking
	


proc RandomColor
	push ax
	push bx
	push cx 
	push dx 
	
	mov bl,10
	mov bh,14	 
	call RandomByCs
	
	cmp al,13
	je decrease
	jne continue
	
	decrease:
		dec al
	
	continue:
		mov [Color],al	
	    pop dx
		pop cx
		pop bx
		pop ax
	ret
endp RandomColor

proc Move
	push ax
	
	mov ah,1
	int 16h
	jz ending
	mov ah,0
	int 16h
	
	cmp ah,04Dh	
	je Right
	
	cmp ah,04Bh
	je Left
	
	cmp ah,050h
	je Up
	
	cmp ah,048h
	je Down
	
	cmp ah,1h
	je exit1
	
	jmp ending
	
exit1:
	call Text_mode
	mov ax, 4C00h ; returns control to dos
  	int 21h	
	
	jmp ending
	
Right:
	add [SquareX],5
	jmp ending
	
Left:
	sub [SquareX],5
	jmp ending
	
Up:
	add [SquareY],5
	jmp ending
	
Down:
	sub [SquareY],5
	
ending:
	pop ax
	ret
endp Move





proc WaitSecs
	push ax
	push cx
	push dx
	
	mov ah, 86h
	mov cx, 1h
	mov dx, 10h
	int 15h 
	
	pop dx 
	pop cx 
	pop ax
	ret
endp WaitSecs

;------------------------------------------------------------

; Description  : get RND between any bl and bh includs (max 0 -255)
; Input        : 1. Bl = min (from 0) , BH , Max (till 255)
; 			     2. RndCurrentPos a  word variable,   help to get good rnd number
; 				 	Declre it at DATASEG :  RndCurrentPos dw ,0
;				 3. EndOfCsLbl: is label at the end of the program one line above END start		
; Output:        Al - rnd num from bl to bh  (example 50 - 150)
; More Info:
; 	Bl must be less than Bh 
; 	in order to get good random value again and agin the Code segment size should be 
; 	at least the number of times the procedure called at the same second ... 
; 	for example - if you call to this proc 50 times at the same second  - 
; 	Make sure the cs size is 50 bytes or more 
; 	(if not, make it to be more) 
proc RandomByCs
    push es
	push si
	push di
	
	mov ax, 40h
	mov	es, ax
	
	sub bh,bl  ; we will make rnd number between 0 to the delta between bl and bh
			   ; Now bh holds only the delta
	cmp bh,0
	jz @@ExitP
 
	mov di, [word RndCurrentPos]
	call MakeMask ; will put in si the right mask according the delta (bh) (example for 28 will put 31)
	
RandLoop: ;  generate random number 
	mov ax, [es:06ch] ; read timer counter
	mov ah, [byte cs:di] ; read one byte from memory (from semi random byte at cs)
	xor al, ah ; xor memory and counter
	
	; Now inc di in order to get a different number next time
	inc di
	cmp di,(EndOfCsLbl - start - 1)
	jb @@Continue
	mov di, offset start
@@Continue:
	mov [word RndCurrentPos], di
	
	and ax, si ; filter result between 0 and si (the mask)
	cmp al,bh    ;do again if  above the delta
	ja RandLoop
	
	add al,bl  ; add the lower limit to the rnd num
		 
@@ExitP:	
	pop di
	pop si
	pop es
	ret
endp RandomByCs


; make mask acording to bh size 
; output Si = mask put 1 in all bh range
; example  if bh 4 or 5 or 6 or 7 si will be 7
; 		   if Bh 64 till 127 si will be 127
Proc MakeMask    
    push bx

	mov si,1
    
@@again:
	shr bh,1
	cmp bh,0
	jz @@EndProc
	
	shl si,1 ; add 1 to si at right
	inc si
	
	jmp @@again
	
@@EndProc:
    pop bx
	ret
endp  MakeMask

;-----------------------------------------------------------


;================================
proc Process_BMP_file
	call OpenFile
    call ReadHeader
    call ReadPalette
    call CopyPal
    call CopyBitmap

	ret
endp Process_BMP_file

proc OpenFile

    ; Open file

    mov ah, 3Dh
    xor al, al
    ;mov dx, offset filename
    int 21h

    jc openerror
    mov [filehandle], ax
    ret

    openerror:
    mov dx, offset ErrorMsg
    mov ah, 9h
    int 21h
    ret
endp OpenFile

proc ReadHeader

    ; Read BMP file header, 54 bytes

    mov ah,3fh
    mov bx, [filehandle]
    mov cx,54
    mov dx,offset Header
    int 21h
    ret
endp ReadHeader

proc ReadPalette

    ; Read BMP file color palette, 256 colors * 4 bytes (400h)

    mov ah,3fh
    mov cx,400h
    mov dx,offset Palette
    int 21h
    ret
endp ReadPalette

proc CopyPal

    ; Copy the colors palette to the video memory
    ; The number of the first color should be sent to port 3C8h
    ; The palette is sent to port 3C9h

    mov si,offset Palette
    mov cx,256
    mov dx,3C8h
    mov al,0

    ; Copy starting color to port 3C8h

    out dx,al

    ; Copy palette itself to port 3C9h

    inc dx
    PalLoop:

    ; Note: Colors in a BMP file are saved as BGR values rather than RGB.

    mov al,[si+2] ; Get red value.
    shr al,2 ; Max. is 255, but video palette maximal

    ; value is 63. Therefore dividing by 4.

    out dx,al ; Send it.
    mov al,[si+1] ; Get green value.
    shr al,2
    out dx,al ; Send it.
    mov al,[si] ; Get blue value.
    shr al,2
    out dx,al ; Send it.
    add si,4 ; Point to next color.

    ; (There is a null chr. after every color.)

    loop PalLoop
    ret
endp CopyPal

proc CopyBitmap

    ; BMP graphics are saved upside-down.
    ; Read the graphic line by line (200 lines in VGA format),
    ; displaying the lines from bottom to top.

    mov ax, 0A000h
    mov es, ax
    mov cx,200
    PrintBMPLoop:
    push cx

    ; di = cx*320, point to the correct screen line
w
    mov di,cx
    shl cx,6
    shl di,8
    add di,cx

    ; Read one line

    mov ah,3fh
    mov cx,320
    mov dx,offset ScrLine
    int 21h

    ; Copy one line into video memory

    cld 

    ; Clear direction flag, for movsb

    mov cx,320
    mov si,offset ScrLine
    rep movsb 

    ; Copy line to the screen
    ;rep movsb is same as the following code:
    ;mov es:di, ds:si
    ;inc si
    ;inc di
    ;dec cx
    ;loop until cx=0

    pop cx
    loop PrintBMPLoop
    ret
endp CopyBitmap
;================================


proc caption
	push ax
	push bx
	push dx
	
	mov bh,0
	mov dh,0
	mov dl,0
	mov ah,2h
	int 10h
	
	
	mov dx, offset Score
	mov ah,9
	int 21h
	
	NEW_LINE
	
	mov dx, offset Number
	mov ah,9
	int 21h
	
	mov bh,0
	mov dh,1
	mov dl,0
	mov ah,2h
	int 10h
	
	
	pop dx 
	pop bx
	pop ax
	ret
endp caption

proc background
	push ax
	push bx
	push cx
	push dx
	
; cx = col dx= row al = color si = height di = width 
	
;Yellow Rect
	mov al,14
	mov cx,0
	mov dx,20
	mov si,170
	mov di,10
	call Rect
	
;Light Green Rect	
	mov al,10
	mov cx,310
	mov dx,0
	mov si,190
	mov di,10
	call Rect
	
;Light Cyan Rect	
	mov al,11
	mov cx,0
	mov dx,190
	mov si,10
	mov di,160
	call Rect
	
;Light Red Rect	
	mov al,12
	mov cx,160
	mov dx,190
	mov si,10
	mov di,160
	call Rect
	
	
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp background 

;--------------------------------------
;Procedures for drawing a rectangle:
;-------------------------------------- 
proc DrawHorizontalLine	near
	push si
	push cx
DrawLine:
	cmp si,0
	jz ExitDrawLine	
	 
    mov ah,0ch	
	int 10h    ; put pixel
	 
	
	inc cx
	dec si
	jmp DrawLine
	
	
ExitDrawLine:
	pop cx
    pop si
	ret
endp DrawHorizontalLine



proc DrawVerticalLine	near
	push si
	push dx
 
DrawVertical:
	cmp si,0
	jz @@ExitDrawLine	
	 
    mov ah,0ch	
	int 10h    ; put pixel
	
	 
	
	inc dx
	dec si
	jmp DrawVertical
	
	
@@ExitDrawLine:
	pop dx
    pop si
	ret
endp DrawVerticalLine


; cx = col dx= row al = color si = height di = width 
proc Rect
	push cx
	push di
NextVerticalLine:	
	
	cmp di,0
	jz @@EndRect
	
	cmp si,0
	jz @@EndRect
	call DrawVerticalLine
	inc cx
	dec di
	jmp NextVerticalLine
	
	
@@EndRect:
	pop di
	pop cx
	ret
endp Rect
;--------------------------------------
;
;--------------------------------------

proc Wait_for_key_press
	push ax
	mov ah,00h
	int 16h
	pop ax
	ret
endp Wait_for_key_press

proc Graphic_mode
	push ax
	mov ax, 13h
	int 10h
	pop ax
	ret
endp Graphic_mode
	
proc Text_mode
	push ax
	mov ah, 0
	mov al, 2
	int 10h
	pop ax
	ret
endp Text_mode



EndOfCsLbl:
END start