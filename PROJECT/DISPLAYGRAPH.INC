;//DISPLAY STRING IN GRAPHICS MODE
DISPLAY_GRAPH MACRO MESS,SIZE,ROW,COL
    MOV AX,@DATA
    MOV ES,AX
	
    mov al, 1                  ;WRITE MODE=1
	mov bh, 0                  ;PAGE NO
	mov bl, 06H                ;color
	mov cL, SIZE               ;message size.
	mov dh, ROW                ;Row
	mov dl, COL                ;COL
	mov bp, offset MESS        ;OFFSET MESS
	mov ah, 13h
	int 10h

ENDM DISPLAY_GRAPH