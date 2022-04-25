;//GAME TEXT MODE INTIAL MODE
GAME_WIND MACRO
LOCAL CHECK,L1,L2,CHECK_KEY
    CHECK:
	MOV AH,1
	INT 16H
	JZ CHECK                             ;7ATETHA HENA 3SHAN COMPARE CHECK LAZEM YEKON BA3D INTRUPT 
	
	CHECK_KEY:
	CMP AL,31H
	JZ L1
	CMP AL,32H
	JZ L2
	mov ah,0                             ;CONSUME BUFFER
    int 16h
	JMP CHECK
	
	L1:
	MOV AH,7                             ;CONSUME BUFFER
	INT 21H
	JMP LEVEL1                           ;JMP TO CHAT MODE
	
	
	L2:
	MOV AH,7                              ;CONSUME BUFFER
	INT 21H
	JMP LEVEL2                           ;JMP TO FINISH
	
ENDM GAME_WIND