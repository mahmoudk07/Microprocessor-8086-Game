;//GAME TEXT MODE INTIAL MODE
GAME_WIND MACRO GAMELEVEL
LOCAL CHECK1,L1,L2,ITSLEVEL1
    LEA SI,GAMELEVEL
    CHECK1:
	MOV AH,1
	INT 16H
	JZ CHECK1                             ;7ATETHA HENA 3SHAN COMPARE CHECK LAZEM YEKON BA3D INTRUPT 
	
	;CHECK KEY IS VALID KEY
	CMP AL,'1'
	JZ L1          
	
	CMP AL,'2'
	JZ L2
	mov ah,0                             ;CONSUME BUFFER
    int 16h
	JMP CHECK1
	
	L1:
	MOV AH,0                            ;CONSUME BUFFER
	INT 16H
	MOV [SI],1                          ;INTIALIZE LEVEL BY 1
	JMP ITSLEVEL1                       ;JMP TO LEVEL 1 
	
	
	L2:
	MOV AH,0                              ;CONSUME BUFFER
	INT 16H
	MOV [SI],2                            ;INTIALIZE LEVEL BY 2
	
	ITSLEVEL1:
	
ENDM GAME_WIND