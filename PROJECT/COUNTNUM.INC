;//RETURN COUNT AND NUMBER 
;///send MESSAGE+INDEX 
;// DX=NUMBER  CX=COUNT OF NUMBERS IN NUMBER
RET_NUM_COUNT MACRO MESSAGE 
    LOCAL SOURCELOOP,COUNTSOURCE,LABEL1,LABEL2,LABEL3,LABEL4,ISNUMBER,ISCHAR,FINISH,OUUT
    LEA DI,MESSAGE
    MOV CX,0
    SOURCELOOP: 
    MOV AL,[DI]
    CMP AL,24H
    JNZ COUNTSOURCE
    JZ LABEL1
    cmp al,93H
	JNZ COUNTSOURCE
    JZ LABEL1
	
    COUNTSOURCE:
    INC CX
    INC DI
    JMP SOURCELOOP
    
    LABEL1:
    LEA DI,MESSAGE
    push cx
	pop bp
    MOV AX,CX
    MOV CX,4
    SUB CX,AX
    CMP CX,0
    JZ LABEL2
    
    LABEL3:
    MOV BX,0
    PUSH BX
    LOOP LABEL3
    
    LABEL2:
    MOV CX,AX
    mov dx,ax
    
    LABEL4:
    MOV AL,[DI]
    CMP AL,61H
    JB ISNUMBER
    
    ISCHAR:
    SUB AL,57H
    MOV AH,0
    PUSH AX
    JMP FINISH
    
    ISNUMBER:
    SUB AL,30H
    MOV AH,0
    PUSH AX
    
    FINISH:
    INC DI
    LOOP LABEL4
    
    mov cx,dx
    mov dx,0
    pop dx
    dec cx  ;;; in case 1 digit only
    cmp cx,0
    jz ouut
    
    MOV AX,0
    
    POP AX
    MOV BL,10H
    MUL BL
    ADD DX,AX
    DEC CX
    CMP CX,0
    JZ OUUT
    
    MOV AX,0
    POP AX
    MOV BX,100H
    PUSH DX
    MUL BX
    POP DX
    ADD DX,AX
    DEC CX
    CMP CX,0
    JZ OUUT
    
    MOV AX,0
    POP AX
    MOV BX,1000H
    PUSH DX
    MUL BX 
    POP DX
    ADD DX,AX
    DEC CX
	OUUT:
    MOV CX,bp   
ENDM RET_NUM_COUNT


