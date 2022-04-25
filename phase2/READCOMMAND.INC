;//READ FROM command from user
READ_COMMAND MACRO COM_MESS,A_SIZE,COL
    LOCAL READING,EXIT1,NOT_EXIT,BACKSPACE

    LEA DI,COM_MESS
	LEA SI,A_SIZE
	MOV [SI],0
    MOV DL,COL
	READING:
	MOV AH,1
	INT 16H
	JZ READING                         ;7ATETHA HENA 3SHAN COMPARE CHECK LAZEM YEKON BA3D INTRUPT 
	;//if backspace
	CMP AL,8
	JZ BACKSPACE
	;//IF F4
	CMP AH,3EH
	JZ EXIT1 
	;//IF ENTER KEY
	CMP AH,1CH
	JZ NOT_EXIT
	
	MOV [DI],AL
	MOVE_CURSOR 21,DL
	MOV BL,06H   
	MOV BH,BL
    MOV CL,4
    SHR BH,CL
    mov ah,9
    MOV CX,1
    int 10h 
	mov ah,0                             ;CONSUME BUFFER
    int 16h
	INC DL
	INC DI
	;ACTUAL SIZE
	MOV CL,[SI]
	INC CL
	MOV [SI],CL
	JMP READING
	
	BACKSPACE:
	mov ah,0                             ;CONSUME BUFFER
    int 16h
	DEC DL
	MOVE_CURSOR 21,DL
    MOV BL,00H
	MOV BH,BL
    MOV CL,4
    SHR BH,CL
    mov ah,9
    MOV CX,1
    int 10h 
	DEC DI
	MOV [DI],'$'
	;ACTUAL SIZE
	MOV CL,[SI]
	DEC CL
	MOV [SI],CL
	JMP READING
	
	EXIT1:
	mov ah,0                             ;CONSUME BUFFER
    int 16h
	JMP FINISHED
	
	NOT_EXIT:
	mov ah,0                             ;CONSUME BUFFER
    int 16h
	
ENDM READ_COMMAND