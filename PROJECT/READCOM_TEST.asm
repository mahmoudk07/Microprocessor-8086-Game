.model small
.data 
COM_MESS db 15,?,15 dup(?)

.code 
.INCLUDE MACROS.INC

main proc far
    mov ax,@data
    mov ds,ax 
        
   LEA DX,COM_MESS
    
	READING:
	MOV AH,1
	INT 16H
	JZ READING                         ;7ATETHA HENA 3SHAN COMPARE CHECK LAZEM YEKON BA3D INTRUPT 
	
	CMP AH,3EH
	JZ EXIT1 
	CMP AL,1CH
	JZ NOT_EXIT
	MOV [SI],AL
	MOVE_CURSOR 20,COL
	MOV BL,0F0H
	MOV BH,BL
    MOV CL,4
    SHR BH,CL
    mov ah,9
    MOV CX,1
    int 10h 
	mov ah,0                             ;CONSUME BUFFER
    int 16h
	JMP READING
	
	EXIT1:
	mov ah,0                             ;CONSUME BUFFER
    int 16h
	JMP FINISHED
	
	NOT_EXIT:
	mov ah,0                             ;CONSUME BUFFER
    int 16h    
    
    FINISHED:
    MOV DX,1FA7H
    main endp
end main