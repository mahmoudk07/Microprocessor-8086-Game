;//FORRBIDEN CHAR TEXT
FORBIDDEN_CHAR MACRO CHAR1
LOCAL CHECK0,CHECK,L1,L2,L3,L4,L5,NUMBER,ALFA,CONT,CAP_ALFA

 MOV DI,OFFSET CHAR1
 
    JMP CHECK
    
	CHECK0:
    mov ah,0                             ;CONSUME BUFFER
    int 16h    
 
    CHECK:
	MOV AH,1
	INT 16H  
	JZ CHECK 
	                     ;7ATETHA HENA 3SHAN COMPARE CHECK LAZEM YEKON BA3D INTRUPT 
	
	;CHECK KEY IS RIGHT
	;-------------------------
	CMP AL,30H
	JZ NUMBER
	JA L1 
	JMP CHECK0 
	
	 L1:
    CMP AL,39H
    JBE NUMBER
    JMP  L2
    
    L2:
    CMP AL,41H 
    JB CHECK0
    JMP L3
    
    L3:
    CMP AL,5AH
    JBE CAP_ALFA
	JMP L4
	
	L4:
	CMP AL,61H 
    JB CHECK0
    JMP L5
	
	L5:
	CMP AL,7AH
    JBE ALFA
	JMP CHECK0
	
	;------------------------- 
	CAP_ALFA:
	ADD AL,20H
	JMP ALFA
	
    NUMBER:
    MOV [DI],AL
    JMP CONT
     
    ALFA:
    MOV [DI],AL
	JMP CONT
  CONT:

ENDM FORBIDDEN_CHAR