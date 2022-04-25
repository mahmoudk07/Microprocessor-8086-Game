;//READ POINTS FORM USER 00-99
READ_POINTS MACRO NUM,NUMCONVERTED
    LOCAL CONVERT,CONVERTWHOLE
    MOV AH,0AH
    LEA DX,NUM
    INT 21H
	;CONVERT FROM ASCII TO DECIMAL NUMBER
    LEA SI,NUM+3
    MOV CX,NUMBER_SIZE
    MOV BL,1
    CONVERT:
    MOV AL,[SI] ; Get least significant digit
    SUB AL,30H  ; Convert to decimal
    MUL BL      ; Get exponent
    MOV [SI],AL ; Write back result
    MOV AL,BL   ; Multiply BL * 10
    MOV BH,10
    MUL BH
    MOV BL,AL
    DEC SI      ; Get next higher significant digit
    LOOP CONVERT
	;CONVERT FROM DECIMAL TO WHOLE DECIMAL 10,8 -> 18
    MOV CX,(NUMBER_SIZE-1)
    LEA SI,NUM+3
    LEA DI,NUMCONVERTED
    CONVERTWHOLE:
    MOV AL,[SI] ;Get first num
    DEC SI      
    MOV BL,[SI] ;Get second num
    MOV DL,[DI] ;Get previous sum
    ADD DL,BL
    ADD DL,AL   ; DL = AL + BL + DL
    MOV [DI],DL ; Write back result and repeat
    LOOP CONVERTWHOLE
    LEA DI,NUMCONVERTED
ENDM READ_POINTS