ADDR_IN_Y_INTO_VAR_IN_AL_X:
    LDI XH, 0x7A ; AL-X (7A00H-7A07H)
    LDI XL, 0x00 ; 
    LDI UL, 8 ; Prepare UL to iterate 8 times
LO1:
    LIN Y ; A = (Y), Y += 1
    SIN X ; Store into AL-X (7A00H-7A07H)
    LOP LO1 ; Loop until UL is zero
    REC ; Clear carry (success)
    RTN ; Return

VAR_IN_AL_X_INTO_ADDR_IN_Y:
    LDI XH, 0x7A ; AL-X (7A00H-7A07H)
    LDI XL, 0x00 ; 
    LDI UL, 8 ; Prepare UL to iterate 8 times
LO2:
    LIN X ; A = (X), X += 1
    SIN Y ; Store into (Y)
    LOP LO2 ; Loop until UL is zero
    REC ; Clear carry (success)
    RTN ; Return

; Input: Number at AL-X (7A00H-7A07H)
; Output: String at buffer pointed by Y register, string info in AL-Y (7A10H-7A17H)

NUM_TO_STR:
    ; Initialize string info in AL-Y
    ; Store buffer address in AL-Y+5,6 (ADDH, ADDL)
    LDA YH
    STA (7A15H)      ; Store high byte of buffer address
    LDA YL
    STA (7A16H)      ; Store low byte of buffer address
    
    ; Initialize length counter in AL-Y+7
    LDI A,0
    STA (7A17H)       ; LENGTH = 0
    
    ; Save original Y (buffer pointer) in U register
    STX U
    LDX Y
    STX U
    
    ; Check if number is zero
    LDA (7A00H)         ; Load exponent
    CPI A,0
    BZS ZERO_CASE
    
    ; Handle sign
    LDA (7A01H)         ; Load sign byte
    CPI A,0x80
    BZR POSITIVE
    LDI A,'-'           ; Negative number
    SIN Y
    SJP INC_LENGTH
    BCS STRING_TOO_LONG
    JMP PROCESS_MANTISSA
    
POSITIVE:
    ; Skip sign for positive numbers in fixed point
    
PROCESS_MANTISSA:
    ; Convert mantissa (5 BCD bytes) to decimal digits
    ; First, we need to determine decimal point position from exponent
    LDA (7A00H)         ; Load exponent
    SBI A,0x80          ; Remove bias (exponent - 128)
    STA (7A16H)       ; Store adjusted exponent temporarily
    
    ; Convert mantissa digits
    LDI UL,5            ; Process 5 mantissa bytes
    LDI XH,0x7A         ; Point to mantissa start
    LDI XL,0x02
    
    ; Add leading digit and decimal point based on exponent
    LDA (7A16H)         ; Get adjusted exponent
    CPI A,0
    BZS ADD_ZERO_POINT  ; If exp=0, format as 0.xxxxx
    BCS ADD_DECIMAL     ; If exp>0, add integer part
    JMP ADD_ZERO_POINT  ; If exp<0, format as 0.xxxxx

ADD_DECIMAL:
    ; For positive exponent, add integer digits first
    LDI A,'1'           ; Leading 1 for normalized mantissa
    SIN Y
    SJP INC_LENGTH
    BCS STRING_TOO_LONG
    
    ; Add decimal point after appropriate digits
    LDI A,'.'
    SIN Y
    SJP INC_LENGTH
    BCS STRING_TOO_LONG
    JMP MANTISSA_LOOP

MANTISSA_LOOP:
    LIN X               ; Load BCD byte
    PSH A
    
    ; High nibble
    ANI A,0xF0          ; Mask low nibble
    ; Shift right 4 times using SHR
    SHR
    SHR  
    SHR
    SHR
    
    ADI A,'0'           ; Convert to ASCII
    SIN Y
    SJP INC_LENGTH
    BCS STRING_TOO_LONG
    
    ; Low nibble
    POP A
    ANI A,0x0F          ; Mask high nibble
    ADI A,'0'           ; Convert to ASCII
    SIN Y
    SJP INC_LENGTH
    BCS STRING_TOO_LONG
    
    LOP UL,MANTISSA_LOOP
    JMP FINALIZE_STRING

ADD_ZERO_POINT:
    ; Add "0." for numbers less than 1
    LDI A,'0'
    SIN Y
    SJP INC_LENGTH
    BCS STRING_TOO_LONG
    
    LDI A,'.'
    SIN Y
    SJP INC_LENGTH
    BCS STRING_TOO_LONG
    JMP MANTISSA_LOOP

ZERO_CASE:
    ; Handle zero special case
    LDI A,'0'
    SIN Y
    SJP INC_LENGTH
    JMP FINALIZE_STRING

INC_LENGTH:
    ; Increment length counter and check bounds
    LDA (7A17H)         ; Load current length
    INC A
    CPI A,20            ; Check if exceeds 20 bytes
    BCS TOO_LONG        ; Branch if carry set (>=20)
    STA (7A17H)       ; Store new length
    REC                 ; Clear carry (success)
    RTN
TOO_LONG:
    SEC                 ; Set carry (failure)
    RTN

STRING_TOO_LONG:
    ; String exceeded 20 bytes - abort
    ; Set length to 0 to indicate error
    LDI A,0
    STA (7A17H)
    RTN

FINALIZE_STRING:
    ; Set final string info in AL-Y
    ; Bytes 0-3: Invalid data (marked with *)
    ; LDI A,0xFF
    ; STA (7A10H)       ; *
    ; STA (7A11H)       ; *
    ; STA (7A12H)       ; *
    ; STA (7A13H)       ; *
    
    ; Byte 4: 00H (marker)
    LDI A,0x00
    STA (7A14H)
    
    ; Bytes 5-6: Buffer address (already set)
    ; Byte 7: Length (already set)
    
    RTN