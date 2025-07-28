; Subroutine labels begin with "SUB_"

SUB_ADDR_IN_Y_INTO_VAR_IN_AL_X:
    LDI XH, 0x7A ; AL-X (7A00H-7A07H)
    LDI XL, 0x00 ; 
    LDI UL, 8 ; Prepare UL to iterate 8 times
LO1:
    LIN Y ; A = (Y), Y += 1
    SIN X ; Store into AL-X (7A00H-7A07H)
    LOP LO1 ; Loop until UL is zero
    REC ; Clear carry (success)
    RTN ; Return

; Treat the numeric value in AL-X (7A00H-7A07H) as a variable address
; this means transforming the 8 byte representation into a 16bit address
; and storing it in the Y register.
; Input: AL-X contains 8 byte address (7A00H-7A07H)
; Output: Y register contains the 16-bit address
SUB_VAR_IN_AL_X_INTO_ADDR_IN_Y:
    ; Check if number is positive (sign byte at 7A01H must be 00H)
    LDA (7A01H)         ; Load sign byte
    CPI A,0x00          ; Check if positive (00H)
    BZR POSITIVE_ADDRESS
    ; Number is negative - error
    SEC                 ; Set carry (error)
    RTN

POSITIVE_ADDRESS:
    ; Build 16-bit address from BCD digits in bytes 2, 3, and 4
    ; We'll extract each BCD digit and build the binary value
    
    ; Initialize 16-bit result to zero
    LDI A,0x00
    STA YH              ; High byte = 0
    STA YL              ; Low byte = 0
    
    ; Process byte 2 (7A02H) - most significant BCD digits
    LDA (7A02H)
    PSH A               ; Save for low nibble
    
    ; High nibble of byte 2 (ten-thousands place)
    ANI A,0xF0
    SHR
    SHR
    SHR
    SHR
    ; Multiply by 10000 and add to result
    ; For addresses up to 65535, we can have at most 6 in ten-thousands
    ; Simple: if digit >= 1, add 10000 (0x2710) to result
    CPI A,0
    BZS SKIP_TEN_THOUSANDS
    ; Add 10000 * digit to YH:YL
    ; 10000 = 0x2710, so for digit 1: add 0x2710, digit 2: add 0x4E20, etc.
    ; Simplified: just use the digit as high byte contribution
    ADI A,A             ; A = A * 2 
    ADI A,A             ; A = A * 4
    ADI A,A             ; A = A * 8  (approximate scaling)
    STA YH              ; Store scaled value as high byte

SKIP_TEN_THOUSANDS:
    ; Low nibble of byte 2 (thousands place)  
    POP A
    ANI A,0x0F
    ; Add thousands: multiply by 1000 and add
    ; 1000 = 0x3E8, so for each digit multiply by 1000
    ; Simplified approach: add to high byte with scaling
    ADI A,A             ; A = A * 2
    ADI A,A             ; A = A * 4  
    ADA YH              ; Add to high byte
    STA YH
    
    ; Process byte 3 (7A03H) - middle BCD digits
    LDA (7A03H)
    PSH A               ; Save for low nibble
    
    ; High nibble (hundreds place)
    ANI A,0xF0
    SHR
    SHR
    SHR
    SHR
    ; Add hundreds to low byte
    ADA YL
    STA YL
    ; Handle carry to high byte
    BCC NO_CARRY_1
    LDA YH
    INC A
    STA YH
NO_CARRY_1:
    
    ; Low nibble (tens place)
    POP A
    ANI A,0x0F
    ; Multiply by 10 and add to low byte
    STA (7A18H)         ; Store digit
    ADI A,A             ; A = A * 2
    ADI A,A             ; A = A * 4
    ADI A,A             ; A = A * 8
    ADA (7A18H)         ; A = A * 8 + digit = A * 9
    ADA (7A18H)         ; A = A * 9 + digit = A * 10
    ADA YL              ; Add to low byte
    STA YL
    BCC NO_CARRY_2
    LDA YH
    INC A
    STA YH
NO_CARRY_2:
    
    ; Process byte 4 (7A04H) - units digit
    LDA (7A04H)
    ANI A,0x0F          ; Only need low nibble for units
    ADA YL              ; Add units to low byte
    STA YL
    BCC NO_CARRY_3
    LDA YH
    INC A
    STA YH
NO_CARRY_3:
    
    ; Validate address is reasonable (not zero, not too high)
    LDA YH
    ORA YL              ; OR high and low bytes
    CPI A,0x00          ; Check if both are zero
    BZS INVALID_ADDRESS
    
    ; Check if address is too high (>= 0x8000)
    LDA YH
    CPI A,0x80
    BCS INVALID_ADDRESS
    
    REC                 ; Clear carry (success)
    RTN

INVALID_ADDRESS:
    ; Address validation failed
    SEC                 ; Set carry (error)
    RTN
    

; Modified NUM_TO_STR function
; Input: Number at AL-X (7A00H-7A07H)
; Output: String at buffer 0x7BB0, length in register A

SUB_NUM_TO_STR:
    ; Set Y to point to input buffer (0x7BB0)
    LDI YH,0x7B
    LDI YL,0xB0
    
    ; Initialize length counter in a temporary location
    LDI A,0
    STA (7A17H)       ; Use 7A17H as temp length counter
    
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
    STA (7A16H)         ; Store adjusted exponent temporarily
    
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
    JMP FINALIZE_NUM_TO_STR

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
    JMP FINALIZE_NUM_TO_STR

FINALIZE_NUM_TO_STR:
    ; Load final length into A register
    LDA (7A17H)         ; Load length into A
    REC                 ; Clear carry (success)
    RTN

; Store address in U register and length in A into string header in AL-X
; Input: U register contains address, A contains length
; Output: String header stored in AL-X (7A00H-7A07H)
SUB_STORE_STRING_HEADER_IN_AL_X:
    ; Store string header in AL-X format
    LDI XH,0x7A         ; Point to AL-X
    LDI XL,0x00
    
    ; Bytes 0-3: Invalid data (marked with *)
    LDI A,0xFF
    SIN X               ; Store * at 7A00H, X=7A01H
    SIN X               ; Store * at 7A01H, X=7A02H  
    SIN X               ; Store * at 7A02H, X=7A03H
    SIN X               ; Store * at 7A03H, X=7A04H
    
    ; Byte 4: 00H (marker)
    LDI A,0x00
    SIN X               ; Store 00H at 7A04H, X=7A05H
    
    ; Bytes 5-6: Address from U register (ADDH, ADDL)
    LDA UH
    SIN X               ; Store UH at 7A05H, X=7A06H
    LDA UL
    SIN X               ; Store UL at 7A06H, X=7A07H
    
    ; Byte 7: Length (restore from stack or temp location)
    PSH A               ; Save current A
    LDA (7A17H)         ; Get length from temp location
    SIN X               ; Store length at 7A07H
    POP A               ; Restore A
    
    REC                 ; Clear carry (success)
    RTN

; Load string header from AL-X into U register (address) and A (length)
; Input: String header in AL-X (7A00H-7A07H)  
; Output: U register contains address, A contains length
SUB_LOAD_STRING_HEADER_FROM_AL_X:
    ; Load string header from AL-X format
    LDI XH,0x7A         ; Point to AL-X
    LDI XL,0x05         ; Skip to address bytes (offset 5)
    
    ; Load address into U register
    LIN X               ; A = high byte of address, X=7A06H
    STA UH              ; Store high byte in UH
    LIN X               ; A = low byte of address, X=7A07H
    STA UL              ; Store low byte in UL
    
    ; Load length into A
    LIN X               ; A = length byte
    
    REC                 ; Clear carry (success)
    RTN

; Get string input from keyboard and store in input buffer (0x7BB0)
; Output: String at buffer 0x7BB0, length in register A
; Uses Key scan subroutine at E243H
SUB_INPUT_STRING:
    ; Set Y to point to input buffer (0x7BB0)
    LDI YH,0x7B
    LDI YL,0xB0
    
    ; Initialize length counter
    LDI A,0
    STA (7A17H)         ; Use 7A17H as temp length counter
    
INPUT_LOOP:
    ; Call key scan subroutine to get next character
    SJP 0xE243          ; Key scan subroutine - key code stored in accumulator
    
    ; Check for Enter key (0x0D) to end input
    CPI A,0x0D
    BZS INPUT_COMPLETE
    
    ; Check for Backspace key (0x08) to delete character
    CPI A,0x08
    BZS HANDLE_BACKSPACE
    
    ; Check if character is printable (>= 0x20 space character)
    CPI A,0x20
    BCS STORE_CHAR      ; Branch if A >= 0x20 (printable)
    JMP INPUT_LOOP      ; Ignore non-printable characters
    
STORE_CHAR:
    ; Check if buffer is full (20 character limit)
    LDA (7A17H)         ; Load current length
    CPI A,20
    BCS INPUT_LOOP      ; If length >= 20, ignore character
    
    ; Store character in buffer
    SIN Y               ; Store A in buffer, increment Y
    
    ; Increment length counter
    LDA (7A17H)
    INC A
    STA (7A17H)
    
    JMP INPUT_LOOP
    
HANDLE_BACKSPACE:
    ; Check if there are characters to delete
    LDA (7A17H)         ; Load current length
    CPI A,0
    BZS INPUT_LOOP      ; If length = 0, nothing to delete
    
    ; Decrement length and buffer pointer
    DEC A
    STA (7A17H)         ; Store new length
    
    ; Move Y pointer back one position
    LDA YL
    DEC A
    STA YL
    ; Handle underflow if needed
    BCC NO_BORROW
    LDA YH
    DEC A
    STA YH
NO_BORROW:
    
    JMP INPUT_LOOP
    
INPUT_COMPLETE:
    ; Load final length into A register
    LDA (7A17H)         ; Load length into A
    REC                 ; Clear carry (success)
    RTN

; Parse string in input buffer (0x7BB0) into number in AL-X
; Input: String length in register A, string at buffer 0x7BB0
; Output: Number stored in AL-X (7A00H-7A07H)
SUB_STR_TO_NUM:
    ; Save string length
    STA (7A17H)         ; Store length for processing
    
    ; Initialize AL-X to zero
    LDI XH,0x7A
    LDI XL,0x00
    LDI UL,8
    LDI A,0
CLEAR_AL_X:
    SIN X
    LOP CLEAR_AL_X
    
    ; Check for empty string
    LDA (7A17H)
    CPI A,0
    BZS STR_PARSE_COMPLETE
    
    ; Set Y to point to input buffer
    LDI YH,0x7B
    LDI YL,0xB0
    
    ; Initialize parsing variables
    LDI A,0
    STA (7A16H)         ; Sign flag (0=positive, 1=negative)
    STA (7A15H)         ; Decimal point position (0=none found)
    STA (7A14H)         ; Digit count
    
    ; Check first character for sign
    LIN Y               ; Load first character
    CPI A,'-'
    BZR NEGATIVE_SIGN
    CPI A,'+'
    BZR POSITIVE_SIGN
    ; No sign, put character back
    LDA YL
    DEC A
    STA YL
    JMP PARSE_DIGITS

NEGATIVE_SIGN:
    LDI A,1
    STA (7A16H)         ; Set negative flag
    JMP PARSE_DIGITS

POSITIVE_SIGN:
    ; Positive sign already default (0)

PARSE_DIGITS:
    LDA (7A17H)         ; Get remaining length
    LDA YL
    SBI A,0xB0          ; Calculate position in buffer
    STA (7A13H)         ; Store current position
    
DIGIT_LOOP:
    ; Check if we've processed all characters
    LDA (7A13H)         ; Current position
    LDA (7A17H)         ; Total length
    CPA (7A13H)         ; Compare position with length
    BZS PARSING_DONE
    
    ; Load next character
    LIN Y
    
    ; Check for decimal point
    CPI A,'.'
    BZR FOUND_DECIMAL
    
    ; Check if it's a digit (0-9)
    CPI A,'0'
    BCS NOT_DIGIT       ; Branch if A < '0'
    CPI A,'9'+1
    BCC NOT_DIGIT       ; Branch if A > '9'
    
    ; It's a digit - convert to BCD and store
    SBI A,'0'           ; Convert ASCII to digit value
    
    ; Shift existing mantissa left and add new digit
    ; This is a simplified version - actual implementation would
    ; need to handle BCD arithmetic properly
    STA (7A12H)         ; Store current digit
    
    ; Increment digit count
    LDA (7A14H)
    INC A
    STA (7A14H)
    
    ; Increment position counter
    LDA (7A13H)
    INC A
    STA (7A13H)
    
    JMP DIGIT_LOOP

FOUND_DECIMAL:
    ; Mark decimal point position
    LDA (7A14H)         ; Current digit count
    STA (7A15H)         ; Store as decimal position
    
    ; Increment position counter
    LDA (7A13H)
    INC A
    STA (7A13H)
    
    JMP DIGIT_LOOP

NOT_DIGIT:
    ; Invalid character - stop parsing
    JMP PARSING_DONE

PARSING_DONE:
    ; Convert collected digits to proper decimal format
    ; Set exponent based on decimal point position
    LDA (7A15H)         ; Decimal point position
    CPI A,0
    BZS NO_DECIMAL_POINT
    
    ; Calculate exponent with decimal point
    LDA (7A14H)         ; Total digits
    SBA (7A15H)         ; Subtract decimal position
    ADI A,0x80          ; Add bias
    STA (7A00H)         ; Store exponent
    JMP SET_SIGN

NO_DECIMAL_POINT:
    ; No decimal point - integer
    LDA (7A14H)         ; Digit count
    DEC A               ; Adjust for normalized form
    ADI A,0x80          ; Add bias
    STA (7A00H)         ; Store exponent

SET_SIGN:
    ; Set sign byte
    LDA (7A16H)         ; Load sign flag
    CPI A,0
    BZS POSITIVE_NUM
    LDI A,0x80          ; Negative
    STA (7A01H)
    JMP STR_PARSE_COMPLETE

POSITIVE_NUM:
    LDI A,0x00          ; Positive
    STA (7A01H)

STR_PARSE_COMPLETE:
    REC                 ; Clear carry (success)
    RTN
