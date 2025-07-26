## **Sharp PC-1500A Assembly Instructions**

### **8-bit CPU command list (1)**

#### **Arithmetic/logical**

| Mnemonic | Symbolic Operation | Status (CVHZIE) | Machine Language (76543210) | Byte | Cycle | Comment |
| :---- | :---- | :---- | :---- | :---- | :---- | :---- |
| ADC Rᴸ | A \+ Rᴸ \+ C → A | OOOO- | 00Rᴸ 0010 | 1 | 6 | ● B5 B4 Rᴴ Rᴸ R |
| Rᴴ | A \+ Rᴴ \+ C → A |  | 10Rᴴ 0010 | 1 | 6 | 00 Xᴴ Xᴸ X |
| (R) | A \+ (R) \+ C → A |  | 00R 0011 | 1 | 7 | 01 Yᴴ Yᴸ Y |
| (a,b) | A \+ (a,b) \+ C → A |  | 10100011 | 3 | 13 | 10 Uᴴ Uᴸ U |
|  |  |  |  |  |  | 11 \* \* \* \* |
| \#(R) | A \+ \#(R) \+ C → A |  | FD \<br\> 00R 0011 | 2 | 11 |  |
| \#(a,b) | A \+ \#(a,b) \+ C → A |  | FD \<br\> 10100011 | 4 | 17 | ● Address of (a,b) |
| ADI A,i | A \+ i \+ C → A |  | 10110011 | 2 | 7 |  |
| (R),i | (R) \+ i \+ (R) |  | 01R 1111 | 2 | 13 | (High order) (Low order) |
| (a,b),i | (a,b) \+ i → (a,b) |  | 11101111 | 4 | 19 | ● (R): ME0 accessed |
| \#(R),i | \#(R) \+ i → \#(R) |  | FD \<br\> 01R 1111 | 3 | 17 | ● \#(R): ME1 accessed |
| \#(a,b),i | \#(a,b) \+ i → \#(a,b) |  | FD \<br\> 11101111 | 5 | 23 |  |

| Mnemonic | Symbolic Operation | Status (CVHZIE) | Machine Language (76543210) | Byte | Cycle | Comment |
| :---- | :---- | :---- | :---- | :---- | :---- | :---- |
| DCA (R) | A \+ (R) → A(BCD) |  | 10 R 1100 | 1 | 15 |  |
| \#(R) | A \+ \#(R) → A(BCD) |  | FD \<br\> 10 R 1100 | 2 | 19 |  |
| ADR Rᴸ | Rᴸ \+ A → Rᴸ \<br\> (16-bit register operation) |  | FD \<br\> 11 R 1010 | 2 | 11 |  |
|  | Rᴴ \+ 1 → Rᴴ if C7 |  |  |  |  |  |
| SBC Rᴸ | A \- Rᴸ \- C → A | OOOO- | 00Rᴸ 0000 | 1 | 6 |  |
| Rᴴ | A \- Rᴴ \- C → A |  | 10Rᴴ 0000 | 1 | 6 |  |
| (R) | A \- (R) \- C → A |  | 00R 0001 | 1 | 7 |  |
| (a,b) | A \- (a,b) \- C → A |  | 10100001 | 3 | 13 |  |
| \#(R) | A \- \#(R) \- C → A |  | FD \<br\> 00R 0001 | 2 | 11 |  |
| \#(a,b) | A \- \#(a,b) \- C → A |  | FD \<br\> 10100001 | 4 | 17 |  |
| SBI A,i | A \- i \- C → A |  | 10110001 | 2 | 7 |  |
| DCS (R) | A \- (R) \- C → A(BCD) |  | 00 R 1100 | 1 | 13 |  |
| \#(R) | A \- \#(R) \- C → A(BCD) |  | FD \<br\> 00 R 1100 | 2 | 17 |  |
| AND (R) | A ^ (R) → A | \---O- | 00 R 1001 | 1 | 7 |  |
| (a,b) | A ^ (a,b) → A |  | 10101001 | 3 | 13 |  |
| \#(R) | A ^ \#(R) → A |  | FD \<br\> 00 R 1001 | 2 | 11 |  |
| \#(a,b) | A ^ \#(a,b) → A |  | FD \<br\> 10101001 | 4 | 17 |  |
| ANI A,i | A ^ i → A |  | 10111001 | 2 | 7 |  |
| (R),i | (R) ^ i → (R) |  | 01 R 1001 | 2 | 13 |  |
| (a,b),i | (a,b) ^ i → (a,b) |  | 11101001 | 4 | 19 |  |
| \#(R),i | \#(R) ^ i → \#(R) |  | FD \<br\> 01 R 1001 | 3 | 17 |  |
| \#(a,b),i | \#(a,b) ^ i → \#(a,b) |  | FD \<br\> 11101001 | 5 | 23 |  |

| Mnemonic | Symbolic Operation | Status (CVHZIE) | Machine Language (76543210) | Byte | Cycle | Comment |
| :---- | :---- | :---- | :---- | :---- | :---- | :---- |
| ORA (R) | A V (R) → A | \---O- | 00 R 1011 | 1 | 7 |  |
| (a,b) | A V (a,b) → A |  | 10101011 | 3 | 13 |  |
| \#(R) | A V \#(R) → A |  | FD \<br\> 00 R 1011 | 2 | 11 |  |
| \#(a,b) | A V \#(a,b) → A |  | FD \<br\> 10101011 | 4 | 17 |  |
| ORI A,i | A V i → A |  | 10111011 | 2 | 7 |  |
| (R),i | (R) V i → (R) |  | 01 R 1011 | 2 | 13 |  |
| (a,b),i | (a,b) V i → (a,b) |  | 11101011 | 4 | 19 |  |
| \#(R),i | \#(R) V i → \#(R) |  | FD \<br\> 01 R 1011 | 3 | 17 |  |
| \#(a,b),i | \#(a,b) V i → \#(a,b) |  | FD \<br\> 11101011 | 5 | 23 |  |
| EOR (R) | A ⊕ (R) → A | \---O- | 00 R 1101 | 1 | 7 |  |
| (a,b) | A ⊕ (a,b) → A |  | 10101101 | 3 | 13 |  |
| \#(R) | A ⊕ \#(R) → A |  | FD \<br\> 00 R 1101 | 2 | 11 |  |
| \#(a,b) | A ⊕ \#(a,b) → A |  | FD \<br\> 10101101 | 4 | 17 |  |
| EAI A,i | A ⊕ i → A |  | 10111101 | 2 | 7 |  |
| INC A | A \+ 1 → A | OOOO- | 11011101 | 1 | 5 |  |
| Rᴸ | Rᴸ \+ 1 → Rᴸ |  | 01 Rᴸ 0000 | 1 | 5 |  |
| Rᴴ | Rᴴ \+ 1 → Rᴴ |  | FD \<br\> 01 Rᴴ 0000 | 2 | 9 |  |
| R | R \+ 1 → R | \---O- | 01 R 0100 | 1 | 5 |  |
| DEC A | A \- 1 → A | OOOO- | 11011111 | 1 | 5 |  |
| Rᴸ | Rᴸ \- 1 → Rᴸ |  | 01 Rᴸ 0010 | 1 | 5 |  |
| Rᴴ | Rᴴ \- 1 → Rᴴ |  | FD \<br\> 01 Rᴴ 0010 | 2 | 9 |  |
| R | R \- 1 → R | \---O- | 01 R 0110 | 1 | 5 |  |

#### **Compare and bit test**

| Mnemonic | Symbolic Operation | Status (CVHZIE) | Machine Language (76543210) | Byte | Cycle | Comment |
| :---- | :---- | :---- | :---- | :---- | :---- | :---- |
| CPA Rᴸ | A \- Rᴸ | OOOO- | 00 Rᴸ 0110 | 1 | 6 |  |
| Rᴴ | A \- Rᴴ |  | 10 Rᴴ 0110 | 1 | 6 |  |
| (R) | A \- (R) |  | 00 R 0111 | 1 | 7 |  |
| (a,b) | A \- (a,b) |  | 10100111 | 3 | 13 |  |
| \#(R) | A \- \#(R) |  | FD \<br\> 00 R 0111 | 2 | 11 |  |
| \#(a,b) | A \- \#(a,b) |  | FD \<br\> 10100111 | 4 | 17 |  |
| CPI Rᴸ,i | Rᴸ \- i |  | 01 Rᴸ 1110 | 2 | 7 |  |
| Rᴴ,i | Rᴴ \- i |  | 01 Rᴴ 1100 | 2 | 7 |  |
| A,i | A \- i |  | 10110111 | 2 | 7 |  |
| BIT (R) | A ^ (R) → Z | \---O- | 00 R 1111 | 1 | 7 |  |
| (a,b) | A ^ (a,b) → Z |  | 10101111 | 3 | 13 |  |
| \#(R) | A ^ \#(R) → Z |  | FD \<br\> 00 R 1111 | 2 | 11 |  |
| \#(a,b) | A ^ \#(a,b) → Z |  | FD \<br\> 10101111 | 4 | 17 |  |
| BII A,i | A ^ i → Z |  | 10111111 | 2 | 7 |  |
| (R),i | (R) ^ i → Z |  | 01 R 1101 | 2 | 10 |  |
| (a,b),i | (a,b) ^ i → Z |  | 11101101 | 4 | 16 |  |
| \#(R),i | \#(R) ^ i → Z |  | FD \<br\> 01 R 1101 | 3 | 14 |  |
| \#(a,b),i | \#(a,b) ^ i → Z |  | FD \<br\> 11101101 | 5 | 20 |  |

#### **Load and store**

| Mnemonic | Symbolic Operation | Status (CVHZIE) | Machine Language (76543210) | Byte | Cycle | Comment |
| :---- | :---- | :---- | :---- | :---- | :---- | :---- |
| LDA Rᴸ | Rᴸ → A | \---O- | 00 Rᴸ 0100 | 1 | 5 |  |
| Rᴴ | Rᴴ → A |  | 10 Rᴴ 0100 | 1 | 5 |  |
| (R) | (R) → A |  | 00 R 0101 | 1 | 6 |  |
| (a,b) | (a,b) → A |  | 10100101 | 3 | 12 |  |
| \#(R) | \#(R) → A |  | FD \<br\> 00 R 0101 | 2 | 10 |  |
| \#(a,b) | \#(a,b) → A |  | FD \<br\> 10100101 | 4 | 16 |  |
| LDE R | (R) → A, R-1 → R |  | 01 R 0111 | 1 | 6 |  |
| LIN R | (R) → A, R+1 → R |  | 01 R 0101 | 1 | 6 |  |
| LDI Rᴸ,i | i → Rᴸ | \---O- | 01 Rᴸ 1010 | 2 | 6 |  |
| Rᴴ,i | i → Rᴴ |  | 01 Rᴴ 1000 | 2 | 6 |  |
| A,i | i → A | \---O- | 10110101 | 2 | 6 |  |
| S,i,j | i,j → S |  | 10101010 | 3 | 12 |  |
| LDX R | R → X |  | 00 R 1000 | 2 | 11 |  |
| S | S → X |  | FD \<br\> 01001000 | 2 | 11 |  |
| P | P → X |  | FD \<br\> 01011000 | 2 | 11 |  |

| Mnemonic | Symbolic Operation | Status (CVHZIE) | Machine Language (76543210) | Byte | Cycle | Comment |
| :---- | :---- | :---- | :---- | :---- | :---- | :---- |
| STA Rᴸ | A → Rᴸ | \---O- | 00 Rᴸ 1010 | 1 | 5 |  |
| Rᴴ | A → Rᴴ |  | 00 Rᴴ 1000 | 1 | 5 |  |
| (R) | A → (R) |  | 00 R 1110 | 1 | 6 |  |
| (a,b) | A → (a,b) |  | 10101110 | 3 | 12 |  |
| \#(R) | A → \#(R) |  | FD \<br\> 00 R 1110 | 2 | 10 |  |
| \#(a,b) | A → \#(a,b) |  | FD \<br\> 10101110 | 4 | 16 |  |
| SDE R | A → (R), R-1 → R |  | 01 R 0011 | 1 | 6 |  |
| SIN R | A → (R), R+1 → R |  | 01 R 0001 | 1 | 6 |  |
| STX R | X → R |  | 01 R 1010 | 2 | 11 |  |
| S | X → S |  | FD \<br\> 01001110 | 2 | 11 |  |
| P | X → P |  | FD \<br\> 01011110 | 2 | 11 |  |
| PSH A | A → (S), S-1 → S |  | 11001000 | 2 | 11 |  |
| R | Rᴸ → (S), Rᴴ → (S-1), \<br\> S-2 → S |  | FD \<br\> 10 R 1000 | 2 | 14 |  |
| POP A | (S+1) → A, S+1 → S | \---O- | FD \<br\> 10001010 | 2 | 12 |  |
| R | (S+1) → Rᴴ, (S+2) → Rᴸ, \<br\> S+2 → S |  | FD \<br\> 00 R 1010 | 2 | 15 |  |
| ATT | A → T(STATUS) | OOOOO | 11101100 | 2 | 9 |  |
| TTA | T(STATUS) → A | \---O- | 10101010 | 2 | 9 |  |

#### **Block transfer and search**

| Mnemonic | Symbolic Operation | Status (CVHZIE) | Machine Language (76543210) | Byte | Cycle | Comment |
| :---- | :---- | :---- | :---- | :---- | :---- | :---- |
| TIN | (X) → Y, X+1 → X, Y+1 → Y | \---O- | 11110101 | 1 | 7 |  |
| CIN | A → X, X+1 → X | OOOO- | 11110111 | 1 | 7 |  |

#### **Rotate and shift**

| Mnemonic | Symbolic Operation | Status (CVHZIE) | Machine Language (76543210) | Byte | Cycle | Comment |
| :---- | :---- | :---- | :---- | :---- | :---- | :---- |
| ROL | C ← 7 ← 0 | OOOO- | 11011011 | 1 | 8 |  |
| ROR | C → 7 → 0 |  | 11010001 | 1 | 9 |  |
| SHL | C ← 7 ← 0 |  | 11011001 | 1 | 6 |  |
| SHR | 0 → 7 → 0 → C |  | 11010101 | 1 | 9 |  |
| DRL | A (x) | \---O- | 11010111 | 1 | 12 |  |
| DRL \# | ME1 Area |  | FD \<br\> 11010111 | 2 | 16 |  |
| DRR | A (x) |  | 11010011 | 1 | 12 |  |
| DRR \# | ME1 Area |  | FD \<br\> 11010011 | 2 | 16 |  |
| AEX | A |  | 11110001 | 1 | 6 |  |

#### **CPU control**

| Mnemonic | Symbolic Operation | Status (CVHZIE) | Machine Language (76543210) | Byte | Cycle | Comment |
| :---- | :---- | :---- | :---- | :---- | :---- | :---- |
| AM0 | A → TIMER(T0\~T7) → T8 | \---O- | FD \<br\> 11001110 | 2 | 9 |  |
| AM1 | 1 → T8 |  | FD \<br\> 11011110 | 2 | 9 |  |
| CDV | divider clear |  | FD \<br\> 10001110 | 2 | 8 |  |
| ATP | A → Output port \<br\> (Clock output) |  | FD \<br\> 11001100 | 2 | 9 |  |
| SDP | 1 → Disp |  | FD \<br\> 11000001 | 2 | 8 |  |
| RDP | 0 → Disp |  | FD \<br\> 11000000 | 2 | 8 |  |
| SPU | 1 → PU |  | 11100001 | 1 | 4 |  |
| RPU | 0 → PU |  | 11100011 | 1 | 4 |  |
| SPV | 1 → PV |  | 10101000 | 1 | 4 |  |
| RPV | 0 → PV |  | 10111000 | 1 | 4 |  |
| ITA | IN → A | \---O- | FD \<br\> 10111010 | 2 | 9 |  |
| RIC | 0 → IE | \---O- | FD \<br\> 10111110 | 2 | 8 |  |
| SIE | 1 → IE | \---O- | 10000001 | 2 | 8 |  |
| HLT |  | \---O- | FD \<br\> 10110001 | 2 | 9 |  |
| OFF |  |  | 01001100 | 2 | 8 |  |
| NOP |  |  | 00111000 | 1 | 5 |  |
| SEC | 1 → C | O---O | 11111011 | 1 | 4 |  |
| REC | 0 → C | O---O | 11111001 | 1 | 4 |  |

#### **Jump**

| Mnemonic | Symbolic Operation | Status (CVHZIE) | Machine Language (76543210) | Byte | Cycle | Comment |
| :---- | :---- | :---- | :---- | :---- | :---- | :---- |
| JMP | i,j → P | \---O- | 10111010 | 3 | 12 | s=0: \+i \<br\> s=1: \-i |
| BCH | s=0: P+i → P \<br\> s=1: P-i → P \<br\> if C=0, continue |  | 100s1110 | 2 | 8 \<br\> 9 | (Includes one more cycle) |
| BCS | if C=1, P±i → P \<br\> if C=0, continue |  | 100s0011 | 2 | 8/10/11 |  |
| BCR | if C=0, P±i → P \<br\> if C=1, continue |  | 100s0001 | 2 | 8/10/11 | 000 NC: non carry \<br\> 001 C: carry \<br\> 010 NH: non half \<br\> 011 H: half \<br\> 100 NZ: non zero \<br\> 101 NV: zero \<br\> 110 NV: non overflow \<br\> 111 V: overflow |
| BVS | if V=1, P±i → P \<br\> if V=0, continue |  | 100s1111 | 2 | 8/10/11 |  |
| BVR | if V=0, P±i → P \<br\> if V=1, continue |  | 100s1101 | 2 | 8/10/11 |  |
| BHS | if H=1, P±i → P \<br\> if H=0, continue |  | 100s0111 | 2 | 8/10/11 |  |
| BHR | if H=0, P±i → P \<br\> if H=1, continue |  | 100s0101 | 2 | 8/10/11 |  |
| BZS | if Z=1, P±i → P \<br\> if Z=0, continue |  | 100s1011 | 2 | 8/10/11 |  |
| BZR | if Z=0, P±i → P \<br\> if Z=1, continue |  | 100s1001 | 2 | 8/10/11 |  |
| LOP Uᴸ,i | Uᴸ-1 → Uᴸ \<br\> if Borrow=0, P-i → P \<br\> if Borrow=1, continue |  | 10001000 | 2 | 8/11 |  |

