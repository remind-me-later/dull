{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Asm.Translate where

import Asm.Types
import Data.Word (Word16)

-- NOTE: do not confuse X and Y with AL-X and AL-Y
-- X and Y are CPU registers, while AL-X and AL-Y are 8 byte memory registers
-- used for arithmetic and logical operations

-- Get a value (8 bytes) from a stack address and put it into the stack
addrIntoVarInStack :: [AsmInst]
addrIntoVarInStack =
  [ AsmLabel 1000,
    -- Pop the return address from the stack into X
    AsmPopReg16 AsmRegX,
    -- Load the address of the number into Y
    AsmPopReg16 AsmRegY,
    AsmLdiReg8 AsmRegUL 8, -- iterate 8 times, for each byte
    AsmLin AsmRegY,
    AsmPshA,
    AsmLop (1 + 2), -- Jump to AsmLin 3 bytes before
    AsmStxRegP -- Return to the caller
  ]

-- Given an address and a value in the stack, store the value at the address
varInStackIntoAddress :: [AsmInst]
varInStackIntoAddress =
  [ AsmLabel 1001,
    -- Pop the return address from the stack into X
    AsmPopReg16 AsmRegX,
    -- Load the address of the number into Y
    AsmPopReg16 AsmRegY,
    -- Add 7
    AsmLdiA 7,
    AsmAdr AsmRegY,
    AsmLdiReg8 AsmRegUL 8, -- iterate 8 times, for each byte
    -- Load the byte of the number into A
    AsmPopA,
    AsmSde AsmRegY,
    AsmLop (1 + 2), -- Jump to AsmLin 3 bytes before
    AsmStxRegP -- Return to the caller
  ]

binOpNums :: Word16 -> [AsmInst]
binOpNums binopFunAddr =
  [ AsmLabel 1002,
    -- Save the return address in X
    AsmPopReg16 AsmRegX,
    -- Push the address of AL-Y=7A10H into the stack
    AsmLdiReg8 AsmRegYH 0x7A,
    AsmLdiReg8 AsmRegYL 0x10,
    AsmPshReg16 AsmRegY,
    -- Call the function to move the number from the stack into the address
    AsmSjp 1001,
    -- Push the address of AL-X=7A00H into the stack
    AsmLdiReg8 AsmRegYH 0x7A,
    AsmLdiReg8 AsmRegYL 0x00,
    AsmPshReg16 AsmRegY,
    -- Call the function to move the number from the stack into the address
    AsmSjp 1001,
    AsmPshReg16 AsmRegX, -- Push the return address
    -- Call the binary operation function
    AsmSjp binopFunAddr,
    -- Move the contents of register AL-X into the stack
    AsmLdiReg8 AsmRegYH 0x7A,
    AsmLdiReg8 AsmRegYL 0x00,
    AsmPshReg16 AsmRegY,
    AsmSjp 1000,
    AsmRtn
  ]

-- Like before, but we only use memory register AL-X
unaryOpNums :: Word16 -> [AsmInst]
unaryOpNums unaryOpFunAddr =
  [ AsmLabel 1003,
    -- Save the return address in AL-X
    AsmPopReg16 AsmRegX,
    -- Push the address of AL-X=7A00H into the stack
    AsmLdiReg8 AsmRegYH 0x7A,
    AsmLdiReg8 AsmRegYL 0x00,
    AsmPshReg16 AsmRegY,
    -- Call the function to move the number from the stack into the address
    AsmSjp 1001,
    -- Call the unary operation function
    AsmSjp unaryOpFunAddr,
    -- Move the contents of register AL-X into the stack
    AsmLdiReg8 AsmRegYH 0x7A,
    AsmLdiReg8 AsmRegYL 0x00,
    AsmPshReg16 AsmRegY,
    AsmSjp 1000,
    AsmRtn
  ]

addAndPush :: [AsmInst]
addAndPush =
  binOpNums 0xEFBA -- Address of the ADD function

subAndPush :: [AsmInst]
subAndPush =
  binOpNums 0xEFB6 -- Address of the SUB function
