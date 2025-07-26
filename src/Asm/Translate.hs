{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Asm.Translate where

import Asm.Types
import Control.Monad.State
import Data.Word (Word16)
import Hir.Types

data TranslationState = TranslationState
  { tsStringLiteralBeginAddress :: Int,
    nextUsableLabel :: Int
  }

getNextLabelAndIncrement :: State TranslationState Int
getNextLabelAndIncrement = do
  state' <- get
  let label = nextUsableLabel state'
  put $ state' {nextUsableLabel = label + 1}
  return label

printSyscallInsts :: [AsmInst]
printSyscallInsts =
  [ -- Store size of character string in the accumulator
    -- it's the first byte to be popped from the stack
    AsmPopA,
    -- Store the address of the string in the U register
    AsmPopReg16 AsmRegU,
    -- The other 5 bytes are useless, move sp to x, substract 5 and store in sp
    AsmLdxRegS AsmRegX,
    -- Save A into XL
    AsmSta (AsmOperandReg8 AsmRegXL),
    AsmLdiA (-5),
    AsmAdr AsmRegX,
    AsmStxRegS AsmRegX,
    -- Move XL to A
    AsmLda (AsmOperandReg8 AsmRegXL),
    -- Call the print intrinsic
    AsmSjp 0xED00
  ]

sleepSyscallInsts :: [AsmInst]
sleepSyscallInsts =
  -- TODO: implement
  []

-- Character ASCII code must be placed in the accumulator
printCharAndAdvanceCursorInsts :: [AsmInst]
printCharAndAdvanceCursorInsts =
  [ -- Sycall
    AsmSjp 0xED4D
  ]

-- The variable address is in U
-- accIntoNumVarInsts :: [AsmInst]
-- accIntoNumVarInsts reg =
--   [ 
--   ]

-- FIXME: for no only works up to 255, should be fixed
-- Result is in the accumulator
asciiStrToNumberInsts :: Word16 -> State TranslationState [AsmInst]
asciiStrToNumberInsts str_address = do
  loopLabel <- getNextLabelAndIncrement
  endLabel <- getNextLabelAndIncrement
  return $
    [ -- XL = (i8)str_address
      AsmLdiReg8 AsmRegXL (fromIntegral str_address),
      -- XH = (i8)str_address >> 8
      AsmLdiReg8 AsmRegXH (fromIntegral (str_address `div` 256)),
      -- A = (X), X += 1
      AsmLin AsmRegX,
      AsmLabel loopLabel,
      -- A = A - '0'
      AsmSbiA
        (fromIntegral (fromEnum '0')),
      -- If A < 0, then it's not a digit, so we stop
      -- SBI already sets the C flag, TODO: check
      AsmBcs endLabel,
      -- Save current
      AsmLabel endLabel
    ]

translateHirIntrinsic :: HirIntrinsic -> State TranslationState [AsmInst]
translateHirIntrinsic HirPrint = do
  -- TODO: cursor pointer should be already in the corretct location, check
  return printSyscallInsts
translateHirIntrinsic HirPause = do
  return $ printSyscallInsts ++ sleepSyscallInsts
translateHirIntrinsic (HirUsing _usingClause) = do
  error "Unimplemented: HirUsing"
