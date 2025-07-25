{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Asm.Translate where

import Asm.Types
import Control.Monad.State
import Hir.Types

data TranslationState = TranslationState
  { tsStringLiteralBeginAddress :: Int
  }

translateHirIntrinsic :: HirIntrinsic -> State TranslationState [AsmInst]
translateHirIntrinsic HirPrint = do
  -- TODO: cursor pointer should be already in the corretct location, check
  return
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
