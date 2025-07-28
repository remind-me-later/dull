module Ir.Types
  ( IrIntrinsic (..),
    IrFun (..),
    IrInst (..),
    IrProgram (..),
  )
where

import Data.Word (Word16)

data IrIntrinsic where
  IrPrintNum :: IrIntrinsic
  IrPrintStr :: IrIntrinsic
  IrUsing :: String -> IrIntrinsic
  IrInputNum :: IrIntrinsic
  IrInputStr :: IrIntrinsic
  IrGPrintNum :: IrIntrinsic
  IrGPrintStr :: IrIntrinsic
  IrClear :: IrIntrinsic
  IrCls :: IrIntrinsic
  IrRandom :: IrIntrinsic
  IrSleep :: IrIntrinsic
  IrSetPokeAddress :: IrIntrinsic
  IrPoke :: {irPokeMemoryArea :: Int} -> IrIntrinsic
  IrCursor :: IrIntrinsic
  IrGCursor :: IrIntrinsic
  IrBeepStmt :: {irBeepHasOptParams :: Bool} -> IrIntrinsic
  deriving (Eq)

instance Show IrIntrinsic where
  show IrPrintNum = "print_num"
  show IrPrintStr = "print_str"
  show (IrUsing usingClause) = "using_fmt(" ++ show usingClause ++ ")"
  show IrInputNum = "input_num"
  show IrInputStr = "input_str"
  show IrGPrintNum = "gprint_num"
  show IrGPrintStr = "gprint_str"
  show IrClear = "clear_vars"
  show IrCls = "cls"
  show IrRandom = "gen_random_seed"
  show IrSleep = "sleep"
  show (IrPoke memArea) =
    "poke_memory_area_" ++ show memArea
  show IrSetPokeAddress = "set_poke_address"
  show IrCursor = "set_cursor"
  show IrGCursor = "set_gcursor"
  show (IrBeepStmt hasOptParams) =
    "beep" ++ (if hasOptParams then "_with_params" else "")

data IrFun where
  -- Arithmetic operations
  IrAddOp :: IrFun
  IrSubOp :: IrFun
  IrMulOp :: IrFun
  IrDivOp :: IrFun
  IrExponentOp :: IrFun
  IrAndOp :: IrFun
  IrOrOp :: IrFun
  -- Comparison operations
  IrEqOp :: IrFun
  IrNeqOp :: IrFun
  IrLtOp :: IrFun
  IrLeqOp :: IrFun
  IrGtOp :: IrFun
  IrGeqOp :: IrFun
  -- "Special" operations
  IrMidOp :: IrFun
  IrLeftOp :: IrFun
  IrRightOp :: IrFun
  IrAsciiOp :: IrFun
  IrPointOp :: IrFun
  IrRndOp :: IrFun
  IrIntOp :: IrFun
  IrSgnOp :: IrFun
  deriving (Eq)

instance Show IrFun where
  show IrMidOp = "mid" -- TODO: docs page 126 wtf does that mean?
  show IrLeftOp = "left AL-X, 7890H"
  show IrRightOp = "right AL-X, 7890H"
  show IrAsciiOp = "ascii AL-X"
  show IrPointOp = "point" -- TODO:
  show IrRndOp = "rnd" -- TODO:
  show IrIntOp = "int AL-X"
  show IrSgnOp = "sgn AL-X"
  show IrAddOp = "add AL-X AL-Y"
  show IrSubOp = "sub AL-X AL-Y"
  show IrMulOp = "mul AL-X AL-Y"
  show IrDivOp = "div AL-X AL-Y"
  show IrExponentOp = "exp AL-X AL-Y"
  show IrAndOp = "and AL-X AL-Y"
  show IrOrOp = "or AL-X AL-Y"
  show IrEqOp = "eq AL-X AL-Y"
  show IrNeqOp = "neq AL-X AL-Y"
  show IrLtOp = "lt AL-X AL-Y"
  show IrLeqOp = "leq AL-X AL-Y"
  show IrGtOp = "gt AL-X AL-Y"
  show IrGeqOp = "geq AL-X AL-Y"

type Label = Int

data IrInst where
  IrLdImmIntoAlX :: Double -> IrInst
  IrLdImmIntoAlY :: Double -> IrInst
  IrLdImmIndirectIntoAlX :: Word16 -> IrInst
  IrLdImmIndirectIntoAlY :: Word16 -> IrInst
  IrAddrInYregIntoAlX :: IrInst
  IrStAlXInYreg :: IrInst
  IrAddrInAlXToYreg :: IrInst
  IrAlXToAlY :: IrInst
  IrAlYToAlX :: IrInst
  IrImmToYreg :: Word16 -> IrInst
  IrFun :: IrFun -> IrInst
  -- Labels
  IrLabel :: Label -> IrInst
  -- Jumps
  IrGoto :: Label -> IrInst
  IrCall :: Label -> IrInst
  IrCondGoto :: Label -> IrInst
  IrCondCall :: Label -> IrInst
  IrReturn :: IrInst
  -- Intrinsics
  IrIntrinsicCall :: IrIntrinsic -> IrInst
  deriving (Eq)

instance Show IrInst where
  show (IrLdImmIntoAlX num) =
    "\tAL-X = " ++ show num
  show (IrLdImmIntoAlY num) =
    "\tAL-Y = " ++ show num
  show (IrLdImmIndirectIntoAlX addr) =
    "\tAL-X = (" ++ show addr ++ ")"
  show (IrLdImmIndirectIntoAlY addr) =
    "\tAL-Y = (" ++ show addr ++ ")"
  show IrStAlXInYreg = "\t(Yreg) = AL-X"
  show IrAddrInAlXToYreg = "\tYreg = (AL-X)"
  show IrAddrInYregIntoAlX = "\tAL-X = (Yreg)"
  show (IrImmToYreg ident) =
    "\tYreg = " ++ show ident
  show IrAlXToAlY = "\tAL-Y = AL-X"
  show IrAlYToAlX = "\tAL-X = AL-Y"
  show (IrLabel idx) = "L" ++ show idx ++ ":"
  show (IrGoto idx) = "\tgoto L" ++ show idx
  show (IrCall idx) = "\tcall L" ++ show idx
  show (IrCondGoto idx) = "\tif Z goto L" ++ show idx
  show (IrCondCall idx) = "\tif Z call L" ++ show idx
  show IrReturn = "\treturn"
  show (IrIntrinsicCall intrinsic) = "\t@" ++ show intrinsic
  show (IrFun op) = "\t" ++ show op

newtype IrProgram = IrProgram
  { irProgramStatements :: [IrInst]
  }
  deriving (Eq)

instance Show IrProgram where
  show (IrProgram stmts) = unlines (map show stmts)