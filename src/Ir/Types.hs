module Ir.Types
  ( IrIntrinsic (..),
    IrFun (..),
    IrInst (..),
    IrProgram (..),
  )
where

import Data.Char (toUpper)
import Data.Word (Word16)
import Numeric (showHex)

data IrIntrinsic where
  IrStrDataInAlXIntoUregAndAreg :: IrIntrinsic
  IrUregAndAregIntoStrDataInAlX :: IrIntrinsic
  IrNumToStrInBuffer :: IrIntrinsic
  IrStrInBufferToNum :: IrIntrinsic
  IrPrintStr :: IrIntrinsic
  IrUsing :: String -> IrIntrinsic
  IrInputStr :: IrIntrinsic
  IrGPrintNum :: IrIntrinsic
  IrGPrintStr :: IrIntrinsic
  IrClear :: IrIntrinsic
  IrCls :: IrIntrinsic
  IrRandom :: IrIntrinsic
  IrSetPokeAddress :: IrIntrinsic
  IrPoke :: {irPokeMemoryArea :: Int} -> IrIntrinsic
  IrGCursor :: IrIntrinsic
  IrBeepStmt :: {irBeepHasOptParams :: Bool} -> IrIntrinsic
  deriving (Eq)

instance Show IrIntrinsic where
  show IrStrDataInAlXIntoUregAndAreg = "SJP LOAD_STRING_HEADER_FROM_AL_X"
  show IrUregAndAregIntoStrDataInAlX = "SJP STORE_STRING_HEADER_IN_AL_X"
  show IrNumToStrInBuffer = "SJP NUM_TO_STR"
  show IrStrInBufferToNum = "SJP STR_TO_NUM"
  show IrPrintStr = "SBJ 0xED00"
  show (IrUsing usingClause) = "using_fmt(" ++ show usingClause ++ ")"
  show IrInputStr = "SJP INPUT_STRING"
  show IrGPrintNum = "gprint_num"
  show IrGPrintStr = "gprint_str"
  show IrClear = "clear_vars"
  show IrCls = "SBJ 0xEE71"
  show IrRandom = "gen_random_seed"
  show (IrPoke memArea) =
    "poke_memory_area_" ++ show memArea
  show IrSetPokeAddress = "set_poke_address"
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
  show IrLeftOp = "LEFT AL-X, 7890H"
  show IrRightOp = "RIGHT AL-X, 7890H"
  show IrAsciiOp = "ASC AL-X"
  show IrPointOp = "point" -- TODO:
  show IrRndOp = "rnd" -- TODO:
  show IrIntOp = "SBJ 0xF5BE"
  show IrSgnOp = "SBJ 0xF59D"
  show IrAddOp = "SBJ 0xEFBA"
  show IrSubOp = "SBJ 0xEFB6"
  show IrMulOp = "SBJ 0xF01A"
  show IrDivOp = "SBJ 0xF084"
  show IrExponentOp = "SBJ 0xF89C"
  show IrAndOp = "AND AL-X AL-Y"
  show IrOrOp = "OR AL-X AL-Y"
  show IrEqOp = "A = 0x04\n\tSBJ 0xD0D2"
  show IrNeqOp = "A = 0x00\n\tSBJ 0xD0D2"
  show IrLtOp = "A = 0x01\n\tSBJ 0xD0D2"
  show IrLeqOp = "A = 0x05\n\tSBJ 0xD0D2"
  show IrGtOp = "A = 0x02\n\tSBJ 0xD0D2"
  show IrGeqOp = "A = 0x06\n\tSBJ 0xD0D2"

type Label = Int

data IrInst where
  IrLdImmIntoAlX :: Double -> IrInst
  IrLdImmIntoAlY :: Double -> IrInst
  IrLdImmIndirectIntoAlX :: Word16 -> IrInst
  IrLdImmIndirectIntoAlY :: Word16 -> IrInst
  IrAddrInYregIntoAlX :: IrInst
  IrStAlXInYreg :: IrInst
  IrAddrInAlXToYreg :: IrInst
  IrStAlXInImm :: Word16 -> IrInst
  IrAlXToAlY :: IrInst
  IrAlYToAlX :: IrInst
  IrImmToYreg :: Word16 -> IrInst
  IrImmToUreg :: Word16 -> IrInst
  IrAlXToAreg :: IrInst
  IrHalt :: IrInst
  IrAToTm0 :: IrInst
  IrAToTm1 :: IrInst
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

showWord16 :: Word16 -> String
showWord16 w = "0x" ++ showHexAllCaps
  where
    showHexAllCaps = map toUpper (showHex w "")

instance Show IrInst where
  show (IrLdImmIntoAlX num) =
    "\tAL-X = " ++ show num
  show (IrLdImmIntoAlY num) =
    "\tAL-Y = " ++ show num
  show (IrLdImmIndirectIntoAlX addr) =
    "\tAL-X = (" ++ showWord16 addr ++ ")"
  show (IrLdImmIndirectIntoAlY addr) =
    "\tAL-Y = (" ++ showWord16 addr ++ ")"
  show IrStAlXInYreg = "\t(Yreg) = AL-X"
  show IrAddrInAlXToYreg = "\tYreg = (AL-X as u16)"
  show IrAddrInYregIntoAlX = "\tAL-X = (Yreg)"
  show (IrImmToYreg imm) =
    "\tYreg = " ++ showWord16 imm
  show (IrImmToUreg imm) =
    "\tUreg = " ++ showWord16 imm
  show IrAlXToAlY = "\tAL-Y = AL-X"
  show IrAlYToAlX = "\tAL-X = AL-Y"
  show (IrLabel idx) = "L" ++ show idx ++ ":"
  show (IrGoto idx) = "\tJMP :L" ++ show idx
  show (IrCall idx) = "\tSJP :L" ++ show idx
  show (IrCondGoto idx) = "\tBZS :L" ++ show idx
  show (IrCondCall idx) = "\tif Z call :L" ++ show idx
  show IrReturn = "\tRTN"
  show (IrIntrinsicCall intrinsic) = "\t" ++ show intrinsic
  show (IrFun op) = "\t" ++ show op
  show IrHalt = "\tHLT"
  show IrAToTm0 = "\tTM0 = Areg"
  show IrAToTm1 = "\tTM1 = Areg"
  show IrAlXToAreg = "\tAreg = AL-X as u8"
  show (IrStAlXInImm addr) = "\t(" ++ showWord16 addr ++ ") = AL-X"

newtype IrProgram = IrProgram
  { irProgramStatements :: [IrInst]
  }
  deriving (Eq)

instance Show IrProgram where
  show (IrProgram stmts) = unlines (map show stmts)