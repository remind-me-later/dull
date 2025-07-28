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
  IrLoadStringHeader :: IrIntrinsic
  IrStoreStringHeader :: IrIntrinsic
  IrNumberToString :: IrIntrinsic
  IrStringToNumber :: IrIntrinsic
  IrPrintString :: IrIntrinsic
  IrFormatUsing :: String -> IrIntrinsic
  IrInputString :: IrIntrinsic
  IrGraphicPrintNumber :: IrIntrinsic
  IrGraphicPrintString :: IrIntrinsic
  IrClearVariables :: IrIntrinsic
  IrClearScreen :: IrIntrinsic
  IrRandomize :: IrIntrinsic
  IrSetPokeAddress :: IrIntrinsic
  IrPokeMemory :: {irPokeMemoryArea :: Int} -> IrIntrinsic
  IrSetGraphicCursor :: IrIntrinsic
  IrBeep :: {irBeepHasOptParams :: Bool} -> IrIntrinsic
  deriving (Eq)

instance Show IrIntrinsic where
  show IrLoadStringHeader = "SJP LOAD_STRING_HEADER_FROM_AL_X"
  show IrStoreStringHeader = "SJP STORE_STRING_HEADER_IN_AL_X"
  show IrNumberToString = "SJP NUM_TO_STR"
  show IrStringToNumber = "SJP STR_TO_NUM"
  show IrPrintString = "SJP 0xED00"
  show (IrFormatUsing usingClause) = "using_fmt(" ++ show usingClause ++ ")"
  show IrInputString = "SJP INPUT_STRING"
  show IrGraphicPrintNumber = "gprint_num"
  show IrGraphicPrintString = "gprint_str"
  show IrClearVariables = "clear_vars"
  show IrClearScreen = "SJP 0xEE71"
  show IrRandomize = "gen_random_seed"
  show (IrPokeMemory memArea) =
    "poke_memory_area_" ++ show memArea
  show IrSetPokeAddress = "set_poke_address"
  show IrSetGraphicCursor = "set_gcursor"
  show (IrBeep hasOptParams) =
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
  show IrIntOp = "SJP 0xF5BE"
  show IrSgnOp = "SJP 0xF59D"
  show IrAddOp = "SJP 0xEFBA"
  show IrSubOp = "SJP 0xEFB6"
  show IrMulOp = "SJP 0xF01A"
  show IrDivOp = "SJP 0xF084"
  show IrExponentOp = "SJP 0xF89C"
  show IrAndOp = "AND AL-X AL-Y"
  show IrOrOp = "OR AL-X AL-Y"
  show IrEqOp = "A = 0x04\n\tSJP 0xD0D2"
  show IrNeqOp = "A = 0x00\n\tSJP 0xD0D2"
  show IrLtOp = "A = 0x01\n\tSJP 0xD0D2"
  show IrLeqOp = "A = 0x05\n\tSJP 0xD0D2"
  show IrGtOp = "A = 0x02\n\tSJP 0xD0D2"
  show IrGeqOp = "A = 0x06\n\tSJP 0xD0D2"

type Label = Int

data IrInst where
  IrLoadImmediateToAlX :: Double -> IrInst
  IrLoadImmediateToAlY :: Double -> IrInst
  IrLoadFromYRegToAlX :: IrInst
  IrStoreAlXToYReg :: IrInst
  IrLoadAddressFromAlXToYReg :: IrInst
  IrStoreAlXToAddress :: Word16 -> IrInst
  IrCopyAlXToAlY :: IrInst
  IrCopyAlYToAlX :: IrInst
  IrLoadImmediateToYReg :: Word16 -> IrInst
  IrLoadImmediateToUReg :: Word16 -> IrInst
  IrCopyAlXToAReg :: IrInst
  IrHalt :: IrInst
  IrCopyARegToTimer0 :: IrInst
  IrCopyARegToTimer1 :: IrInst
  IrCallFunction :: IrFun -> IrInst
  IrPushYReg :: IrInst
  IrPopYReg :: IrInst
  IrPushAlX :: IrInst
  IrPopAlX :: IrInst
  IrPushAlY :: IrInst
  IrPopAlY :: IrInst
  -- Labels
  IrLabel :: Label -> IrInst
  -- Jumps
  IrGoto :: Label -> IrInst
  IrCall :: Label -> IrInst
  IrConditionalGoto :: Label -> IrInst
  IrConditionalCall :: Label -> IrInst
  IrReturn :: IrInst
  -- Intrinsics
  IrCallIntrinsic :: IrIntrinsic -> IrInst
  deriving (Eq)

showWord16 :: Word16 -> String
showWord16 w = "0x" ++ showHexAllCaps
  where
    showHexAllCaps = map toUpper (showHex w "")

instance Show IrInst where
  show (IrLoadImmediateToAlX num) =
    "\tAL-X = " ++ show num
  show (IrLoadImmediateToAlY num) =
    "\tAL-Y = " ++ show num
  show IrStoreAlXToYReg = "\t(Yreg) = AL-X"
  show IrLoadAddressFromAlXToYReg = "\tYreg = (AL-X as u16)"
  show IrLoadFromYRegToAlX = "\tAL-X = (Yreg)"
  show (IrLoadImmediateToYReg imm) =
    "\tYreg = " ++ showWord16 imm
  show (IrLoadImmediateToUReg imm) =
    "\tUreg = " ++ showWord16 imm
  show IrCopyAlXToAlY = "\tAL-Y = AL-X"
  show IrCopyAlYToAlX = "\tAL-X = AL-Y"
  show (IrLabel idx) = "L" ++ show idx ++ ":"
  show (IrGoto idx) = "\tJMP L" ++ show idx
  show (IrCall idx) = "\tSJP L" ++ show idx
  show (IrConditionalGoto idx) = "\tBZS L" ++ show idx
  show (IrConditionalCall idx) = "\tif Z call L" ++ show idx
  show IrReturn = "\tRTN"
  show (IrCallIntrinsic intrinsic) = "\t" ++ show intrinsic
  show (IrCallFunction op) = "\t" ++ show op
  show IrHalt = "\tHLT"
  show IrCopyARegToTimer0 = "\tTM0 = Areg"
  show IrCopyARegToTimer1 = "\tTM1 = Areg"
  show IrCopyAlXToAReg = "\tAreg = AL-X as u8"
  show (IrStoreAlXToAddress addr) = "\t(" ++ showWord16 addr ++ ") = AL-X"
  show IrPushYReg = "\tPSH Yreg"
  show IrPopYReg = "\tPOP Yreg"
  show IrPushAlX = "\tPSH AL-X"
  show IrPopAlX = "\tPOP AL-X"
  show IrPushAlY = "\tPSH AL-Y"
  show IrPopAlY = "\tPOP AL-Y"

newtype IrProgram = IrProgram
  { irProgramStatements :: [IrInst]
  }
  deriving (Eq)

instance Show IrProgram where
  show (IrProgram stmts) = unlines (map show stmts)