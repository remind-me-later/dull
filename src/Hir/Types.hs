module Hir.Types
  ( HirIntrinsic (..),
    HirStackOps (..),
    HirInst (..),
    HirProgram (..),
  )
where

import Ast.Types (LValue)
import TypeSystem (BasicType)

data HirIntrinsic where
  HirPrint :: HirIntrinsic
  HirPause :: HirIntrinsic
  HirUsing :: String -> HirIntrinsic
  HirInput :: {hirInputDestination :: LValue BasicType} -> HirIntrinsic
  HirGPrint :: HirIntrinsic
  HirEnd :: HirIntrinsic
  HirClear :: HirIntrinsic
  HirCls :: HirIntrinsic
  HirRandom :: HirIntrinsic
  HirSetWait :: HirIntrinsic
  HirDoWait :: HirIntrinsic
  HirSetPokeAddress :: HirIntrinsic
  HirPoke :: {hirPokeMemoryArea :: Int} -> HirIntrinsic
  HirCursor :: HirIntrinsic
  HirGCursor :: HirIntrinsic
  HirBeepStmt :: {hirBeepHasOptParams :: Bool} -> HirIntrinsic
  deriving (Eq)

instance Show HirIntrinsic where
  show HirPrint = "print"
  show HirPause = "print_with_pause"
  show (HirUsing usingClause) = "using_fmt(" ++ show usingClause ++ ")"
  show (HirInput dest) = "wait_for_input " ++ show dest
  show HirGPrint = "gprint"
  show HirEnd = "exit"
  show HirClear = "clear_vars"
  show HirCls = "cls"
  show HirRandom = "gen_random_seed"
  show HirSetWait = "set_print_wait_time"
  show HirDoWait = "do_wait"
  show (HirPoke memArea) =
    "poke_memory_area_" ++ show memArea
  show HirSetPokeAddress = "set_poke_address"
  show HirCursor = "cursor"
  show HirGCursor = "gcursor"
  show (HirBeepStmt hasOptParams) =
    "beep" ++ (if hasOptParams then "_with_params" else "")

data HirStackOps where
  -- "Normal" operations
  -- Arithmetic operations
  HirAddOp :: HirStackOps
  HirSubOp :: HirStackOps
  HirMulOp :: HirStackOps
  HirDivOp :: HirStackOps
  HirExponentOp :: HirStackOps
  HirAndOp :: HirStackOps
  HirOrOp :: HirStackOps
  -- Comparison operations
  HirEqOp :: HirStackOps
  HirNeqOp :: HirStackOps
  HirLtOp :: HirStackOps
  HirLeqOp :: HirStackOps
  HirGtOp :: HirStackOps
  HirGeqOp :: HirStackOps
  -- "Special" operations
  HirMidOp :: HirStackOps
  HirLeftOp :: HirStackOps
  HirRightOp :: HirStackOps
  HirAsciiOp :: HirStackOps
  HirPointOp :: HirStackOps
  HirRndOp :: HirStackOps
  HirIntOp :: HirStackOps
  HirSgnOp :: HirStackOps
  deriving (Eq)

instance Show HirStackOps where
  show HirMidOp = "mid"
  show HirLeftOp = "left"
  show HirRightOp = "right"
  show HirAsciiOp = "ascii"
  show HirPointOp = "point"
  show HirRndOp = "rnd"
  show HirIntOp = "int"
  show HirSgnOp = "sgn"
  show HirAddOp = "add"
  show HirSubOp = "sub"
  show HirMulOp = "mul"
  show HirDivOp = "div"
  show HirExponentOp = "exp"
  show HirAndOp = "and"
  show HirOrOp = "or"
  show HirEqOp = "eq"
  show HirNeqOp = "neq"
  show HirLtOp = "lt"
  show HirLeqOp = "leq"
  show HirGtOp = "gt"
  show HirGeqOp = "geq"

data HirInst where
  -- Stack operations
  HirPushLValue :: LValue BasicType -> HirInst
  HirPushStrLit :: String -> HirInst
  HirPushNumLit :: Double -> HirInst
  HirPop :: LValue BasicType -> HirInst
  HirOp :: HirStackOps -> HirInst
  HirStackOps :: HirStackOps -> HirInst
  -- Labels
  HirLabel :: Int -> HirInst
  -- Jumps
  HirGoto :: Int -> HirInst
  HirCall :: Int -> HirInst
  HirCondGoto :: Int -> HirInst
  HirCondCall :: Int -> HirInst
  HirReturn :: HirInst
  -- Intrinsics
  HirIntrinsicCall :: HirIntrinsic -> HirInst
  deriving (Eq)

instance Show HirInst where
  show (HirLabel idx) = "L" ++ show idx ++ ":\n"
  show (HirGoto idx) = "\tgoto L" ++ show idx ++ "\n"
  show (HirCall idx) = "\tcall L" ++ show idx ++ "\n"
  show (HirCondGoto idx) = "\tgoto? L" ++ show idx ++ "\n"
  show (HirCondCall idx) = "\tcall? L" ++ show idx ++ "\n"
  show HirReturn = "\treturn\n"
  show (HirPop lvalue) = "\tpop " ++ show lvalue ++ "\n"
  show (HirIntrinsicCall intrinsic) = "\t@" ++ show intrinsic ++ "\n"
  show (HirPushLValue lvalue) = "\tpush " ++ show lvalue ++ "\n"
  show (HirPushStrLit str) = "\tpush \"" ++ str ++ "\"" ++ "\n"
  show (HirPushNumLit num) = "\tpush " ++ show num ++ "\n"
  show (HirOp op) = "\t" ++ show op ++ "\n"
  show (HirStackOps fun) = "\t" ++ show fun ++ "\n"

newtype HirProgram = HirProgram
  { hirProgramStatements :: [HirInst]
  }
  deriving (Eq)

instance Show HirProgram where
  show (HirProgram stmts) =
    "HirProgram:\n" ++ concatMap show stmts