module Hir.Types where

import Ast.Types
import TypeSystem (BasicType)

data HirIntrinsic where
  HirPrint :: HirIntrinsic
  HirPause :: HirIntrinsic
  HirUsing :: String -> HirIntrinsic
  HirInput :: {hirInputDestination :: Ident} -> HirIntrinsic
  HirGPrint :: HirIntrinsic
  HirEnd :: HirIntrinsic
  HirClear :: HirIntrinsic
  HirCls :: HirIntrinsic
  HirRandom :: HirIntrinsic
  HirWait :: HirIntrinsic
  HirSetPokeAddress :: HirIntrinsic
  HirPoke :: {hirPokeMemoryArea :: PokeKind} -> HirIntrinsic
  HirCursor :: HirIntrinsic
  HirGCursor :: HirIntrinsic
  HirBeepStmt :: {hirBeepHasOptParams :: Bool} -> HirIntrinsic
  deriving (Eq)

instance Show HirIntrinsic where
  show HirPrint = "print"
  show HirPause = "print_with_pause"
  show (HirUsing usingClause) = "using_fmt(" ++ show usingClause ++ ")"
  show (HirInput dest) = "input " ++ show dest
  show HirGPrint = "gprint"
  show HirEnd = "exit"
  show HirClear = "clear_vars"
  show HirCls = "cls"
  show HirRandom = "gen_random_seed"
  show HirWait = "set_print_wait_time"
  show (HirPoke memArea) =
    "poke_memory_area_" ++ (if memArea == Me1 then "1" else "0")
  show HirSetPokeAddress = "set_poke_address"
  show HirCursor = "cursor"
  show HirGCursor = "gcursor"
  show (HirBeepStmt hasOptParams) =
    "beep" ++ (if hasOptParams then "_with_params" else "")

data HirStackOps where
  HirMidFun :: HirStackOps
  HirLeftFun :: HirStackOps
  HirRightFun :: HirStackOps
  HirAsciiFun :: HirStackOps
  HirPointFun :: HirStackOps
  HirRndFun :: HirStackOps
  HirIntFun :: HirStackOps
  HirSgnFun :: HirStackOps
  deriving (Eq)

instance Show HirStackOps where
  show HirMidFun = "mid"
  show HirLeftFun = "left"
  show HirRightFun = "right"
  show HirAsciiFun = "ascii"
  show HirPointFun = "point"
  show HirRndFun = "rnd"
  show HirIntFun = "int"
  show HirSgnFun = "sgn"

data HirInst where
  -- Stack operations
  HirPushLValue :: LValue BasicType -> HirInst
  HirPushStrLit :: String -> HirInst
  HirPushNumLit :: Double -> HirInst
  HirPop :: LValue BasicType -> HirInst
  HirBinOp :: BinOperator -> HirInst
  HirUnaryOp :: UnaryOperator -> HirInst
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
  show (HirCondGoto idx) = "\tcgoto L" ++ show idx ++ "\n"
  show (HirCondCall idx) = "\tccall L" ++ show idx ++ "\n"
  show HirReturn = "\treturn\n"
  show (HirPop lvalue) = "\tpop " ++ show lvalue ++ "\n"
  show (HirIntrinsicCall intrinsic) = "\tintrinsic " ++ show intrinsic ++ "\n"
  show (HirPushLValue lvalue) = "\tpush " ++ show lvalue ++ "\n"
  show (HirPushStrLit str) = "\tpush \"" ++ str ++ "\"" ++ "\n"
  show (HirPushNumLit num) = "\tpush " ++ show num ++ "\n"
  show (HirBinOp op) = "\t" ++ show op ++ "%2\n"
  show (HirUnaryOp op) = "\t" ++ show op ++ "%1\n"
  show (HirStackOps fun) = "\t" ++ show fun ++ "\n"

newtype HirProgram = HirProgram
  { hirProgramStatements :: [HirInst]
  }
  deriving (Eq)

instance Show HirProgram where
  show (HirProgram stmts) =
    "HirProgram:\n" ++ concatMap show stmts