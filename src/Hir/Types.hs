-- In this representation all "words" are 8 bytes long, the size of the BASIC
-- variables

module Hir.Types
  ( HirIntrinsic (..),
    HirStackOps (..),
    HirInst (..),
    HirProgram (..),
    HirIdent (..),
    HirOperand (..),
  )
where

data HirIdent where
  HirBasicIdent ::
    { hirIdentName :: Char,
      hirIdentHasDollar :: Bool
    } ->
    HirIdent
  -- These are identifiers that we include in the Hir code but that
  -- don't exist in the original source code.
  HirFakeIdent ::
    { hirFakeIdentName :: String
    } ->
    HirIdent
  deriving (Eq)

instance Show HirIdent where
  show (HirBasicIdent name hasDollar) =
    if hasDollar then [name, '$'] else [name]
  show (HirFakeIdent name) = '!' : name

data HirOperand where
  HirOperandNumLit :: Double -> HirOperand
  HirOperandStrLitAddr :: String -> HirOperand
  HirOperandVarAddr :: HirIdent -> HirOperand
  deriving (Eq)

instance Show HirOperand where
  show (HirOperandNumLit num) = show num
  show (HirOperandStrLitAddr offset) = '&' : show offset
  show (HirOperandVarAddr ident) = '&' : show ident

data HirIntrinsic where
  HirPrintNum :: HirIntrinsic
  HirPrintStr :: HirIntrinsic
  HirUsing :: String -> HirIntrinsic
  HirInputNum :: HirIntrinsic
  HirInputStr :: HirIntrinsic
  HirGPrintNum :: HirIntrinsic
  HirGPrintStr :: HirIntrinsic
  HirClear :: HirIntrinsic
  HirCls :: HirIntrinsic
  HirRandom :: HirIntrinsic
  HirSleep :: HirIntrinsic
  HirSetPokeAddress :: HirIntrinsic
  HirPoke :: {hirPokeMemoryArea :: Int} -> HirIntrinsic
  HirCursor :: HirIntrinsic
  HirGCursor :: HirIntrinsic
  HirBeepStmt :: {hirBeepHasOptParams :: Bool} -> HirIntrinsic
  deriving (Eq)

instance Show HirIntrinsic where
  show HirPrintNum = "print_num"
  show HirPrintStr = "print_str"
  show (HirUsing usingClause) = "using_fmt(" ++ show usingClause ++ ")"
  show HirInputNum = "input_num"
  show HirInputStr = "input_str"
  show HirGPrintNum = "gprint_num"
  show HirGPrintStr = "gprint_str"
  show HirClear = "clear_vars"
  show HirCls = "cls"
  show HirRandom = "gen_random_seed"
  show HirSleep = "sleep"
  show (HirPoke memArea) =
    "poke_memory_area_" ++ show memArea
  show HirSetPokeAddress = "set_poke_address"
  show HirCursor = "set_cursor"
  show HirGCursor = "set_gcursor"
  show (HirBeepStmt hasOptParams) =
    "beep" ++ (if hasOptParams then "_with_params" else "")

data HirStackOps where
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

type Label = Int

data HirInst where
  -- Stack operations
  HirPush :: HirOperand -> HirInst
  -- Takes a direction and an operand and assigns it to that direction
  HirAssign :: HirInst
  HirOp :: HirStackOps -> HirInst
  HirDeref :: HirInst
  -- Labels
  HirLabel :: Label -> HirInst
  -- Jumps
  HirGoto :: Label -> HirInst
  HirCall :: Label -> HirInst
  HirCondGoto :: Label -> HirInst
  HirCondCall :: Label -> HirInst
  HirReturn :: HirInst
  -- Intrinsics
  HirIntrinsicCall :: HirIntrinsic -> HirInst
  deriving (Eq)

instance Show HirInst where
  show (HirLabel idx) = "L" ++ show idx ++ ":"
  show (HirGoto idx) = "\tgoto L" ++ show idx
  show (HirCall idx) = "\tcall L" ++ show idx
  show (HirCondGoto idx) = "\tgoto? L" ++ show idx
  show (HirCondCall idx) = "\tcall? L" ++ show idx
  show HirReturn = "\treturn"
  show HirAssign = "\tassign"
  show (HirIntrinsicCall intrinsic) = "\t@" ++ show intrinsic
  show (HirPush operand) =
    "\tpush " ++ show operand
  show (HirOp op) = "\t" ++ show op
  show HirDeref = "\tderef"

newtype HirProgram = HirProgram
  { hirProgramStatements :: [HirInst]
  }
  deriving (Eq)

instance Show HirProgram where
  show (HirProgram stmts) = unlines (map show stmts)