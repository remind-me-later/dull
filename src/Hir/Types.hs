module Hir.Types where

import Ast.Types
import TypeSystem (BasicType)

data HirIntrinsic where
  HirPrint ::
    { hirPrintExpression :: Expr BasicType
    } ->
    HirIntrinsic
  HirUsing :: UsingClause -> HirIntrinsic
  HirInput ::
    { hirInputDestination :: Ident
    } ->
    HirIntrinsic
  HirGPrint ::
    { hirGPrintExpr :: Expr BasicType
    } ->
    HirIntrinsic
  HirEnd :: HirIntrinsic
  HirClear :: HirIntrinsic
  HirCls :: HirIntrinsic
  HirRandom :: HirIntrinsic
  HirWait ::
    { hirWaitTimeExpr :: Expr BasicType
    } ->
    HirIntrinsic
  HirPoke ::
    { hirPokeMemoryArea :: PokeKind,
      hirPokeValue :: Expr BasicType
    } ->
    HirIntrinsic
  HirCursor ::
    { hirCursorExpr :: Expr BasicType
    } ->
    HirIntrinsic
  HirGCursor ::
    { hirGCursorExpr :: Expr BasicType
    } ->
    HirIntrinsic
  HirBeepStmt ::
    { hirBeepStmtRepetitionsExpr :: Expr BasicType,
      hirBeepStmtOptionalParams :: Maybe (BeepOptionalParams BasicType)
    } ->
    HirIntrinsic
  deriving (Eq)

instance Show HirIntrinsic where
  show (HirPrint expr) = "puts(" ++ show expr ++ ")"
  show (HirUsing usingClause) = "using_fmt(" ++ show usingClause ++ ")"
  show (HirInput dest) = "gets(" ++ show dest ++ ")"
  show (HirGPrint expr) = "gprint(" ++ show expr ++ ")"
  show HirEnd = "exit()"
  show HirClear = "clear_vars()"
  show HirCls = "cls()"
  show HirRandom = "gen_random_seed()"
  show (HirWait expr) = "set_print_wait_time(" ++ show expr ++ ")"
  show (HirPoke memArea value) =
    "poke(me=" ++ (if memArea == Me1 then "1" else "0") ++ ", " ++ show value ++ ")"
  show (HirCursor expr) = "cursor(" ++ show expr ++ ")"
  show (HirGCursor expr) = "gcursor(" ++ show expr ++ ")"
  show (HirBeepStmt repetitions optionalParams) =
    let params = case optionalParams of
          Just (BeepOptionalParams {beepFrequency, beepDuration}) ->
            ", " ++ show beepFrequency ++ ", " ++ show beepDuration
          Nothing -> ""
     in "beep(" ++ show repetitions ++ params ++ ")"

data HirIntrinsicFun where
  HirMidFun :: HirIntrinsicFun
  HirLeftFun :: HirIntrinsicFun
  HirRightFun :: HirIntrinsicFun
  HirAsciiFun :: HirIntrinsicFun
  HirPointFun :: HirIntrinsicFun
  HirRndFun :: HirIntrinsicFun
  HirIntFun :: HirIntrinsicFun
  HirSgnFun :: HirIntrinsicFun
  deriving (Eq)

instance Show HirIntrinsicFun where
  show HirMidFun = "mid()"
  show HirLeftFun = "left()"
  show HirRightFun = "right()"
  show HirAsciiFun = "ascii()"
  show HirPointFun = "point()"
  show HirRndFun = "rnd()"
  show HirIntFun = "int()"
  show HirSgnFun = "sgn()"

data HirInst where
  -- Stack operations
  HirPushLValue :: LValue BasicType -> HirInst
  HirPushStrLit :: String -> HirInst
  HirPushNumLit :: Double -> HirInst
  HirPopStr :: HirInst
  HirPopNum :: HirInst
  HirBinOp :: BinOperator -> HirInst
  HirUnaryOp :: UnaryOperator -> HirInst
  HirIntrinsicFun :: HirIntrinsicFun -> HirInst
  -- Assignment
  HirAssign :: Assignment BasicType -> HirInst
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
  show (HirAssign assignment) = "\t" ++ show assignment ++ "\n"
  show (HirIntrinsicCall intrinsic) = "\tintrinsic " ++ show intrinsic ++ "\n"
  show (HirPushLValue lvalue) = "\tpush " ++ show lvalue ++ "\n"
  show (HirPushStrLit str) = "\tpush \"" ++ str ++ "\"" ++ "\n"
  show (HirPushNumLit num) = "\tpush " ++ show num ++ "\n"
  show HirPopStr = "\tpop str\n"
  show HirPopNum = "\tpop num\n"
  show (HirBinOp op) = "\tbinop " ++ show op ++ "\n"
  show (HirUnaryOp op) = "\tunaryop " ++ show op ++ "\n"
  show (HirIntrinsicFun fun) = "\t" ++ show fun ++ "\n"

newtype HirProgram = HirProgram
  { hirProgramStatements :: [HirInst]
  }
  deriving (Eq)

instance Show HirProgram where
  show (HirProgram stmts) =
    "HirProgram:\n" ++ concatMap show stmts