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
  HirReturn :: HirIntrinsic
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
  show HirReturn = "return"
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



data HirInst where
  -- Stack operations
  HirPushIdent :: Ident -> HirInst
  HirPushStrLit :: String -> HirInst
  HirPushNumLit :: Double -> HirInst
  HirPopStr :: HirInst
  HirPopNum :: HirInst
  -- Assignment
  HirAssign :: Assignment BasicType -> HirInst
  -- Labels
  HirLabel :: Int -> HirInst
  -- Jumps
  HirGoto :: Int -> HirInst
  HirCall :: Int -> HirInst
  HirCondGoto :: Expr BasicType -> Int -> HirInst
  HirCondCall :: Expr BasicType -> Int -> HirInst
  -- Intrinsics
  HirIntrinsicCall :: HirIntrinsic -> HirInst
  deriving (Eq)

instance Show HirInst where
  show (HirLabel idx) = "L" ++ show idx ++ ":\n"
  show (HirGoto idx) = "\tgoto L" ++ show idx ++ "\n"
  show (HirCall idx) = "\tcall L" ++ show idx ++ "\n"
  show (HirCondGoto cond idx) = "\tif " ++ show cond ++ " goto L" ++ show idx ++ "\n"
  show (HirCondCall cond idx) = "\tif " ++ show cond ++ " call L" ++ show idx ++ "\n"
  show (HirAssign assignment) = "\t" ++ show assignment ++ "\n"
  show (HirIntrinsicCall intrinsic) = "\tintrinsic " ++ show intrinsic ++ "\n"

newtype HirProgram = HirProgram
  { hirProgramStatements :: [HirInst]
  }
  deriving (Eq)

instance Show HirProgram where
  show (HirProgram stmts) =
    "HirProgram:\n" ++ concatMap show stmts