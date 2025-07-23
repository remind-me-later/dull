module Hir.Types where

import Ast.Types
import TypeSystem (BasicType)

data HirStmt where
  HirLabel :: Int -> HirStmt
  HirGoto :: Int -> HirStmt
  HirCall :: Int -> HirStmt
  HirCondGoto :: Expr BasicType -> Int -> HirStmt
  HirCondCall :: Expr BasicType -> Int -> HirStmt
  HirAssign :: Assignment BasicType -> HirStmt
  HirPrint ::
    { hirPrintExpression :: Expr BasicType
    } ->
    HirStmt
  HirUsing :: UsingClause -> HirStmt
  HirInput ::
    { hirInputDestination :: Ident
    } ->
    HirStmt
  HirGPrint ::
    { hirGPrintExpr :: Expr BasicType
    } ->
    HirStmt
  HirReturn :: HirStmt
  HirEnd :: HirStmt
  HirClear :: HirStmt
  HirCls :: HirStmt
  HirRandom :: HirStmt
  HirWait ::
    { hirWaitTimeExpr :: Expr BasicType
    } ->
    HirStmt
  HirPoke ::
    { hirPokeMemoryArea :: PokeKind,
      hirPokeValue :: Expr BasicType
    } ->
    HirStmt
  HirCursor ::
    { hirCursorExpr :: Expr BasicType
    } ->
    HirStmt
  HirGCursor ::
    { hirGCursorExpr :: Expr BasicType
    } ->
    HirStmt
  HirBeepStmt ::
    { hirBeepStmtRepetitionsExpr :: Expr BasicType,
      hirBeepStmtOptionalParams :: Maybe (BeepOptionalParams BasicType)
    } ->
    HirStmt
  deriving (Eq)

instance Show HirStmt where
  show (HirLabel idx) = "L" ++ show idx ++ ":\n"
  show (HirGoto idx) = "\tgoto L" ++ show idx ++ "\n"
  show (HirCall idx) = "\tcall L" ++ show idx ++ "\n"
  show (HirCondGoto cond idx) = "\tif " ++ show cond ++ " goto L" ++ show idx ++ "\n"
  show (HirCondCall cond idx) = "\tif " ++ show cond ++ " call L" ++ show idx ++ "\n"
  show (HirAssign assignment) = "\t" ++ show assignment ++ "\n"
  show (HirPrint {hirPrintExpression}) =
    "\tputs(" ++ show hirPrintExpression ++ ")\n"
  show (HirUsing (UsingClause usingClauseVar)) = "\tusing_fmt(" ++ show usingClauseVar ++ ")\n"
  show (HirInput {hirInputDestination}) =
    "\tgets(" ++ show hirInputDestination ++ ")\n"
  show HirReturn = "\treturn\n"
  show HirEnd = "\texit()\n"
  show HirClear = "\tclear_vars()\n"
  show HirCls = "\tcls()\n"
  show HirRandom = "\tgen_random_seed()\n"
  show (HirGPrint {hirGPrintExpr}) =
    "\tgprint(" ++ show hirGPrintExpr ++ ")\n"
  show (HirWait {hirWaitTimeExpr}) =
    "\tset_print_wait_time(" ++ show hirWaitTimeExpr ++ ")\n"
  show (HirPoke {hirPokeMemoryArea, hirPokeValue}) =
    "\tpoke(me=" ++ (if hirPokeMemoryArea == Me1 then "1" else "0") ++ ", " ++ show hirPokeValue ++ ")\n"
  show (HirCursor {hirCursorExpr}) =
    "\tcursor(" ++ show hirCursorExpr ++ ")\n"
  show (HirGCursor {hirGCursorExpr}) =
    "\tgcursor(" ++ show hirGCursorExpr ++ ")\n"
  show (HirBeepStmt {hirBeepStmtRepetitionsExpr, hirBeepStmtOptionalParams}) =
    let params = case hirBeepStmtOptionalParams of
          Just (BeepOptionalParams {beepFrequency, beepDuration}) ->
            ", " ++ show beepFrequency ++ ", " ++ show beepDuration
          Nothing -> ""
     in "\tbeep(" ++ show hirBeepStmtRepetitionsExpr ++ params ++ ")\n"

newtype HirProgram = HirProgram
  { hirProgramStatements :: [HirStmt]
  }
  deriving (Eq)

instance Show HirProgram where
  show (HirProgram stmts) =
    "HirProgram:\n" ++ concatMap show stmts