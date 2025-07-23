module Hir.Types where

import Ast.Types
import TypeSystem (BasicType)

data HirStmt where
  HirStmt :: Stmt BasicType -> HirStmt
  HirLabel :: Int -> HirStmt
  HirGoto :: Int -> HirStmt
  HirGosub :: Int -> HirStmt
  HirCondGoto :: Expr BasicType -> Int -> HirStmt
  HirCondGosub :: Expr BasicType -> Int -> HirStmt
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
  deriving (Eq)

instance Show HirStmt where
  show (HirStmt stmt) = "\t" ++ show stmt ++ "\n"
  show (HirLabel idx) = "L" ++ show idx ++ ":\n"
  show (HirGoto idx) = "\tgoto L" ++ show idx ++ "\n"
  show (HirGosub idx) = "\tgosub L" ++ show idx ++ "\n"
  show (HirCondGoto cond idx) = "\tif " ++ show cond ++ " goto L" ++ show idx ++ "\n"
  show (HirCondGosub cond idx) = "\tif " ++ show cond ++ " gosub L" ++ show idx ++ "\n"
  show (HirAssign assignment) = "\t" ++ show assignment ++ "\n"
  show (HirPrint {hirPrintExpression}) =
    "\tputs(" ++ show hirPrintExpression ++ ")\n"
  show (HirUsing usingClause) = "\t" ++ show usingClause ++ "\n"
  show (HirInput {hirInputDestination}) =
    "\tgets(" ++ show hirInputDestination ++ ")\n"
  show HirReturn = "\treturn\n"
  show HirEnd = "\tend\n"
  show HirClear = "\tclear_vars()\n"
  show HirCls = "\tcls()\n"
  show HirRandom = "\tgen_random_seed()\n"
  show (HirGPrint {hirGPrintExpr}) =
    "\tgprint(" ++ show hirGPrintExpr ++ ")\n"
  show (HirWait {hirWaitTimeExpr}) =
    "\tset_print_wait_time(" ++ show hirWaitTimeExpr ++ ")\n"

newtype HirProgram = HirProgram
  { hirProgramStatements :: [HirStmt]
  }
  deriving (Eq)

instance Show HirProgram where
  show (HirProgram stmts) =
    "HirProgram:\n" ++ concatMap show stmts