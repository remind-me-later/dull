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
    { hirPrintKind :: PrintKind,
      hirPrintExpression :: Expr BasicType,
      hirPrintEnding :: PrintEnding
    } ->
    HirStmt
  HirUsing :: UsingClause -> HirStmt
  HirInput ::
    { hirInputDestination :: Ident
    } ->
    HirStmt
  HirGPrint ::
    { hirGPrintExpr :: Expr BasicType,
      hirGPrintEnding :: PrintEnding
    } ->
    HirStmt
  HirReturn :: HirStmt
  HirEnd :: HirStmt
  HirClear :: HirStmt
  HirCls :: HirStmt
  HirRandom :: HirStmt
  deriving (Eq)

instance Show HirStmt where
  show (HirStmt stmt) = "\t" ++ show stmt ++ "\n"
  show (HirLabel idx) = "L" ++ show idx ++ ":\n"
  show (HirGoto idx) = "\tGOTO L" ++ show idx ++ "\n"
  show (HirGosub idx) = "\tGOSUB L" ++ show idx ++ "\n"
  show (HirCondGoto cond idx) = "\tIF " ++ show cond ++ " THEN GOTO L" ++ show idx ++ "\n"
  show (HirCondGosub cond idx) = "\tIF " ++ show cond ++ " THEN GOSUB L" ++ show idx ++ "\n"
  show (HirAssign assignment) = "\t" ++ show assignment ++ "\n"
  show (HirPrint {hirPrintKind, hirPrintExpression, hirPrintEnding}) =
    case hirPrintEnding of
      PrintEndingNewLine ->
        "\t" ++ show hirPrintKind ++ "LN " ++ show hirPrintExpression ++ "\n"
      PrintEndingNoNewLine ->
        "\t" ++ show hirPrintKind ++ " " ++ show hirPrintExpression ++ "\n"
  show (HirUsing usingClause) = "\t" ++ show usingClause ++ "\n"
  show (HirInput {hirInputDestination}) =
    "\tINPUT " ++ show hirInputDestination ++ "\n"
  show HirReturn = "\tRETURN\n"
  show HirEnd = "\tEND\n"
  show HirClear = "\tCLEAR\n"
  show HirCls = "\tCLS\n"
  show HirRandom = "\tRANDOM\n"
  show (HirGPrint {hirGPrintExpr, hirGPrintEnding}) =
    case hirGPrintEnding of
      PrintEndingNewLine ->
        "\tGPRINTLN " ++ show hirGPrintExpr ++ "\n"
      PrintEndingNoNewLine ->
        "\tGPRINT " ++ show hirGPrintExpr ++ "\n"

newtype HirProgram = HirProgram
  { hirProgramStatements :: [HirStmt]
  }
  deriving (Eq)

instance Show HirProgram where
  show (HirProgram stmts) =
    "HirProgram:\n" ++ concatMap show stmts