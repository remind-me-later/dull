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
  deriving (Eq)

instance Show HirStmt where
  show (HirStmt stmt) = "\t" ++ show stmt ++ "\n"
  show (HirLabel idx) = "L" ++ show idx ++ ":\n"
  show (HirGoto idx) = "\tGOTO L" ++ show idx ++ "\n"
  show (HirGosub idx) = "\tGOSUB L" ++ show idx ++ "\n"
  show (HirCondGoto cond idx) = "\tIF " ++ show cond ++ " THEN GOTO L" ++ show idx ++ "\n"
  show (HirCondGosub cond idx) = "\tIF " ++ show cond ++ " THEN GOSUB L" ++ show idx ++ "\n"
  show (HirAssign assignment) = "\t" ++ show assignment ++ "\n"

newtype HirProgram = HirProgram
  { hirProgramStatements :: [HirStmt]
  }
  deriving (Eq)

instance Show HirProgram where
  show (HirProgram stmts) =
    "HirProgram:\n" ++ concatMap show stmts