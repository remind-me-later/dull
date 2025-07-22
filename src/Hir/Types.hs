module Hir.Types where

import Ast.Types
import TypeSystem (BasicType)

data HirStmt where
  HirStmt :: Stmt BasicType -> HirStmt
  HirLabel :: Int -> HirStmt
  deriving (Eq)

instance Show HirStmt where
  show (HirStmt stmt) = "\t" ++ show stmt ++ "\n"
  show (HirLabel idx) = show idx ++ ":\n"

newtype HirProgram = HirProgram
  { hirProgramStatements :: [HirStmt]
  }
  deriving (Eq)

instance Show HirProgram where
  show (HirProgram stmts) =
    "HirProgram:\n" ++ concatMap show stmts