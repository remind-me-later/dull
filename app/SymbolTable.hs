module SymbolTable where

import Parser
  ( Assignment (..),
    Expr (..),
    Ident (..),
    Line (..),
    Program (..),
    Stmt (..),
  )

-- Types for our symbol tables
data VarInfo = VarInfo
  { varName :: String,
    varIsNumeric :: Bool,
    varFirstLine :: Double
  }
  deriving (Show, Eq)

data StrConstInfo = StrConstInfo
  { strValue :: String,
    strFirstLine :: Double
  }
  deriving (Show, Eq)

-- New data type for tracking DATA statements
data DataInfo = DataInfo
  { dataLineNum :: Double,
    dataValues :: [Expr]
  }
  deriving (Show, Eq)

-- Get all variables from an expression
getVarsFromExpr :: Double -> Expr -> [VarInfo]
getVarsFromExpr lineNum expr = case expr of
  BinExpr left _ right -> getVarsFromExpr lineNum left ++ getVarsFromExpr lineNum right
  VarExpr (NumIdent s) -> [VarInfo s True lineNum]
  VarExpr (StrIdent s) -> [VarInfo s False lineNum]
  FunCallExpr _ args -> concatMap (getVarsFromExpr lineNum) args
  _ -> []

-- Get all string constants from an expression
getStrConstsFromExpr :: Double -> Expr -> [StrConstInfo]
getStrConstsFromExpr lineNum expr = case expr of
  BinExpr left _ right -> getStrConstsFromExpr lineNum left ++ getStrConstsFromExpr lineNum right
  StrLitExpr s -> [StrConstInfo s lineNum]
  FunCallExpr _ args -> concatMap (getStrConstsFromExpr lineNum) args
  _ -> []

-- Extract variables and string constants from statements
getVarsFromStmt :: Double -> Stmt -> [VarInfo]
getVarsFromStmt lineNum stmt = case stmt of
  LetStmt assignments -> concatMap getVarsFromAssignment assignments
    where
      getVarsFromAssignment (Assignment ident val) =
        let idVar = case ident of
              NumIdent s -> [VarInfo s True lineNum]
              StrIdent s -> [VarInfo s False lineNum]
         in idVar ++ getVarsFromExpr lineNum val
  IfStmt cond thenStmt ->
    getVarsFromExpr lineNum cond ++ getVarsFromStmt lineNum thenStmt
  PrintStmt _ exprs _ ->
    concatMap (getVarsFromExpr lineNum) exprs
  InputStmt maybePrompt targetVar ->
    let promptVars = maybe [] (getVarsFromExpr lineNum) maybePrompt
        targetVars = getVarsFromExpr lineNum targetVar
     in promptVars ++ targetVars
  ForStmt (Assignment ident val) toExpr ->
    let idVar = case ident of
          NumIdent s -> [VarInfo s True lineNum]
          StrIdent s -> [VarInfo s False lineNum]
     in idVar ++ getVarsFromExpr lineNum val ++ getVarsFromExpr lineNum toExpr
  NextStmt ident ->
    case ident of
      NumIdent s -> [VarInfo s True lineNum]
      StrIdent s -> [VarInfo s False lineNum]
  GprintStmt exprs _ ->
    concatMap (getVarsFromExpr lineNum) exprs
  GCursorStmt expr ->
    getVarsFromExpr lineNum expr
  CursorStmt expr ->
    getVarsFromExpr lineNum expr
  BeepStmt exprs ->
    concatMap (getVarsFromExpr lineNum) exprs
  PokeStmt _ exprs ->
    concatMap (getVarsFromExpr lineNum) exprs
  DimStmt expr ->
    getVarsFromExpr lineNum expr
  ReadStmt exprs ->
    concatMap (getVarsFromExpr lineNum) exprs
  DataStmt exprs ->
    concatMap (getVarsFromExpr lineNum) exprs
  RestoreStmt maybeExpr ->
    maybe [] (getVarsFromExpr lineNum) maybeExpr
  WaitStmt maybeExpr ->
    maybe [] (getVarsFromExpr lineNum) maybeExpr
  _ -> []

-- Extract string constants from statements
getStrConstsFromStmt :: Double -> Stmt -> [StrConstInfo]
getStrConstsFromStmt lineNum stmt = case stmt of
  LetStmt assignments ->
    concatMap (\(Assignment _ val) -> getStrConstsFromExpr lineNum val) assignments
  IfStmt cond thenStmt ->
    getStrConstsFromExpr lineNum cond ++ getStrConstsFromStmt lineNum thenStmt
  PrintStmt _ exprs _ ->
    concatMap (getStrConstsFromExpr lineNum) exprs
  InputStmt maybePrompt targetVar ->
    let promptStrs = maybe [] (getStrConstsFromExpr lineNum) maybePrompt
        targetStrs = getStrConstsFromExpr lineNum targetVar
     in promptStrs ++ targetStrs
  ForStmt (Assignment _ val) toExpr ->
    getStrConstsFromExpr lineNum val ++ getStrConstsFromExpr lineNum toExpr
  GprintStmt exprs _ ->
    concatMap (getStrConstsFromExpr lineNum) exprs
  GCursorStmt expr ->
    getStrConstsFromExpr lineNum expr
  CursorStmt expr ->
    getStrConstsFromExpr lineNum expr
  BeepStmt exprs ->
    concatMap (getStrConstsFromExpr lineNum) exprs
  PokeStmt _ exprs ->
    concatMap (getStrConstsFromExpr lineNum) exprs
  DimStmt expr ->
    getStrConstsFromExpr lineNum expr
  ReadStmt exprs ->
    concatMap (getStrConstsFromExpr lineNum) exprs
  DataStmt exprs ->
    concatMap (getStrConstsFromExpr lineNum) exprs
  RestoreStmt maybeExpr ->
    maybe [] (getStrConstsFromExpr lineNum) maybeExpr
  WaitStmt maybeExpr ->
    maybe [] (getStrConstsFromExpr lineNum) maybeExpr
  _ -> []

-- Extract DATA statements from statements
getDataStmts :: Double -> Stmt -> [DataInfo]
getDataStmts lineNum stmt = case stmt of
  DataStmt exprs -> [DataInfo lineNum exprs]
  IfStmt _ thenStmt -> getDataStmts lineNum thenStmt
  _ -> []

-- Generate variable, string constant, and data tables from a program
generateSymbolTables :: Program -> ([VarInfo], [StrConstInfo], [DataInfo])
generateSymbolTables (Program lines) =
  let lineVars = concatMap getVarsFromLine lines
      lineStrConsts = concatMap getStrConstsFromLine lines
      lineDataStmts = concatMap getDataFromLine lines
      -- Remove duplicates, keeping the first occurrence based on variable name
      uniqueVars = nubBy (\v1 v2 -> varName v1 == varName v2) lineVars
      uniqueStrConsts = nubBy (\s1 s2 -> strValue s1 == strValue s2) lineStrConsts
   in (uniqueVars, uniqueStrConsts, lineDataStmts)
  where
    getVarsFromLine (Line num _ stmts) = concatMap (getVarsFromStmt num) stmts
    getStrConstsFromLine (Line num _ stmts) = concatMap (getStrConstsFromStmt num) stmts
    getDataFromLine (Line num _ stmts) = concatMap (getDataStmts num) stmts

-- Helper function to remove duplicates while keeping the first occurrence
nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy _ [] = []
nubBy eq (x : xs) = x : nubBy eq (filter (not . eq x) xs)

-- Pretty print functions for the symbol tables
prettyPrintVarTable :: [VarInfo] -> String
prettyPrintVarTable vars =
  "Variables:\n"
    ++ "Name\tType\tFirst Line\n"
    ++ "-----------------------------\n"
    ++ concatMap
      ( \var ->
          varName var
            ++ "\t"
            ++ (if varIsNumeric var then "Number" else "String")
            ++ "\t"
            ++ show (varFirstLine var)
            ++ "\n"
      )
      vars

prettyPrintStrConstTable :: [StrConstInfo] -> String
prettyPrintStrConstTable strs =
  "String Constants:\n"
    ++ "Value\t\tFirst Line\n"
    ++ "-----------------------------\n"
    ++ concatMap
      ( \str ->
          show (strValue str)
            ++ "\t\t"
            ++ show (strFirstLine str)
            ++ "\n"
      )
      strs

-- Pretty print function for DATA statements
prettyPrintDataTable :: [DataInfo] -> String
prettyPrintDataTable dataInfos =
  "DATA Statements:\n"
    ++ "Line\tValues\n"
    ++ "-----------------------------\n"
    ++ concatMap
      ( \info ->
          show (dataLineNum info)
            ++ "\t"
            ++ formatDataValues (dataValues info)
            ++ "\n"
      )
      dataInfos
  where
    formatDataValues :: [Expr] -> String
    formatDataValues = concatMap showValueWithComma

    showValueWithComma :: Expr -> String
    showValueWithComma expr =
      let value = case expr of
            NumLitExpr n -> show n
            StrLitExpr s -> "\"" ++ s ++ "\""
            _ -> show expr
       in value ++ ", "