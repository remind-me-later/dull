{-# LANGUAGE LambdaCase #-}

module Ast.Types where

import Data.List (intercalate)

data Operation where
  Add :: Operation
  Subtract :: Operation
  Multiply :: Operation
  Divide :: Operation
  Equal :: Operation
  LessThan :: Operation
  GreaterThan :: Operation
  LessThanOrEqual :: Operation
  GreaterThanOrEqual :: Operation
  NotEqual :: Operation
  Or :: Operation
  And :: Operation
  Caret :: Operation
  deriving (Eq)

instance Show Operation where
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide = "/"
  show Equal = "="
  show LessThanOrEqual = "<="
  show GreaterThanOrEqual = ">="
  show NotEqual = "<>"
  show LessThan = "<"
  show GreaterThan = ">"
  show Or = "OR"
  show And = "AND"
  show Caret = "^"

data Punctuation where
  Comma :: Punctuation
  Colon :: Punctuation
  SemiColon :: Punctuation
  LeftParen :: Punctuation
  RightParen :: Punctuation
  NewLine :: Punctuation
  Dollar :: Punctuation
  Dot :: Punctuation
  Hashtag :: Punctuation
  deriving (Eq)

instance Show Punctuation where
  show Comma = ","
  show Colon = ":"
  show SemiColon = ";"
  show LeftParen = "("
  show RightParen = ")"
  show NewLine = "\n"
  show Dollar = "$"
  show Dot = "."
  show Hashtag = "#"

data StmtKeyword where
  -- Statements
  Let :: StmtKeyword
  If :: StmtKeyword
  Then :: StmtKeyword
  Input :: StmtKeyword
  Print :: StmtKeyword
  End :: StmtKeyword
  Remark :: StmtKeyword
  For :: StmtKeyword
  To :: StmtKeyword
  Next :: StmtKeyword
  Clear :: StmtKeyword
  Goto :: StmtKeyword
  Gosub :: StmtKeyword
  Wait :: StmtKeyword
  Pause :: StmtKeyword
  Cls :: StmtKeyword
  Random :: StmtKeyword
  Gprint :: StmtKeyword
  GCursor :: StmtKeyword
  Cursor :: StmtKeyword
  Beep :: StmtKeyword
  Using :: StmtKeyword
  Return :: StmtKeyword
  Poke :: StmtKeyword
  Dim :: StmtKeyword
  Read :: StmtKeyword
  Data :: StmtKeyword
  Restore :: StmtKeyword
  deriving (Eq)

instance Show StmtKeyword where
  show Let = "LET"
  show If = "IF"
  show Then = "THEN"
  show Input = "INPUT"
  show Print = "PRINT"
  show End = "END"
  show Remark = "REM"
  show For = "FOR"
  show To = "TO"
  show Next = "NEXT"
  show Clear = "CLEAR"
  show Goto = "GOTO"
  show Gosub = "GOSUB"
  show Wait = "WAIT"
  show Pause = "PAUSE"
  show Cls = "CLS"
  show Random = "RANDOM"
  show Gprint = "GPRINT"
  show Cursor = "CURSOR"
  show GCursor = "GCURSOR"
  show Beep = "BEEP"
  show Using = "USING"
  show Return = "RETURN"
  show Poke = "POKE"
  show Dim = "DIM"
  show Read = "READ"
  show Data = "DATA"
  show Restore = "RESTORE"

data FunctionArity where
  Arity0 :: FunctionArity
  Arity1 :: FunctionArity
  Arity2 :: FunctionArity
  Arity3 :: FunctionArity
  deriving (Show, Eq)

data FunctionName where
  -- String functions
  MidStr :: FunctionArity -> FunctionName
  LeftStr :: FunctionArity -> FunctionName
  RightStr :: FunctionArity -> FunctionName
  InkeyStr :: FunctionArity -> FunctionName
  -- Integer functions
  Point :: FunctionArity -> FunctionName
  Rnd :: FunctionArity -> FunctionName
  Int :: FunctionArity -> FunctionName
  deriving (Eq)

instance Show FunctionName where
  show (MidStr _) = "MID$"
  show (LeftStr _) = "LEFT$"
  show (RightStr _) = "RIGHT$"
  show (Point _) = "POINT"
  show (Rnd _) = "RND"
  show (InkeyStr _) = "INKEY$"
  show (Int _) = "INT"

data Ident where
  NumVar :: String -> Ident
  StrVar :: String -> Ident
  deriving (Eq)

instance Show Ident where
  show (NumVar s) = s
  show (StrVar s) = s ++ "$"

data Expr where
  BinExpr :: Expr -> Operation -> Expr -> Expr
  NumLitExpr :: Double -> Expr
  VarExpr :: Ident -> Expr
  StrLitExpr :: String -> Expr
  FunCallExpr :: FunctionName -> [Expr] -> Expr
  deriving (Eq)

instance Show Expr where
  show (BinExpr left op right) = "(" ++ show left ++ " " ++ show op ++ " " ++ show right ++ ")"
  show (NumLitExpr n) = show n
  show (VarExpr x) = show x
  show (StrLitExpr s) = "\"" ++ s ++ "\""
  show (FunCallExpr f args) = show f ++ "(" ++ intercalate ", " (show <$> args) ++ ")"

data PrintEnding where
  PrintEndingNewLine :: PrintEnding
  PrintEndingNoNewLine :: PrintEnding
  deriving (Show, Eq)

data PrintKind where
  PrintKindPrint :: PrintKind
  PrintKindPause :: PrintKind
  PrintKindUsing :: PrintKind
  deriving (Show, Eq)

data Assignment where
  Assignment :: Ident -> Expr -> Assignment
  deriving (Eq)

instance Show Assignment where
  show (Assignment x e) = show x ++ " = " ++ show e

data GotoTarget where
  GoToLabel :: String -> GotoTarget
  GoToLine :: Double -> GotoTarget
  deriving (Eq)

instance Show GotoTarget where
  show (GoToLabel s) = "\"" ++ s ++ "\""
  show (GoToLine n) = show n

data PokeKind where
  Me0 :: PokeKind
  Me1 :: PokeKind
  deriving (Eq)

data Stmt where
  LetStmt :: [Assignment] -> Stmt
  IfStmt :: Expr -> Stmt -> Stmt
  PrintStmt :: PrintKind -> [Expr] -> PrintEnding -> Stmt
  InputStmt :: Maybe Expr -> Expr -> Stmt
  EndStmt :: Stmt
  Comment :: Stmt
  ForStmt :: Assignment -> Expr -> Stmt
  NextStmt :: Ident -> Stmt
  ClearStmt :: Stmt
  GoToStmt :: GotoTarget -> Stmt
  GoSubStmt :: GotoTarget -> Stmt
  WaitStmt :: Maybe Expr -> Stmt
  ClsStmt :: Stmt
  RandomStmt :: Stmt
  GprintStmt :: [Expr] -> PrintEnding -> Stmt
  GCursorStmt :: Expr -> Stmt
  CursorStmt :: Expr -> Stmt
  BeepStmt :: [Expr] -> Stmt
  ReturnStmt :: Stmt
  PokeStmt :: PokeKind -> [Expr] -> Stmt
  DimStmt :: Expr -> Stmt
  ReadStmt :: [Expr] -> Stmt
  DataStmt :: [Expr] -> Stmt
  RestoreStmt :: Maybe Expr -> Stmt
  deriving (Eq)

instance Show Stmt where
  show (LetStmt assignments) = "LET " ++ intercalate ", " (show <$> assignments)
  show (IfStmt cond s) = "IF " ++ show cond ++ " THEN " ++ show s
  show (PrintStmt k exprs kind) =
    ( case k of
        PrintKindPrint -> "PRINT "
        PrintKindPause -> "PAUSE "
        PrintKindUsing -> "USING "
    )
      ++ intercalate "; " (show <$> exprs)
      ++ case kind of
        PrintEndingNewLine -> ""
        PrintEndingNoNewLine -> ";"
  show (InputStmt maybePrintExpr me) =
    "INPUT "
      ++ maybe "" (\e -> show e ++ "; ") maybePrintExpr
      ++ show me
  show EndStmt = "END"
  show Comment = "REM"
  show (ForStmt assign to) = "FOR " ++ show assign ++ " TO " ++ show to
  show (NextStmt i) = "NEXT " ++ show i
  show ClearStmt = "CLEAR"
  show (GoToStmt target) = "GOTO " ++ show target
  show (GoSubStmt target) = "GOSUB " ++ show target
  show (WaitStmt e) = "WAIT " ++ show e
  show ClsStmt = "CLS"
  show RandomStmt = "RANDOM"
  show (GprintStmt exprs kind) =
    "GPRINT "
      ++ intercalate "; " (show <$> exprs)
      ++ case kind of
        PrintEndingNewLine -> ""
        PrintEndingNoNewLine -> ";"
  show (GCursorStmt e) = "GCURSOR " ++ show e
  show (BeepStmt exprs) = "BEEP " ++ intercalate ", " (show <$> exprs)
  show (CursorStmt e) = "CURSOR " ++ show e
  show ReturnStmt = "RETURN"
  show (PokeStmt kind exprs) =
    "POKE"
      ++ case kind of
        Me0 -> " "
        Me1 -> "# "
      ++ intercalate ", " (show <$> exprs)
  show (DimStmt e) = "DIM " ++ show e
  show (ReadStmt ids) = "READ " ++ intercalate ", " (show <$> ids)
  show (DataStmt exprs) = "DATA " ++ intercalate ", " (show <$> exprs)
  show (RestoreStmt Nothing) = "RESTORE"
  show (RestoreStmt (Just n)) = "RESTORE " ++ show n

-- A line starts with a line number and can contain multiple statements separated by ":"
-- example: 10 LET A = 5
-- example: 20 PRINT A: A = 5
data Line where
  Line ::
    { lineNumber :: Double,
      lineLabel :: Maybe String,
      lineStmts :: [Stmt]
    } ->
    Line
  deriving (Show, Eq)

newtype Program where
  Program :: {programLines :: [Line]} -> Program

instance Show Program where
  show (Program ls) =
    unlines $
      \case
        Line n Nothing stmts -> show n ++ " " ++ intercalate " : " (show <$> stmts)
        Line n (Just label) stmts -> show n ++ " \"" ++ label ++ "\" " ++ intercalate ": " (show <$> stmts)
        <$> ls
