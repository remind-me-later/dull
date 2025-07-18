{-# LANGUAGE LambdaCase #-}

module Ast.Types
  ( BinOperator (..),
    StmtKeyword (..),
    Function (..),
    Ident (..),
    Expr (..),
    PrintEnding (..),
    PrintKind (..),
    Assignment (..),
    GotoTarget (..),
    PokeKind (..),
    Stmt (..),
    LineNumber,
    Line (..),
    Program (..),
    DimKind (..),
    LValue (..),
    PseudoVariable (..),
    UnaryOperator (..),
    StringVariableOrLiteral (..),
    UsingClause (..),
    StrIdent (..),
    NumIdent (..),
  )
where

import Data.List (intercalate)

data BinOperator where
  AddOp :: BinOperator
  SubtractOp :: BinOperator
  MultiplyOp :: BinOperator
  DivideOp :: BinOperator
  EqualOp :: BinOperator
  LessThanOp :: BinOperator
  GreaterThanOp :: BinOperator
  LessThanOrEqualOp :: BinOperator
  GreaterThanOrEqualOp :: BinOperator
  NotEqualOp :: BinOperator
  OrOp :: BinOperator
  AndOp :: BinOperator
  CaretOp :: BinOperator
  deriving (Eq)

instance Show BinOperator where
  show AddOp = "+"
  show SubtractOp = "-"
  show MultiplyOp = "*"
  show DivideOp = "/"
  show EqualOp = "="
  show LessThanOrEqualOp = "<="
  show GreaterThanOrEqualOp = ">="
  show NotEqualOp = "<>"
  show LessThanOp = "<"
  show GreaterThanOp = ">"
  show OrOp = "OR"
  show AndOp = "AND"
  show CaretOp = "^"

data StmtKeyword where
  -- Statements
  LetKeyword :: StmtKeyword
  IfKeyword :: StmtKeyword
  ThenKeyword :: StmtKeyword
  InputKeyword :: StmtKeyword
  PrintKeyword :: StmtKeyword
  EndKeyword :: StmtKeyword
  RemarkKeyword :: StmtKeyword
  ForKeyword :: StmtKeyword
  ToKeyword :: StmtKeyword
  NextKeyword :: StmtKeyword
  ClearKeyword :: StmtKeyword
  GotoKeyword :: StmtKeyword
  GosubKeyword :: StmtKeyword
  WaitKeyword :: StmtKeyword
  PauseKeyword :: StmtKeyword
  ClsKeyword :: StmtKeyword
  RandomKeyword :: StmtKeyword
  GprintKeyword :: StmtKeyword
  GCursorKeyword :: StmtKeyword
  CursorKeyword :: StmtKeyword
  BeepKeyword :: StmtKeyword
  UsingKeyword :: StmtKeyword
  ReturnKeyword :: StmtKeyword
  PokeKeyword :: StmtKeyword
  DimKeyword :: StmtKeyword
  ReadKeyword :: StmtKeyword
  DataKeyword :: StmtKeyword
  RestoreKeyword :: StmtKeyword
  deriving (Eq)

instance Show StmtKeyword where
  show LetKeyword = "LET"
  show IfKeyword = "IF"
  show ThenKeyword = "THEN"
  show InputKeyword = "INPUT"
  show PrintKeyword = "PRINT"
  show EndKeyword = "END"
  show RemarkKeyword = "REM"
  show ForKeyword = "FOR"
  show ToKeyword = "TO"
  show NextKeyword = "NEXT"
  show ClearKeyword = "CLEAR"
  show GotoKeyword = "GOTO"
  show GosubKeyword = "GOSUB"
  show WaitKeyword = "WAIT"
  show PauseKeyword = "PAUSE"
  show ClsKeyword = "CLS"
  show RandomKeyword = "RANDOM"
  show GprintKeyword = "GPRINT"
  show CursorKeyword = "CURSOR"
  show GCursorKeyword = "GCURSOR"
  show BeepKeyword = "BEEP"
  show UsingKeyword = "USING"
  show ReturnKeyword = "RETURN"
  show PokeKeyword = "POKE"
  show DimKeyword = "DIM"
  show ReadKeyword = "READ"
  show DataKeyword = "DATA"
  show RestoreKeyword = "RESTORE"

data StringVariableOrLiteral where
  StringLiteral :: String -> StringVariableOrLiteral
  StringVariable :: Ident -> StringVariableOrLiteral -- TODO: spearate string and int idents
  deriving (Eq)

instance Show StringVariableOrLiteral where
  show (StringLiteral str) = "\"" ++ str ++ "\""
  show (StringVariable ident) = show ident

data Function where
  -- String functions
  MidFun ::
    { midFunStringExpr :: StringVariableOrLiteral, -- the string to extract from
      midFunStartExpr :: Expr, -- the start position (1-based)
      midFunLengthExpr :: Expr -- the length of the substring
    } ->
    Function
  LeftFun ::
    { leftFunStringExpr :: StringVariableOrLiteral, -- the string to extract from
      leftFunLengthExpr :: Expr -- the length of the substring
    } ->
    Function
  RightFun ::
    { rightFunStringExpr :: StringVariableOrLiteral, -- the string to extract from
      rightFunLengthExpr :: Expr -- the length of the substring
    } ->
    Function
  AsciiFun ::
    { asciiFunArgument :: StringVariableOrLiteral
    } ->
    Function
  -- Integer functions
  PointFun ::
    { pointFunPositionExpr :: Expr -- returns the color of the pixel at the position, the expression must be in range 0-155
    } ->
    Function
  RndFun ::
    { rndRangeEnd :: Int -- TODO: what is the range of RND?
    } ->
    Function
  IntFun ::
    { intFunExpr :: Expr -- returns the integer part of the expression
    } ->
    Function
  SgnFun ::
    { sgnFunExpr :: Expr -- returns 1, 0 or -1
    } ->
    Function
  deriving (Eq)

instance Show Function where
  show MidFun {midFunStringExpr = str, midFunStartExpr = start, midFunLengthExpr = len} =
    "(MID " ++ show str ++ " " ++ show start ++ " " ++ show len ++ ")"
  show LeftFun {leftFunStringExpr = str, leftFunLengthExpr = len} =
    "(LEFT " ++ show str ++ " " ++ show len ++ ")"
  show RightFun {rightFunStringExpr = str, rightFunLengthExpr = len} =
    "(RIGHT " ++ show str ++ " " ++ show len ++ ")"
  show PointFun {pointFunPositionExpr = pos} =
    "(POINT " ++ show pos ++ ")"
  show
    RndFun
      { rndRangeEnd = end
      } =
      "(RND " ++ show end ++ ")"
  show IntFun {intFunExpr = expr} =
    "(INT " ++ show expr ++ ")"
  show SgnFun {sgnFunExpr = expr} =
    "(SGN " ++ show expr ++ ")"
  show
    AsciiFun
      { asciiFunArgument = arg
      } =
      "(ASC " ++ show arg ++ ")"

-- like varibles, but built-in
data PseudoVariable where
  TimePseudoVar :: PseudoVariable
  InkeyPseudoVar :: PseudoVariable -- Read only
  deriving (Eq)

instance Show PseudoVariable where
  show TimePseudoVar = "TIME"
  show InkeyPseudoVar = "INKEY$"

data StrIdent where
  StrIdent :: String -> StrIdent
  deriving (Eq)

instance Show StrIdent where
  show (StrIdent s) = s ++ "$"

data NumIdent where
  NumIdent :: String -> NumIdent
  deriving (Eq)

instance Show NumIdent where
  show (NumIdent s) = s

data Ident where
  NumVar :: NumIdent -> Ident
  StrVar :: StrIdent -> Ident
  deriving (Eq)

instance Show Ident where
  show (NumVar (NumIdent s)) = s
  show (StrVar (StrIdent s)) = s

data LValue where
  LValueIdent :: Ident -> LValue
  LValueArrayAccess ::
    { lValueArrayIdent :: Ident,
      lValueArrayIndex :: Expr
    } ->
    LValue
  LValuePseudoVar :: PseudoVariable -> LValue
  deriving (Eq)

instance Show LValue where
  show (LValueIdent ident) = show ident
  show (LValueArrayAccess ident index) =
    show ident ++ "(" ++ show index ++ ")"
  show (LValuePseudoVar pseudoVar) = show pseudoVar

data UnaryOperator where
  UnaryMinusOp :: UnaryOperator
  UnaryPlusOp :: UnaryOperator
  UnaryNotOp :: UnaryOperator
  deriving (Eq)

instance Show UnaryOperator where
  show UnaryMinusOp = "-"
  show UnaryPlusOp = "+"
  show UnaryNotOp = "NOT"

data Expr where
  UnaryExpr :: UnaryOperator -> Expr -> Expr
  BinExpr :: Expr -> BinOperator -> Expr -> Expr
  NumLitExpr :: Double -> Expr
  LValueExpr :: LValue -> Expr
  StrLitExpr :: String -> Expr
  FunCallExpr :: Function -> Expr
  deriving (Eq)

instance Show Expr where
  show (UnaryExpr op expr) = show op ++ show expr
  show (BinExpr left op right) = "(" ++ show left ++ " " ++ show op ++ " " ++ show right ++ ")"
  show (NumLitExpr n) = show n
  show (LValueExpr lval) = show lval
  show (StrLitExpr s) = "\"" ++ s ++ "\""
  show (FunCallExpr f) = show f

data PrintEnding where
  PrintEndingNewLine :: PrintEnding
  PrintEndingNoNewLine :: PrintEnding
  deriving (Show, Eq)

data PrintKind where
  PrintKindPrint :: PrintKind
  PrintKindPause :: PrintKind
  deriving (Show, Eq)

data UsingClause where
  UsingClause :: {usingClauseExpr :: StringVariableOrLiteral} -> UsingClause
  deriving (Eq)

data Assignment where
  Assignment :: LValue -> Expr -> Assignment
  deriving (Eq)

instance Show Assignment where
  show (Assignment x e) = show x ++ " = " ++ show e

data GotoTarget where
  GoToLabel :: String -> GotoTarget
  GoToLine :: LineNumber -> GotoTarget
  deriving (Eq)

instance Show GotoTarget where
  show (GoToLabel s) = "\"" ++ s ++ "\""
  show (GoToLine n) = show n

data PokeKind where
  Me0 :: PokeKind
  Me1 :: PokeKind
  deriving (Eq)

-- FIXME: allow 2 dimensions
data DimKind where
  DimNumeric ::
    { dimNumericVarName :: NumIdent,
      dimNumericSize :: Int
    } ->
    DimKind
  DimString ::
    { dimStringVarName :: StrIdent,
      dimStringSize :: Int,
      dimStringLength :: Int
    } ->
    DimKind
  deriving (Eq)

instance Show DimKind where
  show (DimNumeric var size) = "DIM " ++ show var ++ "(" ++ show size ++ ")"
  show (DimString var size len) =
    "DIM " ++ show var ++ "(" ++ show size ++ ")*" ++ show len

data Stmt where
  LetStmt :: {letAssignments :: [Assignment]} -> Stmt
  IfThenStmt :: {ifCondition :: Expr, ifThenStmt :: Stmt} -> Stmt
  -- FIXME: the PRINT statement can swparate the screen in two sections, with
  -- the first and second section being comma separated, skip for now
  PrintStmt ::
    { printKind :: PrintKind,
      printExprs :: [Expr],
      printEnding :: PrintEnding,
      printUsingClause :: Maybe UsingClause
    } ->
    Stmt
  UsingStmt ::
    { usingStmtClause :: UsingClause
    } ->
    Stmt
  InputStmt ::
    { inputPrintExpr :: Maybe Expr,
      inputDestination :: StrIdent
    } ->
    Stmt
  EndStmt :: Stmt
  Comment :: Stmt
  ForStmt :: {forAssignment :: Assignment, forToExpr :: Expr} -> Stmt
  NextStmt :: {nextIdent :: NumIdent} -> Stmt
  ClearStmt :: Stmt
  GoToStmt :: {gotoTarget :: GotoTarget} -> Stmt
  GoSubStmt :: {gosubTarget :: GotoTarget} -> Stmt
  WaitStmt ::
    { waitForExpr :: Maybe Expr -- TODO: should be in range 0-65535, how do we enforce this?
    } ->
    Stmt
  ClsStmt :: Stmt
  RandomStmt :: Stmt
  GprintStmt ::
    { gprintExprs :: [Expr],
      gprintEnding :: PrintEnding
    } ->
    Stmt
  GCursorStmt :: {gCursorExpr :: Expr} -> Stmt
  CursorStmt :: {cursorExpr :: Expr} -> Stmt
  BeepStmt :: {beepExprs :: [Expr]} -> Stmt
  ReturnStmt :: Stmt
  PokeStmt ::
    { pokeKind :: PokeKind,
      pokeExprs :: [Expr]
    } ->
    Stmt
  DimStmt :: {dimKind :: DimKind} -> Stmt
  ReadStmt ::
    { readStmtDestinations :: [LValue]
    } ->
    Stmt
  DataStmt :: [Expr] -> Stmt
  RestoreStmt ::
    { restoreLineOrLabelExpr :: Expr
    } ->
    Stmt
  deriving (Eq)

instance Show Stmt where
  show (LetStmt assignments) = "LET " ++ intercalate ", " (show <$> assignments)
  show (IfThenStmt cond s) = "IF " ++ show cond ++ " THEN " ++ show s
  show (PrintStmt k exprs kind maybeUsing) =
    ( case k of
        PrintKindPrint -> "PRINT "
        PrintKindPause -> "PAUSE "
    )
      ++ case maybeUsing of
        Just (UsingClause e) -> "USING " ++ show e ++ "; "
        Nothing -> ""
      ++ intercalate "; " (show <$> exprs)
      ++ case kind of
        PrintEndingNewLine -> ""
        PrintEndingNoNewLine -> ";"
  show (UsingStmt (UsingClause e)) = "USING " ++ show e
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
  show (RestoreStmt n) = "RESTORE " ++ show n

type LineNumber = Int

-- A line starts with a line number and can contain multiple statements separated by ":"
-- example: 10 LET A = 5
-- example: 20 PRINT A: A = 5
data Line where
  Line ::
    { lineNumber :: LineNumber,
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
