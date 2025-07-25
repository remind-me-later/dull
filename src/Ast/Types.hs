module Ast.Types
  ( BinOperator (..),
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
    DimInner (..),
    LValue (..),
    PseudoVariable (..),
    UnaryOperator (..),
    StringVariableOrLiteral (..),
    UsingClause (..),
    ExprInner (..),
    RawProgram,
    RawLine,
    RawStmt,
    RawAssignment,
    RawExpr,
    RawLValue,
    RawFunction,
    RawExprInner,
    BeepOptionalParams (..),
    getIdentName,
  )
where

import Data.List (intercalate)
import Data.Map qualified

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

data StringVariableOrLiteral where
  StringLiteral :: String -> StringVariableOrLiteral
  StringVariable :: Ident -> StringVariableOrLiteral
  deriving (Eq)

instance Show StringVariableOrLiteral where
  show (StringLiteral str) = "\"" ++ str ++ "\""
  show (StringVariable ident) = show ident

data Function et where
  -- String functions
  MidFun ::
    { midFunStringExpr :: StringVariableOrLiteral, -- the string to extract from
      midFunStartExpr :: Expr et, -- the start position (1-based)
      midFunLengthExpr :: Expr et -- the length of the substring
    } ->
    Function et
  LeftFun ::
    { leftFunStringExpr :: StringVariableOrLiteral, -- the string to extract from
      leftFunLengthExpr :: Expr et -- the length of the substring
    } ->
    Function et
  RightFun ::
    { rightFunStringExpr :: StringVariableOrLiteral, -- the string to extract from
      rightFunLengthExpr :: Expr et -- the length of the substring
    } ->
    Function et
  AsciiFun ::
    { asciiFunArgument :: StringVariableOrLiteral
    } ->
    Function et
  -- Integer functions
  PointFun ::
    { pointFunPositionExpr :: Expr et -- returns the color of the pixel at the position, the expression must be in range 0-155
    } ->
    Function et
  RndFun ::
    { rndRangeEnd :: Int -- TODO: what is the range of RND?
    } ->
    Function et
  IntFun ::
    { intFunExpr :: Expr et -- returns the integer part of the expression
    } ->
    Function et
  SgnFun ::
    { sgnFunExpr :: Expr et -- returns 1, 0 or -1
    } ->
    Function et
  deriving (Eq)

instance Show (Function et) where
  show MidFun {midFunStringExpr = str, midFunStartExpr = start, midFunLengthExpr = len} =
    "MID(" ++ show str ++ ", " ++ show start ++ ", " ++ show len ++ ")"
  show LeftFun {leftFunStringExpr = str, leftFunLengthExpr = len} =
    "LEFT(" ++ show str ++ ", " ++ show len ++ ")"
  show RightFun {rightFunStringExpr = str, rightFunLengthExpr = len} =
    "RIGHT(" ++ show str ++ ", " ++ show len ++ ")"
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

data Ident where
  Ident ::
    { identName :: Char,
      identHasDollar :: Bool -- True if the identifier ends with a dollar sign, indicating a string variable
    } ->
    Ident
  deriving (Eq, Ord)

instance Show Ident where
  show (Ident c True) = c : "$"
  show (Ident c False) = [c]

getIdentName :: Ident -> Char
getIdentName (Ident c _) = c

data LValue et where
  LValueIdent :: Ident -> LValue et
  LValueArrayAccess ::
    { lValueArrayIdent :: Ident,
      lValueArrayIndex :: Expr et
    } ->
    LValue et
  LValuePseudoVar :: PseudoVariable -> LValue et
  deriving (Eq)

instance Show (LValue et) where
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

data ExprInner et where
  UnaryExpr :: UnaryOperator -> Expr et -> ExprInner et
  BinExpr :: Expr et -> BinOperator -> Expr et -> ExprInner et
  NumLitExpr :: Double -> ExprInner et
  LValueExpr :: LValue et -> ExprInner et
  StrLitExpr :: String -> ExprInner et
  FunCallExpr :: Function et -> ExprInner et
  deriving (Eq)

instance Show (ExprInner et) where
  show (UnaryExpr op expr) = show op ++ show expr
  show (BinExpr left op right) = "(" ++ show left ++ " " ++ show op ++ " " ++ show right ++ ")"
  show (NumLitExpr n) = show n
  show (LValueExpr lval) = show lval
  show (StrLitExpr s) = "\"" ++ s ++ "\""
  show (FunCallExpr f) = show f

data Expr et where
  Expr ::
    { exprInner :: ExprInner et,
      exprType :: et
    } ->
    Expr et
  deriving (Eq)

instance Show (Expr et) where
  show (Expr {exprInner = inner}) = show inner

data PrintEnding where
  PrintEndingNewLine :: PrintEnding
  PrintEndingNoNewLine :: PrintEnding
  deriving (Eq)

instance Show PrintEnding where
  show PrintEndingNewLine = ""
  show PrintEndingNoNewLine = "\\n"

data PrintKind where
  PrintKindPrint :: PrintKind
  PrintKindPause :: PrintKind
  deriving (Eq)

instance Show PrintKind where
  show PrintKindPrint = "PRINT"
  show PrintKindPause = "PAUSE"

-- FIXME: maybe this should allow also variables
data UsingClause where
  UsingClause :: {usingClauseExpr :: String} -> UsingClause
  deriving (Eq)

instance Show UsingClause where
  show (UsingClause expr) = "USING " ++ show expr

data Assignment et where
  Assignment ::
    { assignmentLValue :: LValue et,
      assignmentExpr :: Expr et,
      assignmentType :: et
    } ->
    Assignment et
  deriving (Eq)

instance Show (Assignment et) where
  show (Assignment lValue expr _) =
    show lValue ++ " = " ++ show expr

data GotoTarget where
  GoToLabel :: String -> GotoTarget
  GoToLine :: LineNumber -> GotoTarget
  deriving (Eq, Ord)

instance Show GotoTarget where
  show (GoToLabel s) = "\"" ++ s ++ "\""
  show (GoToLine n) = show n

data PokeKind where
  Me0 :: PokeKind
  Me1 :: PokeKind
  deriving (Eq)

-- FIXME: allow 2 dimensions
data DimInner where
  DimInner ::
    { dimIdent :: Ident,
      dimSize :: Int,
      dimStringLength :: Maybe Int -- if Nothing, the default string length is used, only used for string arrays
    } ->
    DimInner
  deriving (Eq)

instance Show DimInner where
  show (DimInner {dimIdent, dimSize, dimStringLength}) =
    show dimIdent
      ++ "("
      ++ show dimSize
      ++ ")"
      ++ case dimStringLength of
        Just len -> "*" ++ show len
        Nothing -> ""

data BeepOptionalParams et where
  BeepOptionalParams ::
    { beepFrequency :: Expr et,
      beepDuration :: Expr et
    } ->
    BeepOptionalParams et
  deriving (Eq)

data Stmt et where
  LetStmt :: {letAssignments :: [Assignment et]} -> Stmt et
  IfThenStmt :: {ifCondition :: Expr et, ifThenStmt :: Stmt et} -> Stmt et
  -- FIXME: the PRINT statement can swparate the screen in two sections, with
  -- the first and second section being comma separated, skip for now
  PrintStmt ::
    { printKind :: PrintKind,
      printExprs :: [Expr et],
      printEnding :: PrintEnding,
      printUsingClause :: Maybe UsingClause
    } ->
    Stmt et
  UsingStmt ::
    { usingStmtClause :: UsingClause
    } ->
    Stmt et
  InputStmt ::
    { inputPrintExpr :: Maybe String,
      inputDestination :: LValue et
    } ->
    Stmt et
  EndStmt :: Stmt et
  Comment :: Stmt et
  ForStmt :: {forAssignment :: Assignment et, forToExpr :: Expr et} -> Stmt et
  NextStmt :: {nextIdent :: Ident} -> Stmt et
  ClearStmt :: Stmt et
  GoToStmt :: {gotoTarget :: GotoTarget} -> Stmt et
  GoSubStmt :: {gosubTarget :: GotoTarget} -> Stmt et
  WaitStmt ::
    { waitForExpr :: Maybe (Expr et) -- TODO: should be in range 0-65535, how do we enforce this?
    } ->
    Stmt et
  ClsStmt :: Stmt et
  RandomStmt :: Stmt et
  GprintStmt ::
    { gprintExprs :: [Expr et],
      gprintEnding :: PrintEnding
    } ->
    Stmt et
  GCursorStmt :: {gCursorExpr :: Expr et} -> Stmt et
  CursorStmt :: {cursorExpr :: Expr et} -> Stmt et
  BeepStmt ::
    { beepStmtRepetitionsExpr :: Expr et,
      beepStmtOptionalParams :: Maybe (BeepOptionalParams et)
    } ->
    Stmt et
  ReturnStmt :: Stmt et
  PokeStmt ::
    { pokeKind :: PokeKind,
      pokeExprs :: [Expr et]
    } ->
    Stmt et
  DimStmt :: {dimKind :: DimInner} -> Stmt et
  ReadStmt ::
    { readStmtDestinations :: [LValue et]
    } ->
    Stmt et
  DataStmt :: [Expr et] -> Stmt et
  RestoreStmt ::
    { restoreLineOrLabelExpr :: Expr et
    } ->
    Stmt et
  deriving (Eq)

instance Show (Stmt et) where
  show (LetStmt assignments) = "LET " ++ intercalate ", " (show <$> assignments)
  show (IfThenStmt cond s) = "IF " ++ show cond ++ " THEN " ++ show s
  show (PrintStmt k exprs kind maybeUsing) =
    show k
      ++ " "
      ++ case maybeUsing of
        Just usingClause -> show usingClause ++ "; "
        Nothing -> ""
      ++ intercalate "; " (show <$> exprs)
      ++ case kind of
        PrintEndingNewLine -> ""
        PrintEndingNoNewLine -> ";"
  show (UsingStmt usingClause) = "USING " ++ show usingClause
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
  show (WaitStmt maybeExpr) =
    "WAIT " ++ maybe "" show maybeExpr
  show ClsStmt = "CLS"
  show RandomStmt = "RANDOM"
  show (GprintStmt exprs kind) =
    "GPRINT "
      ++ intercalate "; " (show <$> exprs)
      ++ case kind of
        PrintEndingNewLine -> ""
        PrintEndingNoNewLine -> ";"
  show (GCursorStmt e) = "GCURSOR " ++ show e
  show (BeepStmt repetitions optionalParams) =
    "BEEP "
      ++ show repetitions
      ++ case optionalParams of
        Just (BeepOptionalParams {beepFrequency, beepDuration}) ->
          show beepFrequency ++ ", " ++ show beepDuration
        Nothing -> ""
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
data Line et where
  Line ::
    { lineNumber :: LineNumber,
      lineLabel :: Maybe String,
      lineStmts :: [Stmt et]
    } ->
    Line et
  deriving (Eq)

instance Show (Line et) where
  show (Line n Nothing stmts) =
    show n ++ " " ++ intercalate " : " (show <$> stmts)
  show (Line n (Just label) stmts) =
    show n ++ " \"" ++ label ++ "\" " ++ intercalate ": " (show <$> stmts)

newtype Program et where
  Program :: {programLines :: Data.Map.Map LineNumber (Line et)} -> Program et
  deriving (Eq)

instance Show (Program et) where
  show (Program ls) = intercalate "\n" (show <$> orderedLinesByLineNumber)
    where
      orderedLinesByLineNumber =
        map snd
          . Data.Map.toList
          $ ls

type RawProgram = Program ()

type RawLine = Line ()

type RawStmt = Stmt ()

type RawAssignment = Assignment ()

type RawExpr = Expr ()

type RawExprInner = ExprInner ()

type RawLValue = LValue ()

type RawFunction = Function ()
