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
    ExprInner (..),
    RawProgram,
    RawLine,
    RawStmt,
    RawAssignment,
    RawExpr,
    RawLValue,
    RawFunction,
    RawExprInner,
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
  StringVariable :: StrIdent -> StringVariableOrLiteral
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

data StrIdent where
  StrIdent :: String -> StrIdent
  deriving (Eq, Ord)

instance Show StrIdent where
  show (StrIdent s) = s ++ "$"

data NumIdent where
  NumIdent :: String -> NumIdent
  deriving (Eq, Ord)

instance Show NumIdent where
  show (NumIdent s) = s

data Ident where
  IdentNumIdent :: NumIdent -> Ident
  IdentStrIdent :: StrIdent -> Ident
  deriving (Eq, Ord)

instance Show Ident where
  show (IdentNumIdent (NumIdent s)) = s
  show (IdentStrIdent (StrIdent s)) = s

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
  deriving (Show, Eq)

data PrintKind where
  PrintKindPrint :: PrintKind
  PrintKindPause :: PrintKind
  deriving (Show, Eq)

data UsingClause where
  UsingClause :: {usingClauseExpr :: StringVariableOrLiteral} -> UsingClause
  deriving (Eq)

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
data DimKind where
  DimNumeric ::
    { dimNumericVarName :: NumIdent,
      dimNumericSize :: Int
    } ->
    DimKind
  DimString ::
    { dimStringVarName :: StrIdent,
      dimStringSize :: Int,
      dimStringLength :: Maybe Int
    } ->
    DimKind
  deriving (Eq)

instance Show DimKind where
  show (DimNumeric var size) = "DIM " ++ show var ++ "(" ++ show size ++ ")"
  show (DimString var size len) =
    "DIM "
      ++ show var
      ++ "("
      ++ show size
      ++ ")"
      ++ case len of
        Just l -> "*" ++ show l
        Nothing -> ""

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
    { inputPrintExpr :: Maybe (Expr et),
      inputDestination :: Ident
    } ->
    Stmt et
  EndStmt :: Stmt et
  Comment :: Stmt et
  ForStmt :: {forAssignment :: Assignment et, forToExpr :: Expr et} -> Stmt et
  NextStmt :: {nextIdent :: NumIdent} -> Stmt et
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
  BeepStmt :: {beepExprs :: [Expr et]} -> Stmt et
  ReturnStmt :: Stmt et
  PokeStmt ::
    { pokeKind :: PokeKind,
      pokeExprs :: [Expr et]
    } ->
    Stmt et
  DimStmt :: {dimKind :: DimKind} -> Stmt et
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
