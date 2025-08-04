module Ast.Types
  ( BinOperator (..),
    Function (..),
    Ident (..),
    Expr (..),
    PrintEnding (..),
    Assignment (..),
    MemoryArea (..),
    Stmt (..),
    LineNumber,
    Line (..),
    Program (..),
    DimInner (..),
    LValue (..),
    PseudoVariable (..),
    UnaryOperator (..),
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
    PrintCommaFormat (..),
    GPrintSeparator (..),
    DecimalNumber (..),
    identName,
    newDecimalNumber,
    BinaryNumber (..),
    newBinaryNumber,
    precedence,
    isLeftAssociative,
  )
where

import Data.Char (toUpper)
import Data.List (intercalate)
import Data.Map qualified
import Data.Word (Word16)
import Numeric (showHex)

newtype DecimalNumber = DecimalNumber
  { numberRepr :: Double
  }
  deriving (Eq)

instance Show DecimalNumber where
  show (DecimalNumber n) =
    let absN = abs n
     in if absN >= 1e6 || (absN > 0 && absN < 1e-4)
          then formatScientific n
          else formatRegular n
    where
      formatScientific x =
        let sign = if x < 0 then "-" else ""
            absX = abs x
            exp' = floor (logBase 10 absX) :: Integer
            mantissa = absX / (10 ** fromIntegral exp')
            -- Format mantissa to remove trailing zeros
            mantissaStr = formatMantissa mantissa
         in sign ++ mantissaStr ++ "E" ++ show exp'

      formatMantissa m =
        let rounded = fromIntegral (round (m * 1e10) :: Integer) / 1e10 :: Double
            str = show rounded
         in if '.' `elem` str
              then
                reverse . dropWhile (== '0') . reverse $
                  if last str == '.' then init str else str
              else str

      formatRegular x =
        let (wholePart, fractionalPart) = properFraction x :: (Integer, Double)
            wholeStr = show wholePart
            fractionalStr = if fractionalPart == 0 then "" else show (abs fractionalPart)
         in if null fractionalStr
              then wholeStr
              else wholeStr ++ "." ++ dropWhile (== '0') (drop 1 fractionalStr)

-- Check mantissa fits in 10 digit decimal number
-- And exponent fits in Int8
newDecimalNumber :: Double -> DecimalNumber
newDecimalNumber n =
  if n == 0
    then DecimalNumber {numberRepr = n}
    else
      let absN = abs n
          exp' = floor (logBase 10 absN) :: Int
          mantissa = absN / (10 ** fromIntegral exp')
          -- Ensure mantissa is in [1.0, 10.0) range due to floating point precision issues
          (normalizedMantissa, normalizedExp) =
            if mantissa >= 10.0
              then (mantissa / 10.0, exp' + 1)
              else (mantissa, exp')
          -- Convert mantissa to integer representation to check digit count
          mantissaInt = round (normalizedMantissa * 1e9) :: Integer
          mantissaIntAbs = abs mantissaInt
       in if normalizedExp < -128 || normalizedExp > 127 || mantissaIntAbs >= 10000000000
            then
              error $
                "DecimalNumber: number out of range (mantissa too large or exponent out of Int8 range): "
                  ++ show n
                  ++ " (mantissa: "
                  ++ show mantissaInt
                  ++ ", exponent: "
                  ++ show normalizedExp
                  ++ ")"
            else DecimalNumber {numberRepr = n}

newtype BinaryNumber = BinaryNumber
  { binaryRepr :: Word16
  }
  deriving (Eq)

instance Show BinaryNumber where
  show (BinaryNumber n) =
    let showHexAllCaps x = map toUpper (showHex x "")
     in '&' : showHexAllCaps n

newBinaryNumber :: Word16 -> BinaryNumber
newBinaryNumber n = BinaryNumber {binaryRepr = n}

-- Precedence and associativity for binary operators
precedence :: BinOperator -> Int
precedence OrOp = 1
precedence AndOp = 2
precedence EqualOp = 3
precedence NotEqualOp = 3
precedence LessThanOp = 3
precedence LessThanOrEqualOp = 3
precedence GreaterThanOp = 3
precedence GreaterThanOrEqualOp = 3
precedence AddOp = 4
precedence SubtractOp = 4
precedence MultiplyOp = 5
precedence DivideOp = 5
precedence CaretOp = 6

isLeftAssociative :: BinOperator -> Bool
isLeftAssociative _ = False

-- Helper functions for precedence-aware printing
showExprWithContext :: Int -> Bool -> Expr et -> String
showExprWithContext parentPrec isRightSide (Expr {exprInner = inner}) =
  showExprInnerWithContext parentPrec isRightSide inner

showExprInnerWithContext :: Int -> Bool -> ExprInner et -> String
showExprInnerWithContext _ _ (UnaryExpr op expr) =
  show op ++ showExprWithContext 7 False expr -- Unary has highest precedence
showExprInnerWithContext parentPrec isRightSide (BinExpr left op right) =
  let myPrec = precedence op
      leftAssoc = isLeftAssociative op
      needsParens =
        myPrec < parentPrec
          || (myPrec == parentPrec && isRightSide && leftAssoc)
          || (myPrec == parentPrec && not isRightSide && not leftAssoc)
      leftStr = showExprWithContext myPrec False left
      rightStr = showExprWithContext myPrec True right
      -- result = leftStr ++ show op ++ rightStr
      result = case op of
        OrOp -> leftStr ++ " OR " ++ rightStr
        AndOp -> leftStr ++ " AND " ++ rightStr
        _ -> leftStr ++ show op ++ rightStr -- For other operators, use the default spacing
   in if needsParens then "(" ++ result ++ ")" else result
showExprInnerWithContext _ _ (DecNumLitExpr n) = show n
showExprInnerWithContext _ _ (HexNumLitExpr h) = show h
showExprInnerWithContext _ _ (LValueExpr lval) = show lval
showExprInnerWithContext _ _ (StrLitExpr s) = "\"" ++ s ++ "\""
showExprInnerWithContext _ _ (FunCallExpr f) = show f

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

data Function et where
  MidFun ::
    { midFunStringExpr :: Expr et, -- the string to extract from
      midFunStartExpr :: Expr et, -- the start position (1-based)
      midFunLengthExpr :: Expr et -- the length of the substring
    } ->
    Function et
  LeftFun ::
    { leftFunStringExpr :: Expr et, -- the string to extract from
      leftFunLengthExpr :: Expr et -- the length of the substring
    } ->
    Function et
  RightFun ::
    { rightFunStringExpr :: Expr et, -- the string to extract from
      rightFunLengthExpr :: Expr et -- the length of the substring
    } ->
    Function et
  AsciiFun ::
    { asciiFunArgument :: Expr et
    } ->
    Function et
  PointFun ::
    { pointFunPositionExpr :: Expr et -- returns the color of the pixel at the position, the expression must be in range 0-155
    } ->
    Function et
  RndFun ::
    { rndRangeEnd :: Expr et
    } ->
    Function et
  IntFun ::
    { intFunExpr :: Expr et
    } ->
    Function et
  SgnFun ::
    { sgnFunExpr :: Expr et -- returns 1, 0 or -1
    } ->
    Function et
  StatusFun ::
    { statusFunArg :: Expr et
    } ->
    Function et
  ValFun ::
    { valFunExpr :: Expr et -- the string to convert
    } ->
    Function et
  StrFun ::
    { strFunExpr :: Expr et -- the expression to convert to string
    } ->
    Function et
  ChrFun ::
    { chrFunExpr :: Expr et -- the numeric expression to convert to a character
    } ->
    Function et
  AbsFun ::
    { absFunExpr :: Expr et -- the numeric expression to get the absolute value of
    } ->
    Function et
  LenFun ::
    { lenFunExpr :: Expr et -- the string expression to get the length of
    } ->
    Function et
  PeekFun ::
    { peekMemoryArea :: MemoryArea, -- the memory area to peek from, Me0 or Me1
      peekFunAddress :: Expr et -- the address to peek from, must be a numeric expression
    } ->
    Function et
  LnFun ::
    { lnFunExpr :: Expr et -- the numeric expression to get the natural logarithm of
    } ->
    Function et
  LogFun ::
    { logFunExpr :: Expr et -- the numeric expression to get the base-10 logarithm of
    } ->
    Function et
  DmsFun ::
    { dmsFunExpr :: Expr et
    } ->
    Function et
  DegFun ::
    { degFunExpr :: Expr et
    } ->
    Function et
  deriving (Eq)

instance Show (Function et) where
  show MidFun {midFunStringExpr = str, midFunStartExpr = start, midFunLengthExpr = len} =
    "MID(" ++ showExprWithContext 0 False str ++ "," ++ showExprWithContext 0 False start ++ "," ++ showExprWithContext 0 False len ++ ")"
  show LeftFun {leftFunStringExpr = str, leftFunLengthExpr = len} =
    "LEFT(" ++ showExprWithContext 0 False str ++ "," ++ showExprWithContext 0 False len ++ ")"
  show RightFun {rightFunStringExpr = str, rightFunLengthExpr = len} =
    "RIGHT(" ++ showExprWithContext 0 False str ++ "," ++ showExprWithContext 0 False len ++ ")"
  show PointFun {pointFunPositionExpr = pos} =
    "POINT " ++ showExprWithContext 8 False pos -- Higher than any binary operator
  show
    RndFun
      { rndRangeEnd = end
      } =
      "RND " ++ showExprWithContext 8 False end
  show IntFun {intFunExpr = expr} =
    "INT " ++ showExprWithContext 8 False expr
  show SgnFun {sgnFunExpr = expr} =
    "SGN " ++ showExprWithContext 8 False expr
  show
    AsciiFun
      { asciiFunArgument = arg
      } =
      "ASC " ++ showExprWithContext 8 False arg
  show StatusFun {statusFunArg = arg} =
    "STATUS " ++ show arg
  show ValFun {valFunExpr = expr} =
    "VAL " ++ showExprWithContext 8 False expr
  show StrFun {strFunExpr = expr} =
    "STR$ " ++ showExprWithContext 8 False expr
  show ChrFun {chrFunExpr = expr} =
    "CHR$ " ++ showExprWithContext 8 False expr
  show AbsFun {absFunExpr = expr} =
    "ABS " ++ showExprWithContext 8 False expr
  show LenFun {lenFunExpr = expr} =
    "LEN " ++ showExprWithContext 8 False expr
  show PeekFun {peekMemoryArea = area, peekFunAddress = addr} =
    "PEEK"
      ++ case area of
        Me0 -> " "
        Me1 -> "# "
      ++ showExprWithContext 8 False addr
  show LnFun {lnFunExpr = expr} =
    "LN " ++ showExprWithContext 8 False expr
  show LogFun {logFunExpr = expr} =
    "LOG " ++ showExprWithContext 8 False expr
  show DmsFun {dmsFunExpr = expr} =
    "DMS " ++ showExprWithContext 8 False expr
  show DegFun {degFunExpr = expr} =
    "DEG " ++ showExprWithContext 8 False expr

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
    { identName1 :: Char,
      identName2 :: Maybe Char,
      identHasDollar :: Bool -- True if the identifier ends with a dollar sign, indicating a string variable
    } ->
    Ident
  deriving (Eq, Ord)

instance Show Ident where
  show (Ident c1 c2 hasDollar) =
    c1 : maybe "" (: []) c2 ++ if hasDollar then "$" else ""

identName :: Ident -> String
identName (Ident c1 c2 _) = c1 : maybe "" (: []) c2

data LValue et where
  LValueIdent :: Ident -> LValue et
  LValueArrayAccess ::
    { lValueArrayIdent :: Ident,
      lValueArrayIndex :: Expr et
    } ->
    LValue et
  LValue2DArrayAccess ::
    { lValue2DArrayIdent :: Ident,
      lValue2DArrayRowIndex :: Expr et,
      lValue2DArrayColIndex :: Expr et
    } ->
    LValue et
  LValuePseudoVar :: PseudoVariable -> LValue et
  LValueFixedMemoryAreaVar ::
    { lValueFixedMemoryAreaIndex :: Expr et,
      lValueFixedMemoryAreaHasDollar :: Bool -- True if the variable is a string variable
    } ->
    LValue et
  deriving (Eq)

instance Show (LValue et) where
  show (LValueIdent ident) = show ident
  show (LValueArrayAccess ident index) =
    show ident ++ "(" ++ show index ++ ")"
  show (LValue2DArrayAccess ident rowIndex colIndex) =
    show ident ++ "(" ++ show rowIndex ++ "," ++ show colIndex ++ ")"
  show (LValuePseudoVar pseudoVar) = show pseudoVar
  show (LValueFixedMemoryAreaVar idx hasDollar) =
    if hasDollar
      then "@$(" ++ show idx ++ ")"
      else "@(" ++ show idx ++ ")"

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
  DecNumLitExpr :: DecimalNumber -> ExprInner et
  HexNumLitExpr :: BinaryNumber -> ExprInner et
  LValueExpr :: LValue et -> ExprInner et
  StrLitExpr :: String -> ExprInner et
  FunCallExpr :: Function et -> ExprInner et
  deriving (Eq)

instance Show (ExprInner et) where
  show (UnaryExpr op expr) = show op ++ showExprWithContext 7 False expr
  show (BinExpr left op right) = showExprInnerWithContext 0 False (BinExpr left op right)
  show (DecNumLitExpr n) = show n
  show (HexNumLitExpr h) = show h
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
  show PrintEndingNoNewLine = ";"

-- FIXME: maybe this should allow also variables
data UsingClause where
  UsingClause :: {usingClauseExpr :: Maybe String} -> UsingClause
  deriving (Eq)

instance Show UsingClause where
  show (UsingClause expr) = "USING " ++ maybe "" (\e -> "\"" ++ e ++ "\"") expr

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
    show lValue ++ "=" ++ show expr

data MemoryArea where
  Me0 :: MemoryArea
  Me1 :: MemoryArea
  deriving (Eq)

data DimInner et where
  DimInner1D ::
    { dimIdent :: Ident,
      dimSize :: Expr et,
      dimStringLength :: Maybe (Expr et) -- if Nothing, the default string length is used, only used for string arrays
    } ->
    DimInner et
  DimInner2D ::
    { dimIdent :: Ident,
      dimRows :: Expr et,
      dimCols :: Expr et,
      dimStringLength :: Maybe (Expr et) -- if Nothing, the default string length is used, only used for string arrays
    } ->
    DimInner et
  deriving (Eq)

instance Show (DimInner et) where
  show (DimInner1D {dimIdent, dimSize, dimStringLength}) =
    show dimIdent
      ++ "("
      ++ show dimSize
      ++ ")"
      ++ case dimStringLength of
        Just len -> "*" ++ show len
        Nothing -> ""
  show (DimInner2D {dimIdent, dimRows, dimCols, dimStringLength}) =
    show dimIdent
      ++ "("
      ++ show dimRows
      ++ ","
      ++ show dimCols
      ++ ")"
      ++ case dimStringLength of
        Just len -> "*" ++ show len
        Nothing -> ""

data BeepOptionalParams et where
  BeepOptionalParams ::
    { beepFrequency :: Expr et,
      beepDuration :: Maybe (Expr et)
    } ->
    BeepOptionalParams et
  deriving (Eq)

instance Show (BeepOptionalParams et) where
  show (BeepOptionalParams {beepFrequency, beepDuration}) =
    "," ++ show beepFrequency ++ case beepDuration of
      Just duration -> "," ++ show duration
      Nothing -> ""

data PrintCommaFormat et where
  PrintCommaFormat ::
    { printCommaFormatExpr1 :: Expr et,
      printCommaFormatExpr2 :: Expr et
    } ->
    PrintCommaFormat et
  PrintSemicolonFormat ::
    { printSemicolonFormatUsingClause :: Maybe UsingClause,
      printSemicolonFormatExprs :: [Expr et],
      printSemicolonFormatEnding :: PrintEnding
    } ->
    PrintCommaFormat et
  deriving (Eq)

instance Show (PrintCommaFormat et) where
  show (PrintCommaFormat e1 e2) =
    show e1 ++ "," ++ show e2
  show (PrintSemicolonFormat maybeUsingClause exprs ending) =
    case maybeUsingClause of
      Just usingClause ->
        show usingClause ++ ";" ++ intercalate ";" (show <$> exprs) ++ show ending
      Nothing ->
        intercalate ";" (show <$> exprs) ++ show ending

data GPrintSeparator where
  GPrintSeparatorComma :: GPrintSeparator
  GPrintSeparatorSemicolon :: GPrintSeparator
  GPrintSeparatorEmpty :: GPrintSeparator -- used for the last expression in a GPRINT statement
  deriving (Eq)

instance Show GPrintSeparator where
  show GPrintSeparatorComma = ","
  show GPrintSeparatorSemicolon = ";"
  show GPrintSeparatorEmpty = ""

data Stmt et where
  LetStmt :: {letAssignments :: [Assignment et]} -> Stmt et
  IfThenStmt :: {ifCondition :: Expr et, ifThenStmt :: Stmt et} -> Stmt et
  PrintStmt ::
    { printCommaFormat :: Maybe (PrintCommaFormat et)
    } ->
    Stmt et
  PauseStmt ::
    { pauseCommaFormat :: Maybe (PrintCommaFormat et)
    } ->
    Stmt et
  LPrintStmt ::
    { lprintCommaFormat :: Maybe (PrintCommaFormat et)
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
  ForStmt ::
    { forAssignment :: Assignment et,
      forToExpr :: Expr et,
      forStepExpr :: Maybe (Expr et) -- if Nothing, the step is 1
    } ->
    Stmt et
  NextStmt :: {nextIdent :: Ident} -> Stmt et
  ClearStmt :: Stmt et
  GoToStmt :: {gotoTarget :: Expr et} -> Stmt et
  GoSubStmt :: {gosubTarget :: Expr et} -> Stmt et
  OnGoToStmt ::
    { onGotoExpr :: Expr et,
      onGotoTargets :: [Expr et]
    } ->
    Stmt et
  OnGoSubStmt ::
    { onGosubExpr :: Expr et,
      onGosubTargets :: [Expr et]
    } ->
    Stmt et
  OnErrorGotoStmt ::
    { onErrorGotoTarget :: Expr et
    } ->
    Stmt et
  WaitStmt ::
    { waitForExpr :: Maybe (Expr et)
    } ->
    Stmt et
  ClsStmt :: Stmt et
  RandomStmt :: Stmt et
  GprintStmt ::
    { gprintExprs :: [(Expr et, GPrintSeparator)]
    } ->
    Stmt et
  GCursorStmt :: {gCursorExpr :: Expr et} -> Stmt et
  CursorStmt :: {cursorExpr :: Expr et} -> Stmt et
  BeepStmt ::
    { beepStmtRepetitionsExpr :: Expr et,
      beepStmtOptionalParams :: Maybe (BeepOptionalParams et)
    } ->
    Stmt et
  BeepOnOffStmt ::
    { beepOn :: Bool -- True for BEEP ON, False for BEEP OFF
    } ->
    Stmt et
  ReturnStmt :: Stmt et
  PokeStmt ::
    { pokeKind :: MemoryArea,
      pokeExprs :: [Expr et]
    } ->
    Stmt et
  DimStmt :: {dimDecls :: [DimInner et]} -> Stmt et
  ReadStmt ::
    { readStmtDestinations :: [LValue et]
    } ->
    Stmt et
  DataStmt :: [Expr et] -> Stmt et
  RestoreStmt ::
    { restoreLineOrLabelExpr :: Maybe (Expr et)
    } ->
    Stmt et
  ArunStmt :: Stmt et
  LockStmt :: Stmt et
  UnlockStmt :: Stmt et
  CallStmt ::
    { callExpression :: Expr et,
      maybeCallVariable :: Maybe (LValue et) -- Optional variable to store the result of the call
    } ->
    Stmt et
  -- Printer
  TextStmt :: Stmt et
  GraphStmt :: Stmt et
  ColorStmt ::
    { colorExpr :: Expr et
    } ->
    Stmt et
  CSizeStmt ::
    { characterSizeExpr :: Expr et
    } ->
    Stmt et
  LfStmt ::
    { lineFeedExpr :: Expr et
    } ->
    Stmt et
  deriving (Eq)

showLetStmt :: Bool -> [Assignment et] -> String
showLetStmt isMandatoryLet assignments =
  if isMandatoryLet
    then "LET " ++ intercalate "," (show <$> assignments)
    else intercalate "," (show <$> assignments)

instance Show (Stmt et) where
  show (LetStmt assignments) = showLetStmt False assignments
  show (IfThenStmt cond s) =
    "IF "
      ++ show cond
      ++ case s of
        LetStmt letStmt -> " " ++ showLetStmt True letStmt
        GoToStmt l -> " " ++ show l
        _ -> " " ++ show s
  show (PrintStmt {printCommaFormat}) = "PRINT " ++ maybe "" show printCommaFormat
  show (PauseStmt {pauseCommaFormat}) = "PAUSE " ++ maybe "" show pauseCommaFormat
  show (UsingStmt usingClause) = show usingClause
  show (InputStmt maybePrintExpr me) =
    "INPUT "
      ++ maybe "" (\e -> show e ++ ";") maybePrintExpr
      ++ show me
  show EndStmt = "END"
  show Comment = "REM"
  show (ForStmt assign to step) =
    "FOR " ++ show assign ++ " TO " ++ show to ++ case step of
      Just s -> " STEP " ++ show s
      Nothing -> ""
  show (NextStmt i) = "NEXT " ++ show i
  show ClearStmt = "CLEAR"
  show (GoToStmt target) = "GOTO " ++ show target
  show (GoSubStmt target) = "GOSUB " ++ show target
  show (WaitStmt maybeExpr) =
    "WAIT " ++ maybe "" show maybeExpr
  show ClsStmt = "CLS"
  show RandomStmt = "RANDOM"
  show (GprintStmt exprs) =
    "GPRINT "
      ++ concatMap (\(e, sep) -> show e ++ show sep ++ " ") (init exprs)
      ++ ( \(e, sep) ->
             show e ++ show sep
         )
        (last exprs)
  show (GCursorStmt e) = "GCURSOR " ++ show e
  show (BeepStmt repetitions optionalParams) = "BEEP " ++ show repetitions ++ maybe "" show optionalParams
  show (BeepOnOffStmt beepOn) = "BEEP " ++ if beepOn then "ON" else "OFF"
  show (CursorStmt e) = "CURSOR " ++ show e
  show ReturnStmt = "RETURN"
  show (PokeStmt kind exprs) =
    "POKE"
      ++ case kind of
        Me0 -> " "
        Me1 -> "# "
      ++ intercalate "," (show <$> exprs)
  show (DimStmt decls) = "DIM " ++ intercalate "," (show <$> decls)
  show (ReadStmt ids) = "READ " ++ intercalate "," (show <$> ids)
  show (DataStmt exprs) = "DATA " ++ intercalate "," (show <$> exprs)
  show (RestoreStmt n) = "RESTORE " ++ maybe "" show n
  show ArunStmt = "ARUN"
  show LockStmt = "LOCK"
  show UnlockStmt = "UNLOCK"
  show (OnGoToStmt expr targets) =
    "ON " ++ show expr ++ " GOTO " ++ intercalate "," (show <$> targets)
  show (OnGoSubStmt expr targets) =
    "ON " ++ show expr ++ " GOSUB " ++ intercalate "," (show <$> targets)
  show (CallStmt expr maybeCallVariable) = "CALL " ++ show expr ++ maybe "" (\v -> ", " ++ show v) maybeCallVariable
  show (LPrintStmt maybeCommaFormat) = "LPRINT " ++ maybe "" show maybeCommaFormat
  show (OnErrorGotoStmt target) = "ON ERROR GOTO " ++ show target
  show TextStmt = "TEXT"
  show GraphStmt = "GRAPH"
  show (ColorStmt expr) = "COLOR " ++ show expr
  show (CSizeStmt expr) = "CSIZE " ++ show expr
  show (LfStmt expr) = "LF " ++ show expr

type LineNumber = Word16

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
    show n ++ " " ++ intercalate ":" (show <$> stmts)
  show (Line n (Just label) stmts) =
    show n ++ " \"" ++ label ++ "\"" ++ intercalate ":" (show <$> stmts)

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
