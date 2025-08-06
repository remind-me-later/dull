module Ast.Parser
  ( parseProgram,
  )
where

import Ast.Types
import Control.Applicative (Alternative (many, (<|>)), optional)
import Data.Foldable (find)
import Data.Functor (($>))
import Data.List (isSuffixOf)
import Data.Map qualified
import Data.Maybe (catMaybes, isJust)
import Data.Word (Word16)
import Text.Parsec
  ( Parsec,
    char,
    eof,
    lookAhead,
    many1,
    runParser,
    satisfy,
    sepBy,
    sepBy1,
    skipMany,
    string,
    try,
  )

type Parser = Parsec String ()

-- space consumer
-- New lines have semantic meaning in BASIC, so we don't consume them
-- carriage returns should always be followed by a new line so it's safe to ignore them
sc :: Parser ()
sc = skipMany (satisfy (`elem` [' ', '\t', '\r']))

lex :: Parser a -> Parser a
lex p = p <* sc

exponentPart :: Parser Int
exponentPart = do
  _ <- char 'E'
  sign <- optional (char '+' <|> char '-')
  digits <- many1 (satisfy (`elem` ['0' .. '9']))
  let exponent' = read digits :: Int
  return $ case sign of
    Just '-' -> -exponent'
    _ -> exponent'

decimalNumber :: Parser DecimalNumber
decimalNumber = Ast.Parser.lex $ do
  wholePart <- many (satisfy (`elem` ['0' .. '9']))
  res <- case wholePart of
    [] -> do
      -- now we must have a decimal part
      _ <- char '.'
      decimalPart <- many1 (satisfy (`elem` ['0' .. '9']))
      return (read ("0." ++ decimalPart))
    _ -> do
      -- if we have a whole part, we can have an optional decimal part
      decimalPart <- optional (char '.' *> many1 (satisfy (`elem` ['0' .. '9'])))
      case decimalPart of
        Just d -> return (read (wholePart ++ "." ++ d))
        Nothing -> return (read wholePart)
  -- parse optional exponent part
  maybeExponent <- optional (try exponentPart)
  case maybeExponent of
    Just exponent' -> return $ newDecimalNumber (res * (10 ^^ exponent'))
    Nothing -> return $ newDecimalNumber res

-- hex number begin with &
hexNumber :: Parser BinaryNumber
hexNumber = Ast.Parser.lex $ do
  _ <- char '&'
  hexDigits <- many1 (satisfy (`elem` ['0' .. '9'] ++ ['A' .. 'F']))
  let num = read ("0x" ++ hexDigits) :: Integer
  if num > fromIntegral (maxBound :: Word16)
    then fail "Hex number out of bounds for Word16"
    else return $ newBinaryNumber (fromIntegral num)

word16 :: Parser Word16
word16 = Ast.Parser.lex $ do
  digits <- many1 (satisfy (`elem` ['0' .. '9']))
  return (read digits)

keyword :: String -> Parser String
keyword kw = Ast.Parser.lex $ string kw

symbol :: Char -> Parser Char
symbol sym = Ast.Parser.lex $ char sym

-- Add in inverse order of suffix length to ensure longest match first
ambiguousKeywords :: [String]
ambiguousKeywords =
  [ "AND",
    "OR",
    "NOT",
    "IF",
    "THEN",
    "ELSE",
    "FOR",
    "STEP",
    "NEXT",
    "LF",
    "IF",
    "LN",
    "PI",
    "GOTO",
    "GOSUB",
    "TO",
    "LET"
  ]

ident :: Parser Ident
ident = Ast.Parser.lex $ do
  firstChar <- satisfy (`elem` ['A' .. 'Z'])

  -- Look ahead to see the next characters
  -- This is necessary to determine if the identifier has an ambiguous suffix
  lookahead <- lookAhead (many (satisfy (`elem` ['A' .. 'Z'] ++ ['0' .. '9'])))

  let (hasAmbiguousSuffix, identLengthRest) =
        case find (`isSuffixOf` lookahead) ambiguousKeywords of
          Just kw -> (True, length lookahead - length kw)
          Nothing -> (False, 0)
  rest <- case hasAmbiguousSuffix of
    True -> case identLengthRest of
      0 -> return Nothing -- no rest, just the first character
      _ -> Just <$> satisfy (`elem` ['A' .. 'Z'] ++ ['0' .. '9'])
    False -> optional (satisfy (`elem` ['A' .. 'Z'] ++ ['0' .. '9']))

  dollar <- optional (char '$')
  return
    Ident
      { identName1 = firstChar,
        identName2 = rest,
        identHasDollar = isJust dollar
      }

pseudoVariable :: Parser PseudoVariable
pseudoVariable =
  try (keyword "TIME" $> TimePseudoVar)
    <|> try (keyword "INKEY$" $> InkeyPseudoVar)

lvalueFixedMemoryArea :: Parser RawLValue
lvalueFixedMemoryArea = do
  _ <- char '@'
  hasDollar <- optional (char '$')
  _ <- symbol '('
  expr <- expression
  _ <- symbol ')'
  return
    ( LValueFixedMemoryAreaVar
        { lValueFixedMemoryAreaIndex = expr,
          lValueFixedMemoryAreaHasDollar = isJust hasDollar
        }
    )

lvalueArrayAccess :: Parser RawLValue
lvalueArrayAccess = do
  ident' <- ident
  index <- optional (parens expression)
  case index of
    Just idx ->
      return
        LValueArrayAccess
          { lValueArrayIdent = ident',
            lValueArrayIndex = idx
          }
    Nothing -> return (LValueIdent ident')

lvalue2DArrayAccess :: Parser RawLValue
lvalue2DArrayAccess = do
  ident' <- ident
  _ <- symbol '('
  rowIndex <- expression
  _ <- symbol ','
  colIndex <- expression
  _ <- symbol ')'
  return
    LValue2DArrayAccess
      { lValue2DArrayIdent = ident',
        lValue2DArrayRowIndex = rowIndex,
        lValue2DArrayColIndex = colIndex
      }

lvalue :: Parser RawLValue
lvalue =
  Ast.Parser.lex
    ( try (LValuePseudoVar <$> pseudoVariable)
        <|> try lvalueFixedMemoryArea
        <|> try lvalue2DArrayAccess
        <|> try lvalueArrayAccess
    )

functionCall :: Parser RawFunction
functionCall =
  try midFunCall
    <|> try leftFunCall
    <|> try rightFunCall
    <|> try asciiFunCall
    <|> try pointFunCall
    <|> try rndFunCall
    <|> try intFunCall
    <|> try statusFunCall
    <|> try valFunCall
    <|> try strFunCall
    <|> try chrFunCall
    <|> try absFunCall
    <|> try lenFunCall
    <|> try peekFunCall
    <|> try sgnFunCall
    <|> try lnFunCall
    <|> try logFunCall
    <|> try dmsFunCall
    <|> try degFunCall
    <|> try tanFunCall
    <|> try cosFunCall
    <|> try sinFunCall
    <|> try sqrtFunCall
  where
    rndFunCall = do
      _ <- keyword "RND"
      RndFun <$> expressionFactor
    asciiFunCall = do
      _ <- keyword "ASC"
      AsciiFun <$> expressionFactor
    sgnFunCall = do
      _ <- keyword "SGN"
      SgnFun <$> expressionFactor
    intFunCall = do
      _ <- keyword "INT"
      IntFun <$> expressionFactor
    pointFunCall = do
      _ <- keyword "POINT"
      PointFun <$> expressionFactor
    midFunCall = do
      _ <- keyword "MID$"
      _ <- symbol '('
      strExpr <- expression
      _ <- symbol ','
      startExpr <- expression
      _ <- symbol ','
      lengthExpr <- expression
      _ <- symbol ')'
      return (MidFun {midFunStringExpr = strExpr, midFunStartExpr = startExpr, midFunLengthExpr = lengthExpr})
    leftFunCall = do
      _ <- keyword "LEFT$"
      _ <- symbol '('
      strExpr <- expression
      _ <- symbol ','
      lengthExpr <- expression
      _ <- symbol ')'
      return (LeftFun {leftFunStringExpr = strExpr, leftFunLengthExpr = lengthExpr})
    rightFunCall = do
      _ <- keyword "RIGHT$"
      _ <- symbol '('
      strExpr <- expression
      _ <- symbol ','
      lengthExpr <- expression
      _ <- symbol ')'
      return (RightFun {rightFunStringExpr = strExpr, rightFunLengthExpr = lengthExpr})
    statusFunCall = do
      _ <- keyword "STATUS"
      StatusFun <$> expression
    valFunCall = do
      _ <- keyword "VAL"
      ValFun <$> expressionFactor
    strFunCall = do
      _ <- keyword "STR$"
      StrFun <$> expressionFactor
    chrFunCall = do
      _ <- keyword "CHR$"
      ChrFun <$> expressionFactor
    absFunCall = do
      _ <- keyword "ABS"
      AbsFun <$> expressionFactor
    lenFunCall = do
      _ <- keyword "LEN"
      LenFun <$> expressionFactor
    peekFunCall = do
      _ <- keyword "PEEK"
      me1 <- optional (char '#' $> ())
      addr <- expressionFactor
      return
        PeekFun
          { peekMemoryArea = case me1 of
              Just _ -> Me1
              Nothing -> Me0,
            peekFunAddress = addr
          }
    lnFunCall = do
      _ <- keyword "LN"
      LnFun <$> expressionFactor
    logFunCall = do
      _ <- keyword "LOG"
      LogFun <$> expressionFactor
    dmsFunCall = do
      _ <- keyword "DMS"
      DmsFun <$> expressionFactor
    degFunCall = do
      _ <- keyword "DEG"
      DegFun <$> expressionFactor
    tanFunCall = do
      _ <- keyword "TAN"
      TanFun <$> expressionFactor
    cosFunCall = do
      _ <- keyword "COS"
      CosFun <$> expressionFactor
    sinFunCall = do
      _ <- keyword "SIN"
      SinFun <$> expressionFactor
    sqrtFunCall = do
      _ <- keyword "√" <|> keyword "SQR"
      SqrtFun <$> expressionFactor

stringLiteral :: Parser String
stringLiteral = Ast.Parser.lex $ do
  _ <- char '"'
  content <- many (satisfy (/= '"'))
  _ <- char '"'
  return content

unaryOperator :: UnaryOperator -> Parser UnaryOperator
unaryOperator op = try (keyword (show op) $> op)

binOperator :: BinOperator -> Parser BinOperator
binOperator op = try (keyword (show op) $> op)

parens :: Parser a -> Parser a
parens p = do
  _ <- symbol '('
  result <- p
  _ <- symbol ')'
  return result

commaSeparated :: Parser a -> Parser [a]
commaSeparated p = p `sepBy1` symbol ','

-- Parse new line, possibly with carriage return
newline :: Parser ()
newline = many1 (symbol '\n') $> ()

expressionFactor :: Parser RawExpr
expressionFactor =
  try parenExpr
    <|> try decNumLitExpr
    <|> try strLitExpr
    <|> try funCallExpr
    <|> try hexNumLitExpr
    <|> lvalueExpr
  where
    decNumLitExpr = do
      n <- decimalNumber
      return Expr {exprInner = DecNumLitExpr n, exprType = ()}
    hexNumLitExpr = do
      n <- hexNumber
      return Expr {exprInner = HexNumLitExpr n, exprType = ()}
    strLitExpr = do
      str <- stringLiteral
      return Expr {exprInner = StrLitExpr str, exprType = ()}
    lvalueExpr = do
      lvalue' <- lvalue
      return Expr {exprInner = LValueExpr lvalue', exprType = ()}
    parenExpr = parens expression
    funCallExpr = do
      fun <- functionCall
      return Expr {exprInner = FunCallExpr fun, exprType = ()}

expression :: Parser RawExpr
expression = orExpr
  where
    -- Right-associative: a ^ b ^ c = a ^ (b ^ c)
    exponentExpr = do
      left <- expressionFactor
      maybeOp <- optional (try (binOperator CaretOp))
      case maybeOp of
        Just op -> do
          right <- exponentExpr -- Right-associative: recurse on same level
          return Expr {exprInner = BinExpr left op right, exprType = ()}
        Nothing -> return left

    unaryOpExpr = do
      maybeOp <-
        optional
          ( try (unaryOperator UnaryPlusOp)
              <|> try (unaryOperator UnaryMinusOp)
          )
      case maybeOp of
        Just op -> do
          right <- unaryOpExpr
          return Expr {exprInner = UnaryExpr op right, exprType = ()}
        Nothing -> exponentExpr

    -- Right-associative
    mulDivExpr = do
      left <- unaryOpExpr
      rest <-
        many
          ( do
              op <- try (binOperator MultiplyOp) <|> try (binOperator DivideOp)
              right <- mulDivExpr
              return (op, right)
          )
      return $ foldr (\(op, right) acc -> Expr {exprInner = BinExpr acc op right, exprType = ()}) left rest

    -- Right-associative
    addSubExpr = do
      left <- mulDivExpr
      rest <-
        many
          ( do
              op <- try (binOperator AddOp) <|> try (binOperator SubtractOp)
              right <- addSubExpr
              return (op, right)
          )
      return $ foldl (\acc (op, right) -> Expr {exprInner = BinExpr acc op right, exprType = ()}) left rest

    -- Right-associative: A = B = C < D = E = F
    comparisonExpr = do
      left <- addSubExpr
      maybeOp <-
        optional
          ( -- Order matters, parse longest operators first
            try (binOperator EqualOp) -- =
              <|> try (binOperator NotEqualOp) -- <>
              <|> try (binOperator LessThanOrEqualOp) -- <=
              <|> try (binOperator LessThanOp) -- <
              <|> try (binOperator GreaterThanOrEqualOp) -- >=
              <|> try (binOperator GreaterThanOp) -- >
          )
      case maybeOp of
        Just op -> do
          right <- comparisonExpr
          return Expr {exprInner = BinExpr left op right, exprType = ()}
        Nothing -> return left

    unaryLogicalExpr = do
      maybeOp <- optional (try (unaryOperator UnaryNotOp))
      case maybeOp of
        Just op -> do
          right <- unaryLogicalExpr
          return Expr {exprInner = UnaryExpr op right, exprType = ()}
        Nothing -> comparisonExpr

    andExpr = do
      left <- unaryLogicalExpr
      rest <-
        many
          ( do
              op <- try (binOperator AndOp)
              right <- andExpr
              return (op, right)
          )
      return $ foldl (\acc (op, right) -> Expr {exprInner = BinExpr acc op right, exprType = ()}) left rest

    orExpr = do
      left <- andExpr
      rest <-
        many
          ( do
              op <- try (binOperator OrOp)
              right <- orExpr
              return (op, right)
          )
      return $ foldl (\acc (op, right) -> Expr {exprInner = BinExpr acc op right, exprType = ()}) left rest

assignment :: Parser RawAssignment
assignment = do
  v <- lvalue
  _ <- binOperator EqualOp
  exp' <- expression
  return
    Assignment
      { assignmentLValue = v,
        assignmentExpr = exp',
        assignmentType = ()
      }

letStmt :: Bool -> Parser RawStmt
letStmt mandatoryLet = do
  if mandatoryLet
    then
      keyword "LET" $> ()
    else
      optional (try (keyword "LET")) $> ()

  assignments <- commaSeparated assignment
  return LetStmt {letAssignments = assignments}

usingClause :: Parser UsingClause
usingClause = do
  _ <- keyword "USING"
  UsingClause <$> optional stringLiteral

printStmt :: Parser RawStmt
printStmt = do
  k <- try (keyword "PRINT" $> Left ()) <|> (keyword "PAUSE" $> Right ())
  exprs <-
    many
      ( try
          ( do
              expr <- expression
              sep <- optional (try (symbol ',') <|> symbol ';')
              case sep of
                Just ',' -> return (Left expr, PrintSeparatorComma)
                Just ';' -> return (Left expr, PrintSeparatorSemicolon)
                _ -> return (Left expr, PrintSeparatorEmpty)
          )
          <|> try
            ( do
                using <- usingClause
                _ <- symbol ';'
                return (Right using, PrintSeparatorSemicolon)
            )
      )
  case k of
    Left () -> return PrintStmt {printCommaFormat = PrintSemicolonFormat {printSemicolonFormatExprs = exprs}}
    Right () -> return PauseStmt {pauseCommaFormat = PrintSemicolonFormat {printSemicolonFormatExprs = exprs}}

lprintStmt :: Parser RawStmt
lprintStmt = do
  _ <- keyword "LPRINT"
  exprs <-
    many
      ( try
          ( do
              expr <- expression
              sep <- optional (try (symbol ',') <|> symbol ';')
              case sep of
                Just ',' -> return (Left expr, PrintSeparatorComma)
                Just ';' -> return (Left expr, PrintSeparatorSemicolon)
                _ -> return (Left expr, PrintSeparatorEmpty)
          )
          <|> try
            ( do
                _ <- keyword "TAB"
                expr <- expression
                _ <- symbol ';'
                return (Right (LCursorClause expr), PrintSeparatorSemicolon)
            )
      )
  return LPrintStmt {lprintCommaFormat = LPrintSemicolonFormat {lPrintSemicolonFormatExprs = exprs}}

gPrintStmt :: Parser RawStmt
gPrintStmt = do
  _ <- keyword "GPRINT"
  exprs <-
    many
      ( do
          expr <- expression
          sep <- optional (try (symbol ',') <|> symbol ';')
          case sep of
            Just ',' -> return (expr, PrintSeparatorComma)
            Just ';' -> return (expr, PrintSeparatorSemicolon)
            _ -> return (expr, PrintSeparatorEmpty)
      )

  return GprintStmt {gprintExprs = exprs}

gCursorStmt :: Parser RawStmt
gCursorStmt = do
  _ <- keyword "GCURSOR"
  GCursorStmt <$> expression

cursorStmt :: Parser RawStmt
cursorStmt = do
  _ <- keyword "CURSOR"
  CursorStmt <$> expression

inputStmt :: Parser RawStmt
inputStmt = do
  _ <- keyword "INPUT"
  exprs <-
    try
      ( do
          maybePrintExpr <- optional (stringLiteral <* symbol ';')
          dest <- lvalue
          return (maybePrintExpr, dest)
      )
      `sepBy1` symbol ','
  return InputStmt {inputExprs = exprs}

endStmt :: Parser RawStmt
endStmt = keyword "END" $> EndStmt

ifStmt :: Parser RawStmt
ifStmt = do
  _ <- keyword "IF"
  cond <- expression
  _ <- optional (keyword "THEN")
  -- the then statement can be a normal statement with a mandatory let or a line number
  thenStmt <- try (stmt True) <|> (GoToStmt <$> expression)
  return
    IfThenStmt
      { ifCondition = cond,
        ifThenStmt = thenStmt
      }

forStmt :: Parser RawStmt
forStmt = do
  _ <- keyword "FOR"
  ass <- assignment
  _ <- keyword "TO"
  toExpr <- expression
  stepExpr <- optional (keyword "STEP" *> expression)
  return
    ForStmt
      { forAssignment = ass,
        forToExpr = toExpr,
        forStepExpr = stepExpr
      }

nextStmt :: Parser RawStmt
nextStmt = keyword "NEXT" *> (NextStmt <$> ident)

clearStmt :: Parser RawStmt
clearStmt = keyword "CLEAR" $> ClearStmt

clsStmt :: Parser RawStmt
clsStmt = keyword "CLS" $> ClsStmt

randomStmt :: Parser RawStmt
randomStmt = keyword "RANDOM" $> RandomStmt

gotoStmt :: Parser RawStmt
gotoStmt = do
  _ <- keyword "GOTO"
  GoToStmt <$> expression

gosubStmt :: Parser RawStmt
gosubStmt = do
  _ <- keyword "GOSUB"
  GoSubStmt <$> expression

waitStmt :: Parser RawStmt
waitStmt = keyword "WAIT" *> (WaitStmt <$> optional expression)

usingStmt :: Parser RawStmt
usingStmt = do
  UsingStmt <$> usingClause

comment :: Parser RawStmt
comment = do
  -- Don't skip whitespace, so don't use the 'keyword' function
  _ <- string "REM "
  -- Parse until end of line, and remove line end
  content <- many (satisfy (not . (`elem` ['\n', '\r'])))
  _ <- sc -- Consume any trailing whitespace
  return Comment {commentText = content}

beepOptionalParamsP :: Parser (BeepOptionalParams ())
beepOptionalParamsP = do
  _ <- symbol ','
  frequency <- expression
  duration <- optional (symbol ',' *> expression)
  return BeepOptionalParams {beepFrequency = frequency, beepDuration = duration}

beepStmt :: Parser RawStmt
beepStmt = do
  _ <- keyword "BEEP"

  k <- optional (try (keyword "ON" $> Left ()) <|> try (keyword "OFF" $> Right ()))
  case k of
    Just (Left ()) -> return BeepOnOffStmt {beepOn = True}
    Just (Right ()) -> return BeepOnOffStmt {beepOn = False}
    Nothing -> do
      repetitions <- expression
      optionalParams <- optional beepOptionalParamsP
      return BeepStmt {beepStmtRepetitionsExpr = repetitions, beepStmtOptionalParams = optionalParams}

returnStmt :: Parser RawStmt
returnStmt = keyword "RETURN" $> ReturnStmt

pokeStmt :: Parser RawStmt
pokeStmt = do
  _ <- keyword "POKE"
  kind <- optional (symbol '#')
  exprs <- commaSeparated expression
  return
    ( PokeStmt
        ( case kind of
            Just _ -> Me1
            Nothing -> Me0
        )
        exprs
    )

dimDecl :: Parser (DimInner ())
dimDecl = do
  identifier <- ident
  _ <- symbol '('
  dimRows <- expression
  maybeCols <- optional (symbol ',' *> expression)
  _ <- symbol ')'
  maybeStrLen <- optional (binOperator MultiplyOp *> expression)

  case maybeCols of
    Just cols ->
      return
        DimInner2D
          { dimIdent = identifier,
            dimRows = dimRows,
            dimCols = cols,
            dimStringLength = maybeStrLen
          }
    Nothing ->
      return
        DimInner1D
          { dimIdent = identifier,
            dimSize = dimRows,
            dimStringLength = maybeStrLen
          }

dimStmt :: Parser RawStmt
dimStmt = do
  _ <- keyword "DIM"
  decls <- commaSeparated dimDecl
  return DimStmt {dimDecls = decls}

dataStmt :: Parser RawStmt
dataStmt = do
  _ <- keyword "DATA"
  DataStmt <$> commaSeparated expression

readStmt :: Parser RawStmt
readStmt = do
  _ <- keyword "READ"
  ReadStmt <$> commaSeparated lvalue

restoreStmt :: Parser RawStmt
restoreStmt = do
  _ <- keyword "RESTORE"
  RestoreStmt <$> optional expression

arunStmt :: Parser RawStmt
arunStmt = keyword "ARUN" $> ArunStmt

lockStmt :: Parser RawStmt
lockStmt = keyword "LOCK" $> LockStmt

unlockStmt :: Parser RawStmt
unlockStmt = keyword "UNLOCK" $> UnlockStmt

callStmt :: Parser RawStmt
callStmt = do
  _ <- keyword "CALL"
  expr <- expression
  maybeCallVar <- optional (symbol ',' *> lvalue)
  return CallStmt {callExpression = expr, maybeCallVariable = maybeCallVar}

onGotoGosubStmt :: Parser RawStmt
onGotoGosubStmt = do
  _ <- keyword "ON"
  expr <- expression
  gotoOrGosub <- (keyword "GOTO" $> Left ()) <|> (keyword "GOSUB" $> Right ())
  targets <- commaSeparated expression
  case gotoOrGosub of
    Left () ->
      return
        OnGoToStmt
          { onGotoExpr = expr,
            onGotoTargets = targets
          }
    Right () ->
      return
        OnGoSubStmt
          { onGosubExpr = expr,
            onGosubTargets = targets
          }

onErrorGotoStmt :: Parser RawStmt
onErrorGotoStmt = do
  _ <- keyword "ON"
  _ <- keyword "ERROR"
  _ <- keyword "GOTO"
  target <- expression
  return OnErrorGotoStmt {onErrorGotoTarget = target}

textStmt :: Parser RawStmt
textStmt = keyword "TEXT" $> TextStmt

graphStmt :: Parser RawStmt
graphStmt = keyword "GRAPH" $> GraphStmt

colorStmt :: Parser RawStmt
colorStmt = do
  _ <- keyword "COLOR"
  expr <- expression
  return ColorStmt {colorExpr = expr}

csizeStmt :: Parser RawStmt
csizeStmt = do
  _ <- keyword "CSIZE"
  expr <- expression
  return CSizeStmt {characterSizeExpr = expr}

lfStmt :: Parser RawStmt
lfStmt = do
  _ <- keyword "LF"
  expr <- expression
  return LfStmt {lineFeedExpr = expr}

radianStmt :: Parser RawStmt
radianStmt = keyword "RADIAN" $> RadianStmt

lcursorStmt :: Parser RawStmt
lcursorStmt = do
  _ <- keyword "LCURSOR"
  expr <- expression
  return LCursorStmt {lCursorClause = LCursorClause expr}

stmt :: Bool -> Parser RawStmt
stmt mandatoryLet =
  try endStmt
    <|> try printStmt
    <|> try ifStmt
    <|> try comment
    <|> try inputStmt
    <|> try forStmt
    <|> try nextStmt
    <|> try clearStmt
    <|> try gotoStmt
    <|> try gosubStmt
    <|> try waitStmt
    <|> try clsStmt
    <|> try randomStmt
    <|> try gPrintStmt
    <|> try gCursorStmt
    <|> try beepStmt
    <|> try cursorStmt
    <|> try returnStmt
    <|> try pokeStmt
    <|> try dimStmt
    <|> try readStmt
    <|> try dataStmt
    <|> try restoreStmt
    <|> try usingStmt
    <|> try arunStmt
    <|> try lockStmt
    <|> try unlockStmt
    <|> try onGotoGosubStmt
    <|> try onErrorGotoStmt
    <|> try callStmt
    <|> try lprintStmt
    <|> try textStmt
    <|> try graphStmt
    <|> try colorStmt
    <|> try csizeStmt
    <|> try lfStmt
    <|> try radianStmt
    <|> try lcursorStmt
    <|> try (letStmt mandatoryLet)

line :: Parser RawLine
line = do
  lineNumber <- word16
  lineLabel <- optional (stringLiteral <* many (symbol ':'))
  lineStmts <- optional (stmt False) `sepBy` many1 (symbol ':')
  _ <- newline <|> eof
  return Line {lineNumber, lineLabel, lineStmts = catMaybes lineStmts}

program :: Parser RawProgram
program = do
  sc
  lines' <- many line
  let lineMap = foldr (\l acc -> Data.Map.insert (lineNumber l) l acc) Data.Map.empty lines'
  return (Program lineMap)

parseProgram :: String -> String -> Either String RawProgram
parseProgram fileName contents =
  case runParser program () fileName contents of
    Left err -> Left (show err)
    Right prog -> Right prog