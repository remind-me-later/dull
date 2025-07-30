module Ast.Parser
  ( parseProgram,
  )
where

import Ast.Types
import Control.Applicative (Alternative (many, (<|>)), optional)
import Data.Functor (($>))
import Data.Map qualified
import Data.Maybe (isJust)
import Data.Word (Word16, Word8)
import Text.Parsec
  ( ParsecT,
    char,
    eof,
    lookAhead,
    many1,
    runParserT,
    satisfy,
    sepBy,
    sepBy1,
    skipMany,
    string,
    try,
  )

type Parser = ParsecT String () IO

-- space consumer
-- New lines have semantic meaning in BASIC, so we don't consume them
-- carriage returns should always be followed by a new line so it's safe to ignore them
sc :: Parser ()
sc = skipMany (satisfy (`elem` [' ', '\t', '\r']))

lex :: Parser a -> Parser a
lex p = p <* sc

decimalNumber :: Parser Double -- FIXME: should use the Number representation of BASIC
decimalNumber = Ast.Parser.lex $ do
  wholePart <- many (satisfy (`elem` ['0' .. '9']))
  case wholePart of
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

-- hex number begin with &
hexNumber :: Parser Word16
hexNumber = Ast.Parser.lex $ do
  _ <- char '&'
  hexDigits <- many1 (satisfy (`elem` ['0' .. '9'] ++ ['A' .. 'F']))
  let num = read ("0x" ++ hexDigits) :: Integer
  if num > fromIntegral (maxBound :: Word16)
    then fail "Hex number out of bounds for Word16"
    else return (fromIntegral num)

word16 :: Parser Word16
word16 = Ast.Parser.lex $ do
  digits <- many1 (satisfy (`elem` ['0' .. '9']))
  return (read digits)

word8 :: Parser Word8
word8 = Ast.Parser.lex $ do
  digits <- many1 (satisfy (`elem` ['0' .. '9']))
  let num = read digits :: Integer
  if num > 255
    then fail "Word8 out of bounds (0-255)"
    else return (fromIntegral num)

keyword :: String -> Parser String
keyword kw = Ast.Parser.lex $ string kw

symbol :: Char -> Parser Char
symbol sym = Ast.Parser.lex $ char sym

ident :: Parser Ident
ident = Ast.Parser.lex $ do
  -- Look ahead to see the next 3 characters
  lookahead <- try (lookAhead (many (satisfy (`elem` ['A' .. 'Z']))))

  firstChar <- satisfy (`elem` ['A' .. 'Z'])

  -- If we have 3 or more uppercase letters, only parse the first
  if length lookahead >= 3
    then do
      return
        Ident
          { identName1 = firstChar,
            identName2 = Nothing,
            identHasDollar = False
          }
    else do
      -- Normal parsing for 1-2 characters
      secondChar <- optional (satisfy (`elem` ['A' .. 'Z']))
      dollar <- optional (char '$')
      return
        Ident
          { identName1 = firstChar,
            identName2 = secondChar,
            identHasDollar = isJust dollar
          }

pseudoVariable :: Parser PseudoVariable
pseudoVariable =
  try (keyword "TIME" $> TimePseudoVar)
    <|> keyword "INKEY$" $> InkeyPseudoVar

lvalueFixedMemoryArea :: Parser RawLValue
lvalueFixedMemoryArea = do
  _ <- char '@'
  hasDollar <- optional (char '$')
  _ <- symbol '('
  name <- satisfy (`elem` ['A' .. 'Z'])
  _ <- symbol ')'
  return
    ( LValueFixedMemoryAreaVar
        { lValueFixedMemoryAreaVarName = name,
          lValueFixedMemoryAreaHasDollar = isJust hasDollar
        }
    )

lvalueArrayAccess :: Parser RawLValue
lvalueArrayAccess = do
  ident' <- ident
  index <- optional (parens expression)
  case index of
    Just idx -> return (LValueArrayAccess ident' idx)
    Nothing -> return (LValueIdent ident')

lvalue :: Parser RawLValue
lvalue =
  Ast.Parser.lex
    ( try (LValuePseudoVar <$> pseudoVariable)
        <|> try lvalueFixedMemoryArea
        <|> lvalueArrayAccess
    )

stringVariableOrLiteral :: Parser StringVariableOrLiteral
stringVariableOrLiteral =
  try (StringLiteral <$> stringLiteral)
    <|> (StringVariable <$> ident)

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
    <|> sgnFunCall
  where
    rndFunCall = do
      _ <- keyword "RND"
      RndFun <$> decimalNumber
    asciiFunCall = do
      _ <- keyword "ASC"
      AsciiFun <$> expressionFactor
    sgnFunCall = do
      _ <- keyword "SGN"
      SgnFun <$> expression
    intFunCall = do
      _ <- keyword "INT"
      IntFun <$> expression
    pointFunCall = do
      _ <- keyword "POINT"
      PointFun <$> expression
    midFunCall = do
      _ <- keyword "MID$"
      _ <- symbol '('
      strExpr <- stringVariableOrLiteral
      _ <- symbol ','
      startExpr <- expression
      _ <- symbol ','
      lengthExpr <- expression
      _ <- symbol ')'
      return (MidFun {midFunStringExpr = strExpr, midFunStartExpr = startExpr, midFunLengthExpr = lengthExpr})
    leftFunCall = do
      _ <- keyword "LEFT$"
      _ <- symbol '('
      strExpr <- stringVariableOrLiteral
      _ <- symbol ','
      lengthExpr <- expression
      _ <- symbol ')'
      return (LeftFun {leftFunStringExpr = strExpr, leftFunLengthExpr = lengthExpr})
    rightFunCall = do
      _ <- keyword "RIGHT$"
      _ <- symbol '('
      strExpr <- stringVariableOrLiteral
      _ <- symbol ','
      lengthExpr <- expression
      _ <- symbol ')'
      return (RightFun {rightFunStringExpr = strExpr, rightFunLengthExpr = lengthExpr})
    statusFunCall = do
      _ <- keyword "STATUS"
      StatusFun <$> word8
    valFunCall = do
      _ <- keyword "VAL"
      ValFun <$> expressionFactor
    strFunCall = do
      _ <- keyword "STR$"
      StrFun <$> expression

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
newline = many (symbol '\n') $> ()

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
expression = logicalExpr
  where
    exponentExpr = do
      left <- expressionFactor
      maybeOp <- optional (binOperator CaretOp)
      case maybeOp of
        Just op -> do
          right <- exponentExpr
          return Expr {exprInner = BinExpr left op right, exprType = ()}
        Nothing -> return left

    unaryOpExpr = do
      maybeOp <-
        optional
          ( try
              (unaryOperator UnaryPlusOp)
              <|> unaryOperator UnaryMinusOp
          )
      case maybeOp of
        Just op -> do
          right <- unaryOpExpr
          return Expr {exprInner = UnaryExpr op right, exprType = ()}
        Nothing -> exponentExpr

    mulDivExpr = do
      left <- unaryOpExpr
      maybeOp <-
        optional
          ( try (binOperator MultiplyOp)
              <|> binOperator DivideOp
          )
      case maybeOp of
        Just op -> do
          right <- mulDivExpr
          return Expr {exprInner = BinExpr left op right, exprType = ()}
        Nothing -> return left

    addSubExpr = do
      left <- mulDivExpr
      maybeOp <-
        optional
          ( try (binOperator AddOp)
              <|> binOperator SubtractOp
          )
      case maybeOp of
        Just op -> do
          right <- addSubExpr
          return Expr {exprInner = BinExpr left op right, exprType = ()}
        Nothing -> return left

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
              <|> binOperator GreaterThanOp -- >
          )
      case maybeOp of
        Just op -> do
          right <- comparisonExpr
          return Expr {exprInner = BinExpr left op right, exprType = ()}
        Nothing -> return left

    unaryLogicalExpr = do
      maybeOp <- optional (unaryOperator UnaryNotOp)
      case maybeOp of
        Just op -> do
          right <- unaryLogicalExpr
          return Expr {exprInner = UnaryExpr op right, exprType = ()}
        Nothing -> comparisonExpr

    logicalExpr = do
      left <- unaryLogicalExpr
      maybeOp <-
        optional
          ( try (binOperator AndOp)
              <|> binOperator OrOp
          )
      case maybeOp of
        Just op -> do
          right <- logicalExpr
          return Expr {exprInner = BinExpr left op right, exprType = ()}
        Nothing -> return left

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
  UsingClause <$> stringLiteral

printStmt :: Parser RawStmt
printStmt = do
  k <- try (keyword "PRINT" $> Left ()) <|> (keyword "PAUSE" $> Right ())
  maybeUsing <- optional (usingClause <* symbol ';')
  firstExpr <- expression
  commaExpr <- optional (symbol ',' *> expression)
  commaFormat <- case commaExpr of
    Just expr -> do
      return PrintCommaFormat {printCommaFormatExpr1 = firstExpr, printCommaFormatExpr2 = expr}
    Nothing -> do
      restExprs <- many (try (symbol ';' *> expression))
      semi <- optional (symbol ';')
      return
        PrintSemicolonFormat
          { printSemicolonFormatUsingClause = maybeUsing,
            printSemicolonFormatExprs = firstExpr : restExprs,
            printSemicolonFormatEnding =
              case semi of
                Just _ -> PrintEndingNoNewLine
                Nothing -> PrintEndingNewLine
          }

  return $ case k of
    Left () -> PrintStmt {printCommaFormat = commaFormat}
    Right () -> PauseStmt {pauseCommaFormat = commaFormat}

gPrintStmt :: Parser RawStmt
gPrintStmt = do
  _ <- keyword "GPRINT"
  firstExpr <- expression
  sep <- optional ((try (symbol ',') $> Left ()) <|> (symbol ';' $> Right ()))
  case sep of
    Nothing -> do
      return
        ( GprintStmt
            { gprintExprs = [firstExpr],
              gprintSeparator = GPrintSeparatorComma -- Doesn't matter, we only have one expression
            }
        )
    Just (Left _) -> do
      restExprs <- expression `sepBy` symbol ','
      return
        ( GprintStmt
            { gprintExprs = firstExpr : restExprs,
              gprintSeparator = GPrintSeparatorComma
            }
        )
    Just (Right _) -> do
      restExprs <- expression `sepBy` symbol ';'
      return
        ( GprintStmt
            { gprintExprs = firstExpr : restExprs,
              gprintSeparator = GPrintSeparatorSemicolon
            }
        )

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
  maybePrintExpr <- optional (stringLiteral <* symbol ';')
  dest <- lvalue
  return InputStmt {inputPrintExpr = maybePrintExpr, inputDestination = dest}

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
  _ <- keyword "REM"
  _ <- many (satisfy (/= '\n'))
  return Comment

beepOptionalParamsP :: Parser (BeepOptionalParams ())
beepOptionalParamsP = do
  _ <- symbol ','
  frequency <- expression
  _ <- symbol ','
  duration <- expression
  return BeepOptionalParams {beepFrequency = frequency, beepDuration = duration}

beepStmt :: Parser RawStmt
beepStmt = do
  _ <- keyword "BEEP"

  k <- optional (try (keyword "ON" $> Left ()) <|> (keyword "OFF" $> Right ()))
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

dimStmt :: Parser RawStmt
dimStmt = do
  _ <- keyword "DIM"
  identifier <- ident
  size <- parens word8
  strLen <- optional (binOperator MultiplyOp *> word8)
  return
    ( DimStmt
        ( DimInner
            { dimIdent = identifier,
              dimSize = size,
              dimStringLength = strLen
            }
        )
    )

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
  RestoreStmt <$> expression

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
  return CallStmt {callExpression = expr}

onGotoGosubStmt :: Parser RawStmt
onGotoGosubStmt = do
  _ <- keyword "ON"
  expr <- expression
  gotoOrGosub <- (keyword "GOTO" $> Left ()) <|> (keyword "GOSUB" $> Right ())
  targets <- commaSeparated word16
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
    <|> try callStmt
    <|> letStmt mandatoryLet

line :: Parser RawLine
line = do
  lineNumber <- word16
  lineLabel <- optional (stringLiteral <* optional (symbol ':'))
  lineStmts <- stmt False `sepBy` symbol ':'
  -- if there are no stmts the label wasn't a label it was a print statement
  case (lineLabel, lineStmts) of
    (Just label, []) -> do
      _ <- newline
      return
        Line
          { lineNumber,
            lineLabel = Nothing,
            lineStmts =
              [ PrintStmt
                  { printCommaFormat =
                      PrintSemicolonFormat
                        { printSemicolonFormatUsingClause = Nothing,
                          printSemicolonFormatExprs = [Expr {exprInner = StrLitExpr label, exprType = ()}],
                          printSemicolonFormatEnding = PrintEndingNewLine
                        }
                  }
              ]
          }
    _ -> do
      _ <- newline
      return Line {lineNumber, lineLabel, lineStmts}

program :: Parser RawProgram
program = do
  sc
  lines' <- many line
  eof
  let lineMap = foldr (\l acc -> Data.Map.insert (lineNumber l) l acc) Data.Map.empty lines'
  return (Program lineMap)

parseProgram :: String -> String -> IO (Either String RawProgram)
parseProgram fileName contents = do
  result <- runParserT program () fileName contents
  case result of
    Left err -> return (Left (show err))
    Right prog -> return (Right prog)