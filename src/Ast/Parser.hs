module Ast.Parser
  ( parseProgram,
  )
where

import Ast.Types
import Control.Applicative (Alternative (many, (<|>)), optional)
import Data.Functor (($>))
import Data.Map qualified
import Text.Parsec
  ( ParsecT,
    char,
    eof,
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

number :: Parser Double -- FIXME: should use the Number representation of BASIC
number = Ast.Parser.lex $ do
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

integer :: Parser Int
integer = Ast.Parser.lex $ do
  digits <- many1 (satisfy (`elem` ['0' .. '9']))
  return (read digits)

keyword :: String -> Parser String
keyword kw = Ast.Parser.lex $ string kw

symbol :: Char -> Parser Char
symbol sym = Ast.Parser.lex $ char sym

-- Variables consist of the following:
-- \[A-Z]$?
-- the dollar indicates a string variable

ident :: Parser Ident
ident = Ast.Parser.lex $ do
  firstChar <- satisfy (`elem` ['A' .. 'Z'])
  dollar <- optional (char '$')
  return
    Ident
      { identName = firstChar,
        identHasDollar = case dollar of
          Just _ -> True
          Nothing -> False
      }

pseudoVariable :: Parser PseudoVariable
pseudoVariable =
  try (keyword "TIME" $> TimePseudoVar)
    <|> keyword "INKEY$" $> InkeyPseudoVar

lvalue :: Parser RawLValue
lvalue =
  Ast.Parser.lex
    ( try (LValuePseudoVar <$> pseudoVariable)
        <|> ( do
                ident' <- ident
                index <- optional (parens expression)
                case index of
                  Just idx -> return (LValueArrayAccess ident' idx)
                  Nothing -> return (LValueIdent ident')
            )
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
    <|> sgnFunCall
  where
    rndFunCall = do
      _ <- keyword "RND"
      RndFun <$> integer
    asciiFunCall = do
      _ <- keyword "ASC"
      AsciiFun <$> stringVariableOrLiteral
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
commaSeparated p = sepBy1 p (symbol ',')

-- Parse new line, possibly with carriage return
newline :: Parser ()
newline = many (symbol '\n') $> ()

expression :: Parser RawExpr
expression = logicalExpr
  where
    factor =
      try parenExpr
        <|> try numLitExpr
        <|> try strLitExpr
        <|> try funCallExpr
        <|> lvalueExpr
      where
        numLitExpr = do
          n <- number
          return Expr {exprInner = NumLitExpr n, exprType = ()}
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

    exponentExpr = do
      left <- factor
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
    then do
      keyword "LET" $> ()
    else
      optional (keyword "LET") $> ()

  assignments <- commaSeparated assignment
  return (LetStmt assignments)

usingClause :: Parser UsingClause
usingClause = do
  _ <- keyword "USING"
  UsingClause <$> stringLiteral

printStmt :: Parser RawStmt
printStmt = do
  k <- try (keyword "PRINT" $> Left ()) <|> (keyword "PAUSE" $> Right ())
  maybeUsing <- optional (usingClause <* symbol ';')
  firstExpr <- expression
  restExprs <- many (try (symbol ';' *> expression))
  semi <- optional (symbol ';')
  return
    ( PrintStmt
        { printKind =
            case k of
              Left _ -> PrintKindPrint
              Right _ -> PrintKindPause,
          printExprs = firstExpr : restExprs,
          printEnding =
            case semi of
              Just _ -> PrintEndingNoNewLine
              Nothing -> PrintEndingNewLine,
          printUsingClause = maybeUsing
        }
    )

gPrintStmt :: Parser RawStmt
gPrintStmt = do
  _ <- keyword "GPRINT"
  firstExpr <- expression
  restExprs <- many (try (symbol ';' *> expression))
  semi <- optional (symbol ';')
  return
    ( GprintStmt
        { gprintExprs = firstExpr : restExprs,
          gprintEnding =
            case semi of
              Just _ -> PrintEndingNoNewLine
              Nothing -> PrintEndingNewLine
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
  identifier <- ident
  return InputStmt {inputPrintExpr = maybePrintExpr, inputDestination = identifier}

endStmt :: Parser RawStmt
endStmt = keyword "END" $> EndStmt

ifStmt :: Parser RawStmt
ifStmt = do
  _ <- keyword "IF"
  cond <- expression
  _ <- optional (keyword "THEN")
  -- the then statement can be a normal statement with a mandatory let or a line number
  thenStmt <- try (stmt True) <|> (GoToStmt . GoToLine <$> integer)
  return
    IfThenStmt
      { ifCondition = cond,
        ifThenStmt = thenStmt
      }

forStmt :: Parser RawStmt
forStmt = do
  _ <- keyword "FOR"
  a <- assignment
  _ <- keyword "TO"
  ForStmt a <$> expression

nextStmt :: Parser RawStmt
nextStmt = keyword "NEXT" *> (NextStmt <$> ident)

clearStmt :: Parser RawStmt
clearStmt = keyword "CLEAR" $> ClearStmt

clsStmt :: Parser RawStmt
clsStmt = keyword "CLS" $> ClsStmt

randomStmt :: Parser RawStmt
randomStmt = keyword "RANDOM" $> RandomStmt

gotoTargetStmt :: Parser GotoTarget
gotoTargetStmt =
  try (GoToLabel <$> stringLiteral)
    <|> GoToLine <$> integer

gotoStmt :: Parser RawStmt
gotoStmt = do
  _ <- keyword "GOTO"
  GoToStmt <$> gotoTargetStmt

gosubStmt :: Parser RawStmt
gosubStmt = do
  _ <- keyword "GOSUB"
  GoSubStmt <$> gotoTargetStmt

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
  size <- parens integer
  strLen <- optional (binOperator MultiplyOp *> integer)
  return
    ( DimStmt
        ( DimKind
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
    <|> letStmt mandatoryLet

line :: Parser RawLine
line = do
  lineNumber <- integer
  lineLabel <- optional stringLiteral
  lineStmts <- sepBy (stmt False) (symbol ':')
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
                  { printKind = PrintKindPrint,
                    printExprs = [Expr {exprInner = StrLitExpr label, exprType = ()}],
                    printEnding = PrintEndingNewLine,
                    printUsingClause = Nothing
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