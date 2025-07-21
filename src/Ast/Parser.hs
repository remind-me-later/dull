module Ast.Parser
  ( parseProgram,
  )
where

import Ast.Types
import Control.Applicative (Alternative (many, (<|>)), optional)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Text.Parsec
  ( ParsecT,
    char,
    many1,
    runParserT,
    satisfy,
    sepBy,
    sepBy1,
    skipMany,
    string,
    try,
  )
import TypeSystem (ExprType (..))

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
-- \[A-Z][A-Z\d]$?
-- the dollar indicates a string variable
numIdent :: Parser NumIdent
numIdent = Ast.Parser.lex $ do
  firstChar <- satisfy (`elem` ['A' .. 'Z'])
  secondChar <- optional (satisfy (`elem` ['A' .. 'Z'] ++ ['0' .. '9']))
  let varName = firstChar : maybe "" (: []) secondChar
  return (NumIdent varName)

strIdent :: Parser StrIdent
strIdent = Ast.Parser.lex $ do
  firstChar <- satisfy (`elem` ['A' .. 'Z'])
  secondChar <- optional (satisfy (`elem` ['A' .. 'Z'] ++ ['0' .. '9']))
  _ <- char '$'
  let varName = firstChar : maybe "" (: []) secondChar
  return (StrIdent varName)

-- try strIdent first, then numIdent, since we have to match the longest identifier first
ident :: Parser Ident
ident =
  try (IdentStrIdent <$> strIdent)
    <|> try (IdentNumIdent <$> numIdent)

pseudoVariable :: Parser PseudoVariable
pseudoVariable =
  try (keyword "TIME" $> TimePseudoVar)
    <|> keyword "INKEY$" $> InkeyPseudoVar

lvalue :: Parser LValue
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
    <|> (StringVariable <$> strIdent)

functionCall :: Parser Function
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

stmtKeyword :: StmtKeyword -> Parser StmtKeyword
stmtKeyword kw = try (keyword (show kw) $> kw)

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

expression :: Parser Expr
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
          return Expr {exprInner = NumLitExpr n, exprType = ExprUnknownType}
        strLitExpr = do
          str <- stringLiteral
          return Expr {exprInner = StrLitExpr str, exprType = ExprUnknownType}
        lvalueExpr = do
          lvalue' <- lvalue
          return Expr {exprInner = LValueExpr lvalue', exprType = ExprUnknownType}
        parenExpr = parens expression
        funCallExpr = do
          fun <- functionCall
          return Expr {exprInner = FunCallExpr fun, exprType = ExprUnknownType}

    exponentExpr = do
      left <- factor
      maybeOp <- optional (binOperator CaretOp)
      case maybeOp of
        Just op -> do
          right <- exponentExpr
          return Expr {exprInner = BinExpr left op right, exprType = ExprUnknownType}
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
          return Expr {exprInner = UnaryExpr op right, exprType = ExprUnknownType}
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
          return Expr {exprInner = BinExpr left op right, exprType = ExprUnknownType}
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
          return Expr {exprInner = BinExpr left op right, exprType = ExprUnknownType}
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
          return Expr {exprInner = BinExpr left op right, exprType = ExprUnknownType}
        Nothing -> return left

    unaryLogicalExpr = do
      maybeOp <- optional (unaryOperator UnaryNotOp)
      case maybeOp of
        Just op -> do
          right <- unaryLogicalExpr
          return Expr {exprInner = UnaryExpr op right, exprType = ExprUnknownType}
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
          return Expr {exprInner = BinExpr left op right, exprType = ExprUnknownType}
        Nothing -> return left

assignment :: Parser Assignment
assignment = do
  v <- lvalue
  _ <- binOperator EqualOp
  Assignment v <$> expression

letStmt :: Bool -> Parser Stmt
letStmt mandatoryLet = do
  if mandatoryLet
    then do
      stmtKeyword LetKeyword $> ()
    else
      optional (stmtKeyword LetKeyword) $> ()

  assignments <- commaSeparated assignment
  return (LetStmt assignments)

usingClause :: Parser UsingClause
usingClause = do
  _ <- stmtKeyword UsingKeyword
  UsingClause <$> stringVariableOrLiteral

printStmt :: Parser Stmt
printStmt = do
  k <- stmtKeyword PrintKeyword <|> stmtKeyword PauseKeyword
  maybeUsing <- optional (usingClause <* symbol ';')
  firstExpr <- expression
  restExprs <- many (try (symbol ';' *> expression))
  semi <- optional (symbol ';')
  return
    ( PrintStmt
        { printKind =
            case k of
              PrintKeyword -> PrintKindPrint
              PauseKeyword -> PrintKindPause
              _ -> error "Unexpected print kind",
          printExprs = firstExpr : restExprs,
          printEnding =
            case semi of
              Just _ -> PrintEndingNoNewLine
              Nothing -> PrintEndingNewLine,
          printUsingClause = maybeUsing
        }
    )

gPrintStmt :: Parser Stmt
gPrintStmt = do
  _ <- stmtKeyword GprintKeyword
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

gCursorStmt :: Parser Stmt
gCursorStmt = do
  _ <- stmtKeyword GCursorKeyword
  GCursorStmt <$> expression

cursorStmt :: Parser Stmt
cursorStmt = do
  _ <- stmtKeyword CursorKeyword
  CursorStmt <$> expression

inputStmt :: Parser Stmt
inputStmt = do
  _ <- stmtKeyword InputKeyword
  maybePrintExpr <- optional (expression <* symbol ';')
  identifier <- ident
  return InputStmt {inputPrintExpr = maybePrintExpr, inputDestination = identifier}

endStmt :: Parser Stmt
endStmt = stmtKeyword EndKeyword $> EndStmt

ifStmt :: Parser Stmt
ifStmt = do
  _ <- stmtKeyword IfKeyword
  cond <- expression
  _ <- optional (stmtKeyword ThenKeyword)
  -- the then statement can be a normal statement with a mandatory let or a line number
  thenStmt <- try (stmt True) <|> (GoToStmt . GoToLine <$> integer)
  return
    IfThenStmt
      { ifCondition = cond,
        ifThenStmt = thenStmt
      }

forStmt :: Parser Stmt
forStmt = do
  _ <- stmtKeyword ForKeyword
  a <- assignment
  _ <- stmtKeyword ToKeyword
  ForStmt a <$> expression

nextStmt :: Parser Stmt
nextStmt = stmtKeyword NextKeyword *> (NextStmt <$> numIdent)

clearStmt :: Parser Stmt
clearStmt = stmtKeyword ClearKeyword $> ClearStmt

clsStmt :: Parser Stmt
clsStmt = stmtKeyword ClsKeyword $> ClsStmt

randomStmt :: Parser Stmt
randomStmt = stmtKeyword RandomKeyword $> RandomStmt

gotoTargetStmt :: Parser GotoTarget
gotoTargetStmt =
  try (GoToLabel <$> stringLiteral)
    <|> GoToLine <$> integer

gotoStmt :: Parser Stmt
gotoStmt = do
  _ <- stmtKeyword GotoKeyword
  GoToStmt <$> gotoTargetStmt

gosubStmt :: Parser Stmt
gosubStmt = stmtKeyword GosubKeyword *> (GoSubStmt <$> gotoTargetStmt)

waitStmt :: Parser Stmt
waitStmt = stmtKeyword WaitKeyword *> (WaitStmt <$> optional expression)

usingStmt :: Parser Stmt
usingStmt = do
  UsingStmt <$> usingClause

comment :: Parser Stmt
comment = do
  _ <- stmtKeyword RemarkKeyword
  _ <- many (satisfy (/= '\n'))
  return Comment

beepStmt :: Parser Stmt
beepStmt = do
  _ <- stmtKeyword BeepKeyword
  exprs <- commaSeparated expression
  return (BeepStmt exprs)

returnStmt :: Parser Stmt
returnStmt = stmtKeyword ReturnKeyword $> ReturnStmt

pokeStmt :: Parser Stmt
pokeStmt = do
  _ <- stmtKeyword PokeKeyword
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

dimStmt :: Parser Stmt
dimStmt = do
  _ <- stmtKeyword DimKeyword
  identifier <- ident
  size <- parens integer
  case identifier of
    IdentStrIdent strIdent' -> do
      len <- optional (binOperator MultiplyOp *> integer)
      return
        ( DimStmt
            ( DimString
                { dimStringVarName = strIdent',
                  dimStringSize = size,
                  dimStringLength = fromMaybe 16 len
                }
            )
        )
    IdentNumIdent numIdent' ->
      return
        ( DimStmt
            ( DimNumeric
                { dimNumericVarName = numIdent',
                  dimNumericSize = size
                }
            )
        )

dataStmt :: Parser Stmt
dataStmt = do
  _ <- stmtKeyword DataKeyword
  DataStmt <$> commaSeparated expression

readStmt :: Parser Stmt
readStmt = do
  _ <- stmtKeyword ReadKeyword
  ReadStmt <$> commaSeparated lvalue

restoreStmt :: Parser Stmt
restoreStmt = do
  _ <- stmtKeyword RestoreKeyword
  RestoreStmt <$> expression

stmt :: Bool -> Parser Stmt
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

line :: Parser Line
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
                    printExprs = [Expr {exprInner = StrLitExpr label, exprType = ExprUnknownType}],
                    printEnding = PrintEndingNewLine,
                    printUsingClause = Nothing
                  }
              ]
          }
    _ -> do
      _ <- newline
      return Line {lineNumber, lineLabel, lineStmts}

program :: Parser Program
program = do
  sc
  Program <$> many line

parseProgram :: String -> String -> IO (Either String Program)
parseProgram fileName contents = do
  result <- runParserT program () fileName contents
  case result of
    Left err -> return (Left (show err))
    Right prog -> return (Right prog)