module Ast.Parser where

import Ast.Types
import Control.Applicative (Alternative (many, (<|>)), optional)
import Data.Functor (($>))
import Text.Parsec
  ( ParsecT,
    char,
    many1,
    runParserT,
    satisfy,
    sepBy,
    skipMany,
    space,
    string,
    try,
  )

type Parser = ParsecT String () IO

-- space consumer
sc :: Parser ()
sc = skipMany (space $> ())

lex :: Parser a -> Parser a
lex p = p <* sc

number :: Parser Double -- FIXME: should use the Number representation of BASIC
number = Ast.Parser.lex $ do
  wholePart <- many1 (satisfy (`elem` ['0' .. '9']))
  decimalPart <- optional (char '.' *> many1 (satisfy (`elem` ['0' .. '9'])))
  let numberStr = wholePart ++ maybe "" ('.' :) decimalPart
  return (read numberStr)

-- Variables consist of the following:
-- \[A-Z][A-Z\d]$?
-- the dollar indicates a string variable
ident :: Parser Ident
ident = Ast.Parser.lex $ do
  firstChar <- satisfy (`elem` ['A' .. 'Z'])
  secondChar <- optional (satisfy (`elem` ['A' .. 'Z'] ++ ['0' .. '9']))
  dollar <- optional (char '$')
  let varName = firstChar : maybe "" (: []) secondChar
  case dollar of
    Just _ -> return (StrVar varName)
    Nothing -> return (NumVar varName)

functionName :: Parser FunctionName -- TODO: should arity be defined here?
functionName = Ast.Parser.lex $ do
  try (string "MID$" $> MidStr Arity3)
    <|> try (string "LEFT$" $> LeftStr Arity2)
    <|> try (string "RIGHT$" $> RightStr Arity2)
    <|> try (string "INKEY$" $> InkeyStr Arity0)
    <|> try (string "POINT" $> Point Arity1)
    <|> try (string "RND" $> Rnd Arity1)
    <|> try (string "INT" $> Int Arity1)

stringLiteral :: Parser String
stringLiteral = Ast.Parser.lex $ do
  _ <- char '"'
  content <- many (satisfy (/= '"'))
  _ <- char '"'
  return content

operation :: Operation -> Parser Operation
operation op = Ast.Parser.lex $ do
  try (string (show op) $> op)

stmtKeyword :: StmtKeyword -> Parser StmtKeyword
stmtKeyword keyword = Ast.Parser.lex $ do
  try (string (show keyword) $> keyword)

punctuation :: Parser Punctuation
punctuation = Ast.Parser.lex $ do
  try (char ',' $> Comma)
    <|> try (char '.' $> Dot)
    <|> try (char ':' $> Colon)
    <|> try (char ';' $> SemiColon)
    <|> try (char '(' $> LeftParen)
    <|> try (char ')' $> RightParen)
    <|> try (char '$' $> Dollar)
    <|> try (char '\n' $> NewLine)
    <|> try (char '#' $> Hashtag)

parens :: Parser a -> Parser a
parens p = Ast.Parser.lex $ do
  _ <- char '('
  result <- p
  _ <- char ')'
  return result

commaSeparated :: Parser a -> Parser [a]
commaSeparated p = Ast.Parser.lex $ sepBy p (Ast.Parser.lex (char ','))

expression :: Parser Expr
expression = comparisonExpr
  where
    factor = parenExpr <|> numLitExpr <|> strLitExpr <|> funCallExpr <|> numVarExpr
      where
        numLitExpr = NumLitExpr <$> number
        strLitExpr = StrLitExpr <$> stringLiteral
        numVarExpr = VarExpr <$> ident
        parenExpr = parens expression
        funCallExpr = do
          f <- functionName
          args <- parens (commaSeparated expression)
          return (FunCallExpr f args)

    mulDivExpr = do
      left <- factor
      maybeOp <- optional (operation Multiply <|> operation Divide <|> operation Caret)
      case maybeOp of
        Just op -> BinExpr left op <$> mulDivExpr
        Nothing -> return left

    addSubExpr = do
      left <- mulDivExpr
      maybeOp <- optional (operation Add <|> operation Subtract)
      case maybeOp of
        Just op -> BinExpr left op <$> addSubExpr
        Nothing -> return left

    orExpr = do
      left <- addSubExpr
      maybeOp <- optional (operation Or)
      case maybeOp of
        Just op -> BinExpr left op <$> orExpr
        Nothing -> return left

    andExpr = do
      left <- orExpr
      maybeOp <- optional (operation And)
      case maybeOp of
        Just op -> BinExpr left op <$> andExpr
        Nothing -> return left

    comparisonExpr = do
      left <- andExpr
      maybeOp <- optional (operation Equal <|> operation LessThan <|> operation GreaterThan <|> operation LessThanOrEqual <|> operation GreaterThanOrEqual <|> operation NotEqual)
      case maybeOp of
        Just op -> BinExpr left op <$> comparisonExpr
        Nothing -> return left

assignment :: Parser Assignment
assignment = do
  v <- ident
  _ <- operation Equal
  Assignment v <$> expression

letStmt :: Parser Stmt
letStmt = do
  _ <- stmtKeyword Let
  assignments <- commaSeparated assignment
  return (LetStmt assignments)

printStmt :: Parser Stmt
printStmt = do
  k <- stmtKeyword Print <|> stmtKeyword Pause <|> stmtKeyword Using
  exprs <- commaSeparated expression
  semi <- optional (Ast.Parser.lex (char ';'))
  return
    ( PrintStmt
        ( case k of
            Print -> PrintKindPrint
            Pause -> PrintKindPause
            Using -> PrintKindUsing
            _ -> error "Unexpected print kind" -- FIXME: reshape AST to avoid this
        )
        exprs
        ( case semi of
            Just _ -> PrintEndingNoNewLine
            Nothing -> PrintEndingNewLine
        )
    )

gPrintStmt :: Parser Stmt
gPrintStmt = do
  _ <- stmtKeyword Gprint
  exprs <- commaSeparated expression
  semi <- optional (Ast.Parser.lex (char ';'))
  return
    ( GprintStmt
        exprs
        ( case semi of
            Just _ -> PrintEndingNoNewLine
            Nothing -> PrintEndingNewLine
        )
    )

gCursorStmt :: Parser Stmt
gCursorStmt = do
  _ <- stmtKeyword GCursor
  GCursorStmt <$> expression

cursorStmt :: Parser Stmt
cursorStmt = do
  _ <- stmtKeyword Cursor
  CursorStmt <$> expression

inputStmt :: Parser Stmt
inputStmt = do
  _ <- stmtKeyword Input
  maybePrintExpr <- optional (expression <* Ast.Parser.lex (char ';'))
  InputStmt maybePrintExpr <$> expression

endStmt :: Parser Stmt
endStmt = Ast.Parser.lex (stmtKeyword End) $> EndStmt

ifStmt :: Parser Stmt
ifStmt = do
  _ <- stmtKeyword If
  cond <- expression
  _ <- stmtKeyword Then
  IfStmt cond <$> stmt

forStmt :: Parser Stmt
forStmt = do
  _ <- stmtKeyword For
  a <- assignment
  _ <- stmtKeyword To
  ForStmt a <$> expression

nextStmt :: Parser Stmt
nextStmt = stmtKeyword Next *> (NextStmt <$> ident)

clearStmt :: Parser Stmt
clearStmt = stmtKeyword Clear $> ClearStmt

clsStmt :: Parser Stmt
clsStmt = stmtKeyword Cls $> ClsStmt

randomStmt :: Parser Stmt
randomStmt = stmtKeyword Random $> RandomStmt

gotoTarget :: Parser GotoTarget
gotoTarget =
  try (GoToLabel <$> stringLiteral)
    <|> GoToLine <$> number

gotoStmt :: Parser Stmt
gotoStmt = stmtKeyword Goto *> (GoToStmt <$> gotoTarget)

gosubStmt :: Parser Stmt
gosubStmt = stmtKeyword Gosub *> (GoSubStmt <$> gotoTarget)

waitStmt :: Parser Stmt
waitStmt = stmtKeyword Wait *> (WaitStmt <$> optional expression)

comment :: Parser Stmt
comment = do
  _ <- stmtKeyword Remark
  _ <- many (satisfy (/= '\n'))
  return Comment

beepStmt :: Parser Stmt
beepStmt = do
  _ <- stmtKeyword Beep
  exprs <- commaSeparated expression
  return (BeepStmt exprs)

returnStmt :: Parser Stmt
returnStmt = stmtKeyword Return $> ReturnStmt

pokeStmt :: Parser Stmt
pokeStmt = do
  _ <- stmtKeyword Poke
  kind <- optional (Ast.Parser.lex (char '#'))
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
  _ <- stmtKeyword Dim
  DimStmt <$> expression

dataStmt :: Parser Stmt
dataStmt = do
  _ <- stmtKeyword Data
  DataStmt <$> commaSeparated expression

readStmt :: Parser Stmt
readStmt = do
  _ <- stmtKeyword Read
  ReadStmt <$> commaSeparated expression

restoreStmt :: Parser Stmt
restoreStmt = do
  _ <- stmtKeyword Restore
  RestoreStmt <$> optional expression

stmt :: Parser Stmt
stmt =
  try letStmt
    <|> try endStmt
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
    <|> restoreStmt

line :: Parser Line
line = do
  lineNumber <- number
  label <- optional stringLiteral
  stmts <- sepBy stmt (Ast.Parser.lex (char ':'))
  _ <- optional (Ast.Parser.lex (char '\n'))
  return (Line lineNumber label stmts)

program :: Parser Program
program = Program <$> many line

parseProgram :: String -> String -> IO (Either String Program)
parseProgram fileName contents = do
  result <- runParserT program () fileName contents
  case result of
    Left err -> return (Left (show err))
    Right prog -> return (Right prog)