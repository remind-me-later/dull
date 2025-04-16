{-# LANGUAGE LambdaCase #-}

module Parser (NumExpr (..), Stmt (..), TParser, Program (..), program) where

import Control.Applicative (Alternative (many, (<|>)), optional)
import Data.Functor (($>))
import ParserCombinators (Parser (..), satisfy, sepBy)
import Token qualified (Keyword (..), Operator (..), Punctuation (..), Token (..))

data Ty = Num | Str deriving (Show, Eq)

data Ident = VarDef String Ty deriving (Show, Eq)

data NumExpr
  = BinExpr NumExpr Token.Operator NumExpr
  | NumExpr Int
  | NumVar Ident
  | StrExpr String
  deriving (Show, Eq)

data StrExpr
  = StrLit
      String
  | StrVar
      Ident
  | Concat
      StrExpr
      StrExpr
  deriving (Show, Eq)

type PrintExpr = Either NumExpr StrExpr

data PrintKind = NewLine | NoNewLine deriving (Show, Eq)

data Assignment
  = Assignment
      Ident
      NumExpr
  deriving (Show, Eq)

data GotoTarget
  = GoToIdent String
  | GoToLine Int
  deriving (Show, Eq)

data Stmt
  = LetStmt [Assignment]
  | IfStmt NumExpr Stmt
  | PrintStmt [PrintExpr] PrintKind
  | InputStmt (Maybe PrintExpr) NumExpr
  | EndStmt
  | Comment
  | ForStmt Assignment NumExpr
  | NextStmt Ident
  | ClearStmt
  | GoToStmt GotoTarget
  | GoSubStmt GotoTarget
  | WaitStmt NumExpr
  deriving (Show, Eq)

-- A line starts with a line number and can contain multiple statements separated by ":"
-- example: 10 LET A = 5
-- example: 20 PRINT A: A = 5
data Line
  = Line {lineLabel :: Maybe String, lineNumber :: Int, lineStmts :: [Stmt]}
  deriving (Show, Eq)

newtype Program = Program {programLines :: [Line]} deriving (Show, Eq)

type TParser o = Parser [Token.Token] o

-- An identifier is a token identifier that can optionally end with a "$" sign to indicate a string variable
-- example: A$, B, C$
-- example: A = 5
-- example: A$ = "Hello"
ident :: TParser Ident
ident = do
  s <- atom
  maybeDollar <- optional (satisfy (== Token.Punctuation Token.Dollar))
  case maybeDollar of
    Just _ -> return (VarDef s Str)
    Nothing -> return (VarDef s Num)
  where
    atom = Parser (\case Token.Identifier s : rest -> Just (s, rest); _ -> Nothing)

number :: TParser Int
number = Parser (\case Token.Number n : rest -> Just (n, rest); _ -> Nothing)

operator :: Token.Operator -> TParser Token.Operator
operator op = Parser (\case Token.Operator op' : rest | op == op' -> Just (op, rest); _ -> Nothing)

expr :: TParser NumExpr
expr = eqExpr
  where
    factor = parenExpr <|> numberExpr <|> identifierExpr
      where
        numberExpr = NumExpr <$> number
        identifierExpr = NumVar <$> ident
        parenExpr = satisfy (== Token.Punctuation Token.LeftParen) *> expr <* satisfy (== Token.Punctuation Token.RightParen)

    mulExpr = do
      left <- factor
      maybeOp <- optional (operator Token.Multiply)
      case maybeOp of
        Just op -> BinExpr left op <$> mulExpr
        Nothing -> return left

    addSubExpr = do
      left <- mulExpr
      maybeOp <- optional (operator Token.Add <|> operator Token.Subtract)
      case maybeOp of
        Just op -> BinExpr left op <$> addSubExpr
        Nothing -> return left

    eqExpr = do
      left <- addSubExpr
      maybeOp <- optional (operator Token.Equal <|> operator Token.LessThan <|> operator Token.GreaterThan)
      case maybeOp of
        Just op -> BinExpr left op <$> eqExpr
        Nothing -> return left

-- String expressions allow concatenation
strExpr :: TParser StrExpr
strExpr = do
  left <- strFactor
  maybeOp <- optional (operator Token.Add)
  case maybeOp of
    Just _ -> Concat left <$> strExpr
    Nothing -> return left
  where
    strFactor = strLit <|> strVar
      where
        strLit = StrLit <$> stringLiteral
        strVar = StrVar <$> ident <* satisfy (== Token.Punctuation Token.Dollar)

stringLiteral :: Parser [Token.Token] String
stringLiteral = Parser (\case Token.StringLiteral s : rest -> Just (s, rest); _ -> Nothing)

-- Either a string expression or a numeric expression
printExpr :: TParser PrintExpr
printExpr = Left <$> expr <|> Right <$> strExpr

-- In our version of BASIC the LET keyword is mandatory
-- example: LET A = 5
-- multiple variable can be assigned, separated by commas
-- example: LET A = 5, B = 10
assignment :: TParser Assignment
assignment = do
  v <- ident
  _ <- operator Token.Equal
  Assignment v <$> expr

letStmt :: TParser Stmt
letStmt = do
  _ <- satisfy (== Token.Keyword Token.Let)
  assignments <- sepBy (== Token.Punctuation Token.Comma) assignment
  return (LetStmt assignments)

-- A print statement can contain multiple expressions separated by ';'
-- example: PRINT A; B
-- If the print statement ends with a semicolon, the next print statement will be on the same line
-- example: PRINT "A"; "B"
--          PRINT "C"
-- will print: AB
--             C
-- whereas: PRINT "A"; "B";
--          PRINT "C"
-- will print: ABC
printStmt :: TParser Stmt
printStmt = do
  _ <- satisfy (== Token.Keyword Token.Print)
  printExpr' <- printExpr
  restPrintExprs <- many (satisfy (== Token.Punctuation Token.SemiColon) *> printExpr)
  semi <- optional (satisfy (== Token.Punctuation Token.SemiColon))
  return
    ( PrintStmt
        (printExpr' : restPrintExprs)
        ( case semi of
            Just _ -> NoNewLine
            Nothing -> NewLine
        )
    )

-- An input statement can contain an optional print expression
-- example: INPUT A$, B$
-- example: INPUT "Accept? (Y/N)"; A$
inputStmt :: TParser Stmt
inputStmt = do
  _ <- satisfy (== Token.Keyword Token.Input)
  maybePrintExpr <- optional (printExpr <* satisfy (== Token.Punctuation Token.SemiColon))
  InputStmt maybePrintExpr <$> expr

endStmt :: TParser Stmt
endStmt = satisfy (== Token.Keyword Token.End) $> EndStmt

-- FIXME: If the statement of the THEN branch is a let statement the LET keyword MUST be used
ifStmt :: TParser Stmt
ifStmt = do
  _ <- satisfy (== Token.Keyword Token.If)
  cond <- expr
  _ <- satisfy (== Token.Keyword Token.Then)
  IfStmt cond <$> stmt

forStmt :: TParser Stmt
forStmt = do
  _ <- satisfy (== Token.Keyword Token.For)
  assign <- assignment
  _ <- satisfy (== Token.Keyword Token.To)
  ForStmt assign <$> expr

nextStmt :: TParser Stmt
nextStmt = satisfy (== Token.Keyword Token.Next) *> (NextStmt <$> ident)

clearStmt :: TParser Stmt
clearStmt = satisfy (== Token.Keyword Token.Clear) $> ClearStmt

gotoTarget :: TParser GotoTarget
gotoTarget =
  GoToIdent <$> stringLiteral
    <|> GoToLine <$> number

gotoStmt :: TParser Stmt
gotoStmt = satisfy (== Token.Keyword Token.Goto) *> (GoToStmt <$> gotoTarget)

gosubStmt :: TParser Stmt
gosubStmt = satisfy (== Token.Keyword Token.Gosub) *> (GoSubStmt <$> gotoTarget)

waitStmt :: TParser Stmt
waitStmt = satisfy (== Token.Keyword Token.Wait) *> (WaitStmt <$> expr)

comment :: TParser Stmt
comment = do
  _ <- satisfy (== Token.Keyword Token.Remark)
  _ <- many (satisfy (/= Token.Punctuation Token.NewLine))
  return Comment

stmt :: TParser Stmt
stmt =
  letStmt
    <|> endStmt
    <|> printStmt
    <|> ifStmt
    <|> comment
    <|> inputStmt
    <|> forStmt
    <|> nextStmt
    <|> clearStmt
    <|> gotoStmt
    <|> gosubStmt
    <|> waitStmt

-- Parse LET statements last to avoid ambiguity with expressions
-- multiple statements in the same line MUST be separated by ":"
line :: TParser Line
line = do
  lineNumber <- number
  label <- optional stringLiteral
  stmts <- sepBy (== Token.Punctuation Token.Colon) stmt
  _ <- optional (satisfy (== Token.Punctuation Token.NewLine))
  return (Line label lineNumber stmts)

program :: TParser Program
program = do
  programLines <- many line
  return (Program programLines)
