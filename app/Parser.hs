{-# LANGUAGE LambdaCase #-}

module Parser (Expr (..), Stmt (..), TParser, Program (..), program) where

import Control.Applicative (Alternative (many, (<|>)), optional)
import Data.Functor (($>))
import ParserCombinators (Parser (..), satisfy)
import Token (Keyword (..), Operator (..), Punctuation (..), Token (..))

newtype Ident = Ident String deriving (Show, Eq)

data Expr
  = BinExpr Expr Operator Expr
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

data Ty = Num | Str deriving (Show, Eq)

data VarDef = VarDef Ident Ty deriving (Show, Eq)

data PrintExpr = PrintStr StrExpr | PrintExp Expr deriving (Show, Eq)

data PrintKind = NewLine | NoNewLine deriving (Show, Eq)

data Assignment
  = Assignment
      VarDef
      Expr
  deriving (Show, Eq)

data Stmt
  = LetStmt [Assignment]
  | IfStmt Expr Stmt
  | PrintStmt [PrintExpr] PrintKind
  | InputStmt (Maybe PrintExpr) Expr
  | EndStmt
  | Comment
  | ForStmt Assignment Expr
  | NextStmt VarDef
  deriving (Show, Eq)

-- A line starts with a line number and can contain multiple statements separated by ":"
-- example: 10 LET A = 5
-- example: 20 PRINT A: A = 5
data Line
  = Line {lineNumber :: Int, lineStmts :: [Stmt]}
  deriving (Show, Eq)

newtype Program = Program {programLines :: [Line]} deriving (Show, Eq)

type TParser o = Parser [Token] o

identifier :: TParser Ident
identifier = Parser (\case Identifier s : rest -> Just (Ident s, rest); _ -> Nothing)

number :: TParser Int
number = Parser (\case Number n : rest -> Just (n, rest); _ -> Nothing)

operator :: Operator -> TParser Operator
operator op = Parser (\case Operator op' : rest | op == op' -> Just (op, rest); _ -> Nothing)

expr :: TParser Expr
expr = eqExpr
  where
    factor = parenExpr <|> numberExpr <|> identifierExpr
      where
        numberExpr = NumExpr <$> number
        identifierExpr = NumVar <$> identifier
        parenExpr = satisfy (== Punctuation LeftParen) *> expr <* satisfy (== Punctuation RightParen)

    mulExpr = do
      left <- factor
      maybeOp <- optional (operator Multiply)
      case maybeOp of
        Just op -> BinExpr left op <$> mulExpr
        Nothing -> return left

    addSubExpr = do
      left <- mulExpr
      maybeOp <- optional (operator Add <|> operator Subtract)
      case maybeOp of
        Just op -> BinExpr left op <$> addSubExpr
        Nothing -> return left

    eqExpr = do
      left <- addSubExpr
      maybeOp <- optional (operator Equal <|> operator LessThan <|> operator GreaterThan)
      case maybeOp of
        Just op -> BinExpr left op <$> eqExpr
        Nothing -> return left

-- String expressions allow concatenation
strExpr :: TParser StrExpr
strExpr = do
  left <- strFactor
  maybeOp <- optional (operator Add)
  case maybeOp of
    Just _ -> Concat left <$> strExpr
    Nothing -> return left
  where
    strFactor = strLit <|> strVar
      where
        strLit = StrLit <$> stringLiteral
        strVar = StrVar <$> identifier <* satisfy (== Punctuation Dollar)

    stringLiteral = Parser (\case StringLiteral s : rest -> Just (s, rest); _ -> Nothing)

-- Either a string expression or a numeric expression
printExpr :: TParser PrintExpr
printExpr = PrintExp <$> expr <|> PrintStr <$> strExpr

-- In our version of BASIC the LET keyword is mandatory
-- example: LET A = 5
-- multiple variable can be assigned, separated by commas
-- example: LET A = 5, B = 10
assignment :: TParser Assignment
assignment = do
  ident <- identifier
  _ <- operator Equal
  Assignment (VarDef ident Num) <$> expr

letStmt :: TParser Stmt
letStmt = do
  _ <- satisfy (== Keyword Let)
  assign <- assignment
  restAssigns <- many (satisfy (== Punctuation Comma) *> assignment)
  return (LetStmt (assign : restAssigns))

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
  _ <- satisfy (== Keyword Print)
  printExpr' <- printExpr
  restPrintExprs <- many (satisfy (== Punctuation SemiColon) *> printExpr)
  semi <- optional (satisfy (== Punctuation SemiColon))
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
  _ <- satisfy (== Keyword Input)
  maybePrintExpr <- optional (printExpr <* satisfy (== Punctuation SemiColon))
  InputStmt maybePrintExpr <$> expr

endStmt :: TParser Stmt
endStmt = satisfy (== Keyword End) $> EndStmt

-- FIXME: If the statement of the THEN branch is a let statement the LET keyword MUST be used
ifStmt :: TParser Stmt
ifStmt = do
  _ <- satisfy (== Keyword If)
  cond <- expr
  _ <- satisfy (== Keyword Then)
  IfStmt cond <$> stmt

forStmt :: TParser Stmt
forStmt = do
  _ <- satisfy (== Keyword For)
  assign <- assignment
  _ <- satisfy (== Keyword To)
  ForStmt assign <$> expr

nextStmt :: TParser Stmt
nextStmt = do
  _ <- satisfy (== Keyword Next)
  varDef <- VarDef <$> identifier <*> pure Num
  return (NextStmt varDef)

comment :: TParser Stmt
comment = do
  _ <- satisfy (== Keyword Remark)
  _ <- many (satisfy (/= Punctuation Token.Newline))
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

-- Parse LET statements last to avoid ambiguity with expressions
-- multiple statements in the same line MUST be separated by ":"
line :: TParser Line
line = do
  lineNumber <- number
  firstStmt <- stmt
  restStmts <- many (optional (satisfy (== Punctuation Colon)) *> stmt)
  _ <- optional (satisfy (== Punctuation Token.Newline))
  return (Line lineNumber (firstStmt : restStmts))

program :: TParser Program
program = do
  programLines <- many line
  return (Program programLines)
