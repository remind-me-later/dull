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

data PrintStmtKind = SemiColon | Newline deriving (Eq)

data PrintExpr = PrintStr StrExpr | PrintExp Expr deriving (Show, Eq)

instance Show PrintStmtKind where
  show Parser.SemiColon = "';'"
  show Parser.Newline = "'\\n'"

data Stmt
  = LetStmt VarDef Expr
  | IfStmt Expr Stmt
  | PrintStmt PrintExpr PrintStmtKind
  | EndStmt
  deriving (Show, Eq)

-- A line starts with a line number and can contain multiple statements separated by ":"
-- example: 10 LET A = 5
-- example: 20 PRINT A: A = 5
data Line
  = Line Int [Stmt]
  deriving (Show, Eq)

newtype Program = Program [Line] deriving (Show, Eq)

type TParser o = Parser [Token] o

identifier :: TParser Ident
identifier = Parser (\case Identifier s : rest -> Just (Ident s, rest); _ -> Nothing)

number :: TParser Int
number = Parser (\case Number n : rest -> Just (n, rest); _ -> Nothing)

operator :: Operator -> TParser Operator
operator op = Parser (\case Operator op' : rest | op == op' -> Just (op, rest); _ -> Nothing)

stringLiteral :: TParser String
stringLiteral = Parser (\case StringLiteral s : rest -> Just (s, rest); _ -> Nothing)

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
      maybeOp <- optional (operator Equal)
      case maybeOp of
        Just op -> BinExpr left op <$> eqExpr
        Nothing -> return left

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

-- In BASIC a let statment can optionally begin with the LET keyword
-- example: LET A = 5
-- example: A = 5
letStmt :: TParser Stmt
letStmt =
  explicitLetStmt
    <|> implicitLetStmt

implicitLetStmt :: TParser Stmt
implicitLetStmt = do
  ident <- identifier
  _ <- operator Equal
  LetStmt (VarDef ident Num) <$> expr

explicitLetStmt :: TParser Stmt
explicitLetStmt = satisfy (== Keyword Let) *> implicitLetStmt

explicitPrintStmt :: TParser Stmt
explicitPrintStmt = do
  _ <- satisfy (== Keyword Print)
  strExpr' <- strExpr
  kind <- optional (satisfy (== Punctuation Token.SemiColon))
  case kind of
    Just (Punctuation Token.SemiColon) -> return (PrintStmt (PrintStr strExpr') Parser.SemiColon)
    _ -> return (PrintStmt (PrintStr strExpr') Parser.Newline)

implicitPrintStmt :: TParser Stmt
implicitPrintStmt = do
  strExpr' <- strExpr
  kind <- optional (satisfy (== Punctuation Token.SemiColon))
  case kind of
    Just (Punctuation Token.SemiColon) -> return (PrintStmt (PrintStr strExpr') Parser.SemiColon)
    _ -> return (PrintStmt (PrintStr strExpr') Parser.Newline)

implicitPrintStmtExpr :: TParser Stmt
implicitPrintStmtExpr = do
  strExpr' <- expr
  kind <- optional (satisfy (== Punctuation Token.SemiColon))
  case kind of
    Just (Punctuation Token.SemiColon) -> return (PrintStmt (PrintExp strExpr') Parser.SemiColon)
    _ -> return (PrintStmt (PrintExp strExpr') Parser.Newline)

printStmt :: TParser Stmt
printStmt =
  explicitPrintStmt
    <|> implicitPrintStmt
    <|> implicitPrintStmtExpr

endStmt :: TParser Stmt
endStmt = satisfy (== Keyword End) $> EndStmt

-- FIXME: If the statement of the THEN branch is a let statement the LET keyword MUST be used
ifStmt :: TParser Stmt
ifStmt = do
  _ <- satisfy (== Keyword If)
  cond <- expr
  _ <- optional (satisfy (== Keyword Then))
  IfStmt cond <$> stmt

stmt :: TParser Stmt
stmt =
  letStmt
    <|> endStmt
    <|> printStmt
    <|> ifStmt

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
