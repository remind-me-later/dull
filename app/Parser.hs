{-# LANGUAGE LambdaCase #-}

module Parser (Expr (..), Stmt (..), TParser, Program (..), program) where

import Control.Applicative (Alternative (many, (<|>)), optional)
import Data.Functor (($>))
import Data.List (intercalate)
import ParserCombinators (Parser (..), satisfy, sepBy)
import Token (Token (Identifier), Ty (NumType))
import Token qualified (Id (..), Keyword (..), Operation (..), Punctuation (..), Token (..), Ty (..))

data Ident where
  NumIdent :: String -> Ident
  StrIdent :: String -> Ident
  deriving (Eq)

instance Show Ident where
  show (NumIdent s) = s
  show (StrIdent s) = s ++ "$"

data Expr where
  BinExpr :: Expr -> Token.Operation -> Expr -> Expr
  NumLitExpr :: Int -> Expr
  VarExpr :: Ident -> Expr
  StrLitExpr :: String -> Expr
  FunCallExpr :: Ident -> [Expr] -> Expr
  deriving (Eq)

instance Show Expr where
  show (BinExpr left op right) = "(" ++ show left ++ " " ++ show op ++ " " ++ show right ++ ")"
  show (NumLitExpr n) = show n
  show (VarExpr x) = show x
  show (StrLitExpr s) = "\"" ++ s ++ "\""
  show (FunCallExpr f args) = show f ++ "(" ++ intercalate ", " (show <$> args) ++ ")"

data PrintEnding where
  NewLine :: PrintEnding
  NoNewLine :: PrintEnding
  deriving (Show, Eq)

data PrintKind where
  Print :: PrintKind
  Pause :: PrintKind
  deriving (Show, Eq)

data Assignment where
  Assignment :: Ident -> Expr -> Assignment
  deriving (Eq)

instance Show Assignment where
  show (Assignment x e) = show x ++ " = " ++ show e

data GotoTarget where
  GoToIdent :: String -> GotoTarget
  GoToLine :: Int -> GotoTarget
  deriving (Show, Eq)

data Stmt where
  LetStmt :: [Assignment] -> Stmt
  IfStmt :: Expr -> Stmt -> Stmt -- Should be BOOL even if we reject valid programs
  PrintStmt :: PrintKind -> [Expr] -> PrintEnding -> Stmt
  InputStmt :: Maybe Expr -> Expr -> Stmt
  EndStmt :: Stmt
  Comment :: Stmt
  ForStmt :: Assignment -> Expr -> Stmt
  NextStmt :: Ident -> Stmt
  ClearStmt :: Stmt
  GoToStmt :: GotoTarget -> Stmt
  GoSubStmt :: GotoTarget -> Stmt
  WaitStmt :: Expr -> Stmt
  ClsStmt :: Stmt
  RandomStmt :: Stmt
  GprintStmt :: [Expr] -> PrintEnding -> Stmt
  deriving (Eq)

instance Show Stmt where
  show (LetStmt assignments) = "LET " ++ intercalate ", " (show <$> assignments)
  show (IfStmt cond s) = "IF " ++ show cond ++ " THEN " ++ show s
  show (PrintStmt k exprs kind) =
    ( case k of
        Print -> "PRINT "
        Pause -> "PAUSE "
    )
      ++ intercalate "; " (show <$> exprs)
      ++ case kind of
        NewLine -> ""
        NoNewLine -> ";"
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
  show (WaitStmt e) = "WAIT " ++ show e
  show ClsStmt = "CLS"
  show RandomStmt = "RANDOM"
  show (GprintStmt exprs kind) =
    "GPRINT "
      ++ intercalate "; " (show <$> exprs)
      ++ case kind of
        NewLine -> ""
        NoNewLine -> ";"

-- A line starts with a line number and can contain multiple statements separated by ":"
-- example: 10 LET A = 5
-- example: 20 PRINT A: A = 5
data Line where
  Line ::
    { lineNumber :: Int,
      lineLabel :: Maybe String,
      lineStmts :: [Stmt]
    } ->
    Line
  deriving (Show, Eq)

newtype Program where
  Program :: {programLines :: [Line]} -> Program

instance Show Program where
  show (Program ls) =
    unlines $
      \case
        Line n Nothing stmts -> show n ++ " " ++ intercalate " : " (show <$> stmts)
        Line n (Just label) stmts -> show n ++ " \"" ++ label ++ "\" " ++ intercalate ": " (show <$> stmts)
        <$> ls

type TParser o = Parser [Token.Token] o

ident :: TParser Ident
ident = numIdent <|> strIdent
  where
    numIdent :: TParser Ident
    numIdent = Parser (\case Token.Identifier ((Token.Id s NumType)) : rest -> Just (NumIdent s, rest); _ -> Nothing)

    strIdent :: TParser Ident
    strIdent = Parser (\case Token.Identifier ((Token.Id s Token.StrType)) : rest -> Just (StrIdent s, rest); _ -> Nothing)

number :: TParser Int
number = Parser (\case Token.Number n : rest -> Just (n, rest); _ -> Nothing)

operation :: Token.Operation -> TParser Token.Operation
operation op = Parser (\case Token.Operation op' : rest | op == op' -> Just (op, rest); _ -> Nothing)

expr :: TParser Expr
expr = logicalExpr
  where
    factor = parenExpr <|> numLitExpr <|> strLitExpr <|> funCallExpr <|> numVarExpr
      where
        numLitExpr = NumLitExpr <$> number
        strLitExpr = StrLitExpr <$> stringLiteral
        numVarExpr = VarExpr <$> ident
        parenExpr = satisfy (== Token.Punctuation Token.LeftParen) *> expr <* satisfy (== Token.Punctuation Token.RightParen)
        funCallExpr = do
          f <- ident
          _ <- satisfy (== Token.Punctuation Token.LeftParen)
          args <- sepBy (== Token.Punctuation Token.Comma) expr
          _ <- satisfy (== Token.Punctuation Token.RightParen)
          return (FunCallExpr f args)

    mulDivExpr = do
      left <- factor
      maybeOp <- optional (operation Token.Multiply <|> operation Token.Divide)
      case maybeOp of
        Just op -> BinExpr left op <$> mulDivExpr
        Nothing -> return left

    addSubExpr = do
      left <- mulDivExpr
      maybeOp <-
        optional
          ( operation Token.Add
              <|> operation Token.Subtract
          )
      case maybeOp of
        Just op -> BinExpr left op <$> addSubExpr
        Nothing -> return left

    logicalExpr = do
      left <- addSubExpr
      maybeOp <-
        optional
          ( operation Token.Equal
              <|> operation Token.LessThan
              <|> operation Token.GreaterThan
              <|> operation Token.LessThanOrEqual
              <|> operation Token.GreaterThanOrEqual
              <|> operation Token.NotEqual
          )
      case maybeOp of
        Just op -> BinExpr left op <$> logicalExpr
        Nothing -> return left

stringLiteral :: TParser String
stringLiteral = Parser (\case Token.StringLiteral s : rest -> Just (s, rest); _ -> Nothing)

-- In our version of BASIC the LET keyword is mandatory
-- example: LET A = 5
-- multiple variable can be assigned, separated by commas
-- example: LET A = 5, B = 10
assignment :: TParser Assignment
assignment = do
  v <- ident
  _ <- operation Token.Equal
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
  k <- satisfy (== Token.Keyword Token.Print) <|> satisfy (== Token.Keyword Token.Pause)
  exprs <- sepBy (== Token.Punctuation Token.SemiColon) expr
  semi <- optional (satisfy (== Token.Punctuation Token.SemiColon))
  return
    ( PrintStmt
        ( case k of
            Token.Keyword Token.Print -> Print
            _ -> Pause
        )
        exprs
        ( case semi of
            Just _ -> NoNewLine
            Nothing -> NewLine
        )
    )

gprintStmt :: TParser Stmt
gprintStmt = do
  _ <- satisfy (== Token.Keyword Token.Gprint)
  exprs <- sepBy (== Token.Punctuation Token.SemiColon) expr
  semi <- optional (satisfy (== Token.Punctuation Token.SemiColon))
  return
    ( GprintStmt
        exprs
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
  maybePrintExpr <- optional (expr <* satisfy (== Token.Punctuation Token.SemiColon))
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
  a <- assignment
  _ <- satisfy (== Token.Keyword Token.To)
  ForStmt a <$> expr

nextStmt :: TParser Stmt
nextStmt = satisfy (== Token.Keyword Token.Next) *> (NextStmt <$> ident)

clearStmt :: TParser Stmt
clearStmt = satisfy (== Token.Keyword Token.Clear) $> ClearStmt

clsStmt :: TParser Stmt
clsStmt = satisfy (== Token.Keyword Token.Cls) $> ClsStmt

randomStmt :: TParser Stmt
randomStmt = satisfy (== Token.Keyword Token.Random) $> RandomStmt

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
    <|> clsStmt
    <|> randomStmt
    <|> gprintStmt

-- Parse LET statements last to avoid ambiguity with expressions
-- multiple statements in the same line MUST be separated by ":"
line :: TParser Line
line = do
  lineNumber <- number
  label <- optional stringLiteral
  stmts <- sepBy (== Token.Punctuation Token.Colon) stmt
  _ <- optional (satisfy (== Token.Punctuation Token.NewLine))
  return (Line lineNumber label stmts)

program :: TParser Program
program = Program <$> many line
