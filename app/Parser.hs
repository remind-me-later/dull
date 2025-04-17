{-# LANGUAGE LambdaCase #-}

module Parser (Expr (..), Stmt (..), TParser, Program (..), program) where

import Control.Applicative (Alternative (many, (<|>)), optional)
import Data.Functor (($>))
import Data.List (intercalate)
import ParserCombinators (Parser (..), satisfy, sepBy)
import Token qualified (ArithmeticOp (..), Keyword (..), LogicalOp (..), Punctuation (..), Token (..))

data Ident where
  NumIdent :: String -> Ident
  StrIdent :: String -> Ident
  deriving (Eq)

instance Show Ident where
  show (NumIdent x) = x
  show (StrIdent s) = s ++ "$"

data Expr where
  NumBinExpr :: Expr -> Token.ArithmeticOp -> Expr -> Expr
  NumLitExpr :: Int -> Expr
  NumVarExpr :: Ident -> Expr
  StrLitExpr :: String -> Expr
  StrVarExpr :: Ident -> Expr
  StrCatExpr :: Expr -> Expr -> Expr
  BoolBinExpr :: Expr -> Token.LogicalOp -> Expr -> Expr
  deriving (Eq)

instance Show Expr where
  show (NumBinExpr left op right) = "(" ++ show left ++ " " ++ show op ++ " " ++ show right ++ ")"
  show (BoolBinExpr left op right) = "(" ++ show left ++ " " ++ show op ++ " " ++ show right ++ ")"
  show (NumLitExpr n) = show n
  show (NumVarExpr x) = show x
  show (StrVarExpr s) = show s
  show (StrLitExpr s) = "\"" ++ s ++ "\""
  show (StrCatExpr left right) = "(" ++ show left ++ " + " ++ show right ++ ")"

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

-- An identifier is a token identifier that can optionally end with a "$" sign to indicate a string variable
-- example: A$, B, C$
-- example: A = 5
-- example: A$ = "Hello"
ident :: TParser Ident
ident = do
  s <- atom
  d <- optional (satisfy (== Token.Punctuation Token.Dollar))
  case d of
    Just _ -> return (StrIdent s)
    Nothing -> return (NumIdent s)
  where
    atom = Parser (\case Token.Identifier s : rest -> Just (s, rest); _ -> Nothing)

strIdent :: TParser Ident
strIdent = do
  s <- atom
  _ <- satisfy (== Token.Punctuation Token.Dollar)
  return (StrIdent s)
  where
    atom = Parser (\case Token.Identifier s : rest -> Just (s, rest); _ -> Nothing)

number :: TParser Int
number = Parser (\case Token.Number n : rest -> Just (n, rest); _ -> Nothing)

arithmeticOp :: Token.ArithmeticOp -> TParser Token.ArithmeticOp
arithmeticOp op = Parser (\case Token.ArithmeticOp op' : rest | op == op' -> Just (op, rest); _ -> Nothing)

logicalOp :: Token.LogicalOp -> TParser Token.LogicalOp
logicalOp op = Parser (\case Token.LogicalOp op' : rest | op == op' -> Just (op, rest); _ -> Nothing)

expr :: TParser Expr
expr = strExpr <|> numExpr
  where
    numExpr :: TParser Expr
    numExpr = compExpr
      where
        factor = parenExpr <|> numLitExpr <|> numVarExpr
          where
            numLitExpr = NumLitExpr <$> number
            numVarExpr = NumVarExpr <$> ident
            parenExpr = satisfy (== Token.Punctuation Token.LeftParen) *> numExpr <* satisfy (== Token.Punctuation Token.RightParen)

        mulExpr = do
          left <- factor
          maybeOp <- optional (arithmeticOp Token.Multiply)
          case maybeOp of
            Just op -> NumBinExpr left op <$> mulExpr
            Nothing -> return left

        addSubExpr = do
          left <- mulExpr
          maybeOp <- optional (arithmeticOp Token.Add <|> arithmeticOp Token.Subtract)
          case maybeOp of
            Just op -> NumBinExpr left op <$> addSubExpr
            Nothing -> return left

        compExpr = do
          left <- addSubExpr
          maybeOp <-
            optional
              ( logicalOp Token.Equal
                  <|> logicalOp Token.LessThan
                  <|> logicalOp Token.GreaterThan
                  <|> logicalOp Token.LessThanOrEqual
                  <|> logicalOp Token.GreaterThanOrEqual
                  <|> logicalOp Token.NotEqual
              )
          case maybeOp of
            Just op -> BoolBinExpr left op <$> compExpr
            Nothing -> return left

    -- String expressions allow concatenation
    strExpr :: TParser Expr
    strExpr = do
      left <- strFactor
      maybeOp <- optional (arithmeticOp Token.Add)
      case maybeOp of
        Just _ -> StrCatExpr left <$> strExpr
        Nothing -> return left
      where
        strFactor = strLit <|> strVar
          where
            strLit = StrLitExpr <$> stringLiteral
            strVar = StrVarExpr <$> strIdent

stringLiteral :: TParser String
stringLiteral = Parser (\case Token.StringLiteral s : rest -> Just (s, rest); _ -> Nothing)

-- In our version of BASIC the LET keyword is mandatory
-- example: LET A = 5
-- multiple variable can be assigned, separated by commas
-- example: LET A = 5, B = 10
assignment :: TParser Assignment
assignment = do
  v <- ident
  _ <- logicalOp Token.Equal
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
  return (Line lineNumber label stmts)

program :: TParser Program
program = Program <$> many line
