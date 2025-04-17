module Token
  ( Token (..),
    ArithmeticOp (..),
    LogicalOp (..),
    Keyword (..),
    Punctuation (..),
    tokens,
  )
where

import Control.Applicative (Alternative (many, some, (<|>)))
import Data.Functor (($>))
import ParserCombinators (Parser, char, satisfy, string)

data LogicalOp where
  Equal :: LogicalOp
  LessThan :: LogicalOp
  GreaterThan :: LogicalOp
  LessThanOrEqual :: LogicalOp
  GreaterThanOrEqual :: LogicalOp
  NotEqual :: LogicalOp
  deriving (Eq)

instance Show LogicalOp where
  show Equal = "="
  show LessThan = "<"
  show GreaterThan = ">"
  show LessThanOrEqual = "<="
  show GreaterThanOrEqual = ">="
  show NotEqual = "<>"

data ArithmeticOp where
  Add :: ArithmeticOp
  Subtract :: ArithmeticOp
  Multiply :: ArithmeticOp
  Divide :: ArithmeticOp
  deriving (Eq)

instance Show ArithmeticOp where
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide = "/"

data Punctuation where
  Comma :: Punctuation
  Colon :: Punctuation
  SemiColon :: Punctuation
  LeftParen :: Punctuation
  RightParen :: Punctuation
  NewLine :: Punctuation
  Dollar :: Punctuation
  deriving (Eq)

instance Show Punctuation where
  show Comma = ","
  show Colon = ":"
  show SemiColon = ";"
  show LeftParen = "("
  show RightParen = ")"
  show NewLine = "\n"
  show Dollar = "$"

data Keyword where
  Let :: Keyword
  If :: Keyword
  Then :: Keyword
  Input :: Keyword
  Print :: Keyword
  End :: Keyword
  Remark :: Keyword
  For :: Keyword
  To :: Keyword
  Next :: Keyword
  Clear :: Keyword
  Goto :: Keyword
  Gosub :: Keyword
  Wait :: Keyword
  Pause :: Keyword
  deriving (Eq)

instance Show Keyword where
  show Let = "LET"
  show If = "IF"
  show Then = "THEN"
  show Input = "INPUT"
  show Print = "PRINT"
  show End = "END"
  show Remark = "REM"
  show For = "FOR"
  show To = "TO"
  show Next = "NEXT"
  show Clear = "CLEAR"
  show Goto = "GOTO"
  show Gosub = "GOSUB"
  show Wait = "WAIT"
  show Pause = "PAUSE"

data Token where
  Identifier :: String -> Token
  Number :: Int -> Token
  ArithmeticOp :: ArithmeticOp -> Token
  LogicalOp :: LogicalOp -> Token
  Keyword :: Keyword -> Token
  Punctuation :: Punctuation -> Token
  StringLiteral :: String -> Token
  deriving (Show, Eq)

type SParser o = Parser String o

space :: SParser Char
space = satisfy (`elem` " \t")

spaces :: SParser String
spaces = many space

number :: SParser Int
number = read <$> some (satisfy (`elem` ['0' .. '9']))

identifier :: SParser String
identifier = do
  c <- satisfy (`elem` ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_'])
  cs <- many (satisfy (`elem` ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['_']))
  return (c : cs)

stringLiteral :: SParser String
stringLiteral =
  char '"'
    *> many (satisfy (/= '"'))
    <* char '"'

arithmeticOp :: SParser ArithmeticOp
arithmeticOp =
  (string "+" $> Add)
    <|> (string "-" $> Subtract)
    <|> (string "*" $> Multiply)
    <|> (string "/" $> Divide)

logicalOp :: SParser LogicalOp
logicalOp =
  (string "=" $> Equal)
    <|> (string "<=" $> LessThanOrEqual)
    <|> (string "<" $> LessThan)
    <|> (string ">=" $> GreaterThanOrEqual)
    <|> (string ">" $> GreaterThan)
    <|> (string "<>" $> NotEqual)

-- Ensure that keywords are separated by spaces. Otherwise the parser will split valid identifiers like `let1` into `let` and `1`.
keyword :: SParser Keyword
keyword =
  (string "LET" $> Let)
    <|> (string "IF" $> If)
    <|> (string "THEN" $> Then)
    <|> (string "PRINT" $> Print)
    <|> (string "END" $> End)
    <|> (string "REM" $> Remark)
    <|> (string "INPUT" $> Input)
    <|> (string "FOR" $> For)
    <|> (string "TO" $> To)
    <|> (string "NEXT" $> Next)
    <|> (string "CLEAR" $> Clear)
    <|> (string "GOTO" $> Goto)
    <|> (string "GOSUB" $> Gosub)
    <|> (string "WAIT" $> Wait)
    <|> (string "PAUSE" $> Pause)

punctuation :: SParser Punctuation
punctuation =
  (char ',' $> Comma)
    <|> (char ':' $> Colon)
    <|> (char ';' $> SemiColon)
    <|> (char '(' $> LeftParen)
    <|> (char ')' $> RightParen)
    <|> (char '$' $> Dollar)
    <|> (char '\n' $> NewLine)

token :: SParser Token
token =
  spaces
    *> ( Number <$> number
           <|> StringLiteral <$> stringLiteral
           <|> Keyword <$> keyword
           <|> Identifier <$> identifier
           <|> LogicalOp <$> logicalOp
           <|> ArithmeticOp <$> arithmeticOp
           <|> Punctuation <$> punctuation
       )

tokens :: SParser [Token]
tokens = many token