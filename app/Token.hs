module Token
  ( Token (..),
    Operation (..),
    Keyword (..),
    Punctuation (..),
    tokens,
    Ty (..),
    Id (..),
  )
where

import Control.Applicative (Alternative (many, some, (<|>)), optional)
import Data.Functor (($>))
import ParserCombinators (Parser, char, satisfy, string)

data Operation where
  Add :: Operation
  Subtract :: Operation
  Multiply :: Operation
  Divide :: Operation
  Equal :: Operation
  LessThan :: Operation
  GreaterThan :: Operation
  LessThanOrEqual :: Operation
  GreaterThanOrEqual :: Operation
  NotEqual :: Operation
  deriving (Eq)

instance Show Operation where
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide = "/"
  show Equal = "="
  show LessThan = "<"
  show GreaterThan = ">"
  show LessThanOrEqual = "<="
  show GreaterThanOrEqual = ">="
  show NotEqual = "<>"

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
  -- Statements
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
  Cls :: Keyword
  Random :: Keyword
  Gprint :: Keyword
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
  show Cls = "CLS"
  show Random = "RANDOM"
  show Gprint = "GPRINT"

data Ty where
  NumType :: Ty
  StrType :: Ty
  deriving (Show, Eq)

data Id where
  Id :: String -> Ty -> Id
  deriving (Show, Eq)

data Token where
  Identifier :: Id -> Token
  Number :: Int -> Token
  Operation :: Operation -> Token
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

identifier :: SParser Id
identifier = do
  cs <- some (satisfy (`elem` ['a' .. 'z'] ++ ['A' .. 'Z']))
  d <- optional (char '$')
  return
    ( Id
        cs
        ( case d of
            Just _ -> StrType
            Nothing -> NumType
        )
    )

stringLiteral :: SParser String
stringLiteral =
  char '"'
    *> many (satisfy (/= '"'))
    <* char '"'

operation :: SParser Operation
operation =
  (string "+" $> Add)
    <|> (string "-" $> Subtract)
    <|> (string "*" $> Multiply)
    <|> (string "/" $> Divide)
    <|> (string "=" $> Equal)
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
    <|> (string "CLS" $> Cls)
    <|> (string "RANDOM" $> Random)
    <|> (string "GPRINT" $> Gprint)

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
           <|> Operation <$> operation
           <|> Punctuation <$> punctuation
       )

tokens :: SParser [Token]
tokens = many token