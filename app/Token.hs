module Token
  ( Token (..),
    Operator (..),
    Keyword (..),
    Punctuation (..),
    tokens,
  )
where

import Control.Applicative (Alternative (many, some, (<|>)))
import Data.Functor (($>))
import ParserCombinators (Parser, char, satisfy, string)

data Operator
  = Add
  | Subtract
  | Multiply
  | Equal
  | Range
  | LessThan
  | GreaterThan
  | LessThanOrEqual
  | GreaterThanOrEqual
  | NotEqual
  deriving (Show, Eq)

data Punctuation
  = Comma
  | Colon
  | SemiColon
  | LeftParen
  | RightParen
  | Newline
  | Dollar
  deriving (Show, Eq)

data Keyword
  = Let
  | If
  | Then
  | Input
  | Print
  | End
  | Remark
  | For
  | To
  | Next
  deriving (Show, Eq)

data Token
  = Identifier String
  | Number Int
  | Operator Operator
  | Keyword Keyword
  | Punctuation Punctuation
  | StringLiteral String
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

-- Be careful with the order of the parsers, because the parser for `==` is a prefix of the parser for `=`.
operator :: SParser Operator
operator =
  (string "+" $> Add)
    <|> (string "-" $> Subtract)
    <|> (string "*" $> Multiply)
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

punctuation :: SParser Punctuation
punctuation =
  (char ',' $> Comma)
    <|> (char ':' $> Colon)
    <|> (char ';' $> SemiColon)
    <|> (char '(' $> LeftParen)
    <|> (char ')' $> RightParen)
    <|> (char '$' $> Dollar)
    <|> (char '\n' $> Newline)

token :: SParser Token
token =
  spaces
    *> ( Number <$> number
           <|> StringLiteral <$> stringLiteral
           <|> Keyword <$> keyword
           <|> Identifier <$> identifier
           <|> Operator <$> operator
           <|> Punctuation <$> punctuation
       )

tokens :: SParser [Token]
tokens = many token