module Token
  ( Token (..),
    Operation (..),
    StmtKeyword (..),
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
  Or :: Operation
  And :: Operation
  Caret :: Operation
  deriving (Eq)

instance Show Operation where
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide = "/"
  show Equal = "="
  show LessThanOrEqual = "<="
  show GreaterThanOrEqual = ">="
  show NotEqual = "<>"
  show LessThan = "<"
  show GreaterThan = ">"
  show Or = "OR"
  show And = "AND"
  show Caret = "^"

data Punctuation where
  Comma :: Punctuation
  Colon :: Punctuation
  SemiColon :: Punctuation
  LeftParen :: Punctuation
  RightParen :: Punctuation
  NewLine :: Punctuation
  Dollar :: Punctuation
  Dot :: Punctuation
  Hashtag :: Punctuation
  deriving (Eq)

instance Show Punctuation where
  show Comma = ","
  show Colon = ":"
  show SemiColon = ";"
  show LeftParen = "("
  show RightParen = ")"
  show NewLine = "\n"
  show Dollar = "$"
  show Dot = "."
  show Hashtag = "#"

data StmtKeyword where
  -- Statements
  Let :: StmtKeyword
  If :: StmtKeyword
  Then :: StmtKeyword
  Input :: StmtKeyword
  Print :: StmtKeyword
  End :: StmtKeyword
  Remark :: StmtKeyword
  For :: StmtKeyword
  To :: StmtKeyword
  Next :: StmtKeyword
  Clear :: StmtKeyword
  Goto :: StmtKeyword
  Gosub :: StmtKeyword
  Wait :: StmtKeyword
  Pause :: StmtKeyword
  Cls :: StmtKeyword
  Random :: StmtKeyword
  Gprint :: StmtKeyword
  GCursor :: StmtKeyword
  Cursor :: StmtKeyword
  Beep :: StmtKeyword
  Using :: StmtKeyword
  Return :: StmtKeyword
  Poke :: StmtKeyword
  Dim :: StmtKeyword
  Read :: StmtKeyword
  Data :: StmtKeyword
  Restore :: StmtKeyword
  deriving (Eq)

instance Show StmtKeyword where
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
  show Cursor = "CURSOR"
  show GCursor = "GCURSOR"
  show Beep = "BEEP"
  show Using = "USING"
  show Return = "RETURN"
  show Poke = "POKE"
  show Dim = "DIM"
  show Read = "READ"
  show Data = "DATA"
  show Restore = "RESTORE"

data Ty where
  NumType :: Ty
  StrType :: Ty
  deriving (Show, Eq)

data Id where
  Id :: String -> Ty -> Id
  deriving (Show, Eq)

data Token where
  Identifier :: Id -> Token
  Number :: Double -> Token
  Operation :: Operation -> Token
  Keyword :: StmtKeyword -> Token
  Punctuation :: Punctuation -> Token
  StringLiteral :: String -> Token
  deriving (Show, Eq)

type SParser o = Parser String o

space :: SParser Char
space = satisfy (`elem` " \t")

spaces :: SParser String
spaces = many space

-- Can be an integer, for example 123, or a float, for example 123.45
number :: SParser Double
number = do
  wholePart <- some (satisfy (`elem` ['0' .. '9']))
  decimalPart <- optional (char '.' *> some (satisfy (`elem` ['0' .. '9'])))
  let numberStr = wholePart ++ maybe "" ('.' :) decimalPart
  return (read numberStr)

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
    <|> (string "<>" $> NotEqual)
    <|> (string "<" $> LessThan)
    <|> (string ">=" $> GreaterThanOrEqual)
    <|> (string ">" $> GreaterThan)
    <|> (string "OR" $> Or)
    <|> (string "AND" $> And)
    <|> (string "^" $> Caret)

-- Ensure that keywords are separated by spaces. Otherwise the parser will split valid identifiers like `let1` into `let` and `1`.
keyword :: SParser StmtKeyword
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
    <|> (string "GCURSOR" $> GCursor)
    <|> (string "BEEP" $> Beep)
    <|> (string "CURSOR" $> Cursor)
    <|> (string "USING" $> Using)
    <|> (string "RETURN" $> Return)
    <|> (string "POKE" $> Poke)
    <|> (string "DIM" $> Dim)
    <|> (string "READ" $> Read)
    <|> (string "DATA" $> Data)
    <|> (string "RESTORE" $> Restore)

punctuation :: SParser Punctuation
punctuation =
  (char ',' $> Comma)
    <|> (char '.' $> Dot)
    <|> (char ':' $> Colon)
    <|> (char ';' $> SemiColon)
    <|> (char '(' $> LeftParen)
    <|> (char ')' $> RightParen)
    <|> (char '$' $> Dollar)
    <|> (char '\n' $> NewLine)
    <|> (char '#' $> Hashtag)

token :: SParser Token
token =
  spaces
    *> ( Number <$> number
           <|> StringLiteral <$> stringLiteral
           <|> Keyword <$> keyword
           <|> Operation <$> operation
           <|> Punctuation <$> punctuation
           <|> Identifier <$> identifier
       )

tokens :: SParser [Token]
tokens = many token