module TypeSystem
  ( ExprType (..),
  )
where

data ExprType where
  ExprStringType :: ExprType
  ExprNumericType :: ExprType
  ExprUnknownType :: ExprType
  ExprArrType ::
    { exprArrType :: ExprType,
      exprArrSize :: Int,
      exprArrLength :: Int -- Only valid for strings, for numeric arrays this is always 1
    } ->
    ExprType
  deriving (Show, Eq)
