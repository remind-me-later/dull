module TypeSystem
  ( BasicType (..),
  )
where

data BasicType where
  BasicStringType :: BasicType
  BasicNumericType :: BasicType
  BasicUnknownType :: BasicType
  BasicArrType ::
    { exprArrType :: BasicType,
      exprArrSize :: Int,
      exprArrLength :: Int -- Only valid for strings, for numeric arrays this is always 1
    } ->
    BasicType
  deriving (Show, Eq)
