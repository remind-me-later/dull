module TypeSystem
  ( BasicType (..),
  )
where

data BasicType where
  BasicStringType :: BasicType
  BasicNumericType :: BasicType
  BasicNumArrType ::
    { numericArrSize :: Int
    } ->
    BasicType
  BasicStrArrType ::
    { strArrSize :: Int,
      strArrLength :: Int
    } ->
    BasicType
  deriving (Show, Eq)
