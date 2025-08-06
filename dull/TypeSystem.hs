module TypeSystem (BasicType (..)) where

data BasicType where
  BasicStringType :: BasicType
  BasicNumericType :: BasicType
  -- Array sizes are not known at compile time
  BasicNumArrType :: BasicType
  BasicStrArrType :: BasicType
  BasicNum2DArrType :: BasicType
  BasicStr2DArrType :: BasicType
  deriving (Show, Eq)
