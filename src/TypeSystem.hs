module TypeSystem where

data Ty where
  Num :: Ty
  Str :: Ty
  deriving (Eq, Show)
