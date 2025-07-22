module SymbolTable
  ( Symbol (..),
    SymbolTable (..),
    emptySymbolTable,
    insertSymbol,
    lookupSymbol,
    sizeOfTy,
  )
where

import Ast.Types (Ident (..))
import Data.List (intercalate, sortBy)
import Data.Map
import TypeSystem

data Symbol where
  Symbol ::
    { symbolName :: String,
      symbolStackOffset :: Int,
      exprType :: BasicType
    } ->
    Symbol
  deriving (Eq)

instance Show Symbol where
  show Symbol {symbolName, symbolStackOffset, exprType} =
    symbolName ++ case exprType of
      BasicStringType -> "$ : " ++ show symbolStackOffset
      BasicNumericType -> " : " ++ show symbolStackOffset
      BasicNumArrType {numericArrSize} ->
        "(" ++ show numericArrSize ++ ") : " ++ show symbolStackOffset
      BasicStrArrType {strArrSize, strArrLength} ->
        "$("
          ++ show strArrSize
          ++ ")*"
          ++ show strArrLength
          ++ " : "
          ++ show symbolStackOffset

data SymbolTable where
  SymbolTable ::
    { symbols :: Map Ident Symbol,
      nextStackOffset :: Int
    } ->
    SymbolTable
  deriving (Eq)

instance Show SymbolTable where
  show SymbolTable {symbols} =
    "{"
      ++ intercalate ", " (show <$> orderedSymbols)
      ++ "}"
    where
      orderedSymbols =
        sortBy
          (\s1 s2 -> compare (symbolStackOffset s1) (symbolStackOffset s2))
          (elems symbols)

emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable {symbols = empty, nextStackOffset = 0}

sizeOfTy :: BasicType -> Int
sizeOfTy BasicStringType = 1
sizeOfTy BasicNumericType = 1
sizeOfTy (BasicNumArrType {numericArrSize}) = numericArrSize + 1
sizeOfTy (BasicStrArrType {strArrSize, strArrLength}) = (strArrSize + 1) * strArrLength

insertSymbol :: Ident -> BasicType -> SymbolTable -> SymbolTable
insertSymbol sym ty st@SymbolTable {symbols, nextStackOffset} =
  let alreadyInSymbols = Data.Map.member sym symbols
      newSymbolName = show sym
      newSymbol = Symbol {symbolName = newSymbolName, symbolStackOffset = nextStackOffset, exprType = ty}
      newSymbols = insert sym newSymbol symbols
      newOffset = nextStackOffset + sizeOfTy ty
   in if alreadyInSymbols
        then st
        else SymbolTable {symbols = newSymbols, nextStackOffset = newOffset}

lookupSymbol :: Ident -> SymbolTable -> Maybe Symbol
lookupSymbol name SymbolTable {symbols} =
  Data.Map.lookup name symbols