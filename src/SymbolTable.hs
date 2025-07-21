module SymbolTable
  ( ExprType (..),
    Symbol (..),
    SymbolTable (..),
    emptySymbolTable,
    insertSymbol,
    lookupSymbol,
  )
where

import Data.Map

data ExprType where
  ExprStringType :: ExprType
  ExprNumericType :: ExprType
  ExprUnknownType :: ExprType
  ExprArrType ::
    { exprArrType :: ExprType,
      exprArrSize :: Int
    } ->
    ExprType
  deriving (Show, Eq)

data Symbol where
  Symbol ::
    { symbolName :: String,
      symbolStackOffset :: Int,
      exprType :: ExprType
    } ->
    Symbol
  deriving (Show, Eq)

data SymbolTable where
  SymbolTable ::
    { symbols :: Map String Symbol,
      nextStackOffset :: Int
    } ->
    SymbolTable
  deriving (Show, Eq)

emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable {symbols = empty, nextStackOffset = 0}

sizeOfTy :: ExprType -> Int
sizeOfTy ExprStringType = 1
sizeOfTy ExprNumericType = 1
sizeOfTy (ExprArrType {exprArrSize}) = exprArrSize
sizeOfTy ExprUnknownType = 0

insertSymbol :: String -> ExprType -> SymbolTable -> SymbolTable
insertSymbol sym ty st@SymbolTable {symbols, nextStackOffset} =
  let alreadyInSymbols = Data.Map.member sym symbols
      newSymbol = Symbol {symbolName = sym, symbolStackOffset = nextStackOffset, exprType = ty}
      newSymbols = insert sym newSymbol symbols
      newOffset = nextStackOffset + sizeOfTy ty
   in if alreadyInSymbols
        then st
        else SymbolTable {symbols = newSymbols, nextStackOffset = newOffset}

lookupSymbol :: String -> SymbolTable -> Maybe Symbol
lookupSymbol name SymbolTable {symbols} =
  Data.Map.lookup name symbols