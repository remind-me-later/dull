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
import Data.Map
import TypeSystem

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
    { symbols :: Map Ident Symbol,
      nextStackOffset :: Int
    } ->
    SymbolTable
  deriving (Show, Eq)

emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable {symbols = empty, nextStackOffset = 0}

sizeOfTy :: ExprType -> Int
sizeOfTy ExprStringType = 1
sizeOfTy ExprNumericType = 1
sizeOfTy (ExprArrType {exprArrSize, exprArrLength}) = exprArrSize * exprArrLength
sizeOfTy ExprUnknownType = 0

insertSymbol :: Ident -> ExprType -> SymbolTable -> SymbolTable
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