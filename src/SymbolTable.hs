module SymbolTable
  ( Variable (..),
    SymbolTable (..),
    emptySymbolTable,
    insertSymbol,
    lookupSymbol,
    sizeOfTy,
    insertStringLiteral,
  )
where

import Ast.Types (Ident (..))
import Data.List (intercalate, sortBy)
import Data.Map
import TypeSystem

data Variable where
  Variable ::
    { variableName :: String,
      variableOffset :: Int,
      variableType :: BasicType
    } ->
    Variable
  deriving (Eq)

instance Show Variable where
  show Variable {variableName, variableOffset, variableType} =
    variableName ++ case variableType of
      BasicStringType -> "$ : " ++ show variableOffset
      BasicNumericType -> " : " ++ show variableOffset
      BasicNumArrType {numericArrSize} ->
        "(" ++ show numericArrSize ++ ") : " ++ show variableOffset
      BasicStrArrType {strArrSize, strArrLength} ->
        "$("
          ++ show strArrSize
          ++ ")*"
          ++ show strArrLength
          ++ " : "
          ++ show variableOffset

data SymbolTable where
  SymbolTable ::
    { symbolMap :: Map Ident Variable,
      stringLiteralMap :: Map String Int,
      nextOffset :: Int
    } ->
    SymbolTable
  deriving (Eq)

instance Show SymbolTable where
  show SymbolTable {symbolMap, stringLiteralMap} =
    "vars: {"
      ++ intercalate ", " (show <$> orderedSymbols)
      ++ "}\n"
      ++ "strings: {"
      ++ intercalate ", " (show <$> stringLits)
      ++ "}\n"
    where
      orderedSymbols =
        sortBy
          (\s1 s2 -> compare (variableOffset s1) (variableOffset s2))
          (elems symbolMap)
      stringLits =
        sortBy
          (\s1 s2 -> compare (snd s1) (snd s2))
          (toList stringLiteralMap)

emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable {symbolMap = empty, nextOffset = 0, stringLiteralMap = empty}

insertSymbol :: Ident -> BasicType -> SymbolTable -> SymbolTable
insertSymbol sym ty st@SymbolTable {symbolMap, nextOffset} =
  let alreadyInSymbols = Data.Map.member sym symbolMap
      newSymbolName = show sym
      newSymbol = Variable {variableName = newSymbolName, variableOffset = nextOffset, variableType = ty}
      newSymbols = insert sym newSymbol symbolMap
      newOffset = nextOffset + sizeOfTy ty
   in if alreadyInSymbols
        then st
        else st {symbolMap = newSymbols, nextOffset = newOffset}

insertStringLiteral :: String -> SymbolTable -> SymbolTable
insertStringLiteral str st@SymbolTable {stringLiteralMap, nextOffset} =
  let alreadyInLiterals = Data.Map.member str stringLiteralMap
      newOffset = nextOffset + length str
   in if alreadyInLiterals
        then st
        else st {stringLiteralMap = insert str nextOffset stringLiteralMap, nextOffset = newOffset}

lookupSymbol :: Ident -> SymbolTable -> Maybe Variable
lookupSymbol name SymbolTable {symbolMap} =
  Data.Map.lookup name symbolMap