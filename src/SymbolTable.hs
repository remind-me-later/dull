module SymbolTable
  ( Variable (..),
    SymbolTable (..),
    emptySymbolTable,
    insertVariable,
    insertLabel,
    lookupLabel,
  )
where

import Ast.Types (Ident (..), identName)
import Data.List (intercalate)
import Data.Map
import Data.Word (Word16)
import TypeSystem

data Variable where
  Variable ::
    { variableName :: String,
      variableType :: BasicType
    } ->
    Variable
  deriving (Eq)

instance Show Variable where
  show Variable {variableName, variableType} =
    variableName ++ case variableType of
      BasicStringType -> "$"
      BasicNumericType -> ""
      BasicNumArrType -> "(?)"
      BasicStrArrType -> "$(?)"
      BasicNum2DArrType -> "(?,?)"
      BasicStr2DArrType -> "$(?,?)"

data SymbolTable where
  SymbolTable ::
    { symbolMap :: Map Ident Variable,
      labelToLineMap :: Map String Word16
    } ->
    SymbolTable
  deriving (Eq)

instance Show SymbolTable where
  show SymbolTable {symbolMap, labelToLineMap} =
    "vars: {"
      ++ intercalate "," (show <$> elems symbolMap)
      ++ "}, labels: {"
      ++ intercalate "," (show <$> toList labelToLineMap)
      ++ "}"

emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable {symbolMap = empty, labelToLineMap = empty}

insertVariable :: Ident -> BasicType -> SymbolTable -> SymbolTable
insertVariable sym ty st@SymbolTable {symbolMap} =
  let alreadyInSymbols = Data.Map.member sym symbolMap
      newSymbolName = identName sym
      newSymbol = Variable {variableName = newSymbolName, variableType = ty}
      newSymbols = insert sym newSymbol symbolMap
   in if alreadyInSymbols
        then st
        else st {symbolMap = newSymbols}

insertLabel :: String -> Word16 -> SymbolTable -> SymbolTable
insertLabel label line st@SymbolTable {labelToLineMap} =
  let alreadyInLabels = Data.Map.member label labelToLineMap
      newLabels = insert label line labelToLineMap
   in if alreadyInLabels
        then st
        else st {labelToLineMap = newLabels}

lookupLabel :: String -> SymbolTable -> Maybe Word16
lookupLabel label SymbolTable {labelToLineMap} =
  Data.Map.lookup label labelToLineMap
