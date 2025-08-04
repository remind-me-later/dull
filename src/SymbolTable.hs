module SymbolTable
  ( Variable (..),
    SymbolTable (..),
    emptySymbolTable,
    insertVariable,
    lookupSymbol,
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
      BasicNumArrType {numericArrSize} ->
        "(" ++ show numericArrSize ++ ")"
      BasicStrArrType {strArrSize, strArrLength} ->
        "$("
          ++ show strArrSize
          ++ ")*"
          ++ show strArrLength
      BasicNum2DArrType {num2DArrRows, num2DArrCols} ->
        "(" ++ show num2DArrRows ++ "," ++ show num2DArrCols ++ ")"
      BasicStr2DArrType {str2DArrRows, str2DArrCols, str2DArrLength} ->
        "$("
          ++ show str2DArrRows
          ++ ","
          ++ show str2DArrCols
          ++ ")*"
          ++ show str2DArrLength

data GotoTargetData where
  IsBranch :: GotoTargetData
  IsFunctionCall :: GotoTargetData
  deriving (Eq, Ord)

instance Show GotoTargetData where
  show IsBranch = "GOTO"
  show IsFunctionCall = "GOSUB"

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

lookupSymbol :: Ident -> SymbolTable -> Maybe Variable
lookupSymbol name SymbolTable {symbolMap} =
  Data.Map.lookup name symbolMap
