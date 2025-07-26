module SymbolTable
  ( Variable (..),
    SymbolTable (..),
    emptySymbolTable,
    insertVariable,
    lookupSymbol,
    sizeOfTy,
    insertStringLiteral,
    insertUsedLabel,
    insertFakeVariable,
  )
where

import Ast.Types (GotoTarget, Ident (..), getIdentName)
import Data.List (intercalate, sortBy)
import Data.Map
import Data.Word (Word16)
import TypeSystem

data Variable where
  Variable ::
    { variableName :: String,
      variableOffset :: Word16,
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
      fakeSymbolMap :: Map String Variable,
      stringLiteralMap :: Map String Word16,
      nextOffsetVar :: Word16,
      nextOffsetString :: Word16,
      usedLabels :: Map GotoTarget GotoTargetData
    } ->
    SymbolTable
  deriving (Eq)

instance Show SymbolTable where
  show SymbolTable {symbolMap, stringLiteralMap, usedLabels, fakeSymbolMap} =
    "vars: {"
      ++ intercalate ", " (show <$> orderedSymbols)
      ++ "}\n"
      ++ "strings: {"
      ++ intercalate ", " (show <$> stringLits)
      ++ "}\n"
      ++ "used labels: {"
      ++ intercalate ", " (show <$> usedLabels')
      ++ "}\n"
      ++ "fake vars: {"
      ++ intercalate ", " (show <$> fakeSymbols')
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
      usedLabels' =
        sortBy
          (\(l1, _) (l2, _) -> compare l1 l2)
          (toList usedLabels)
      fakeSymbols' =
        sortBy
          (\s1 s2 -> compare (variableOffset s1) (variableOffset s2))
          (elems fakeSymbolMap)

emptySymbolTable :: SymbolTable
emptySymbolTable =
  SymbolTable
    { symbolMap = empty,
      nextOffsetVar = 0,
      nextOffsetString = 0,
      stringLiteralMap = empty,
      usedLabels = mempty,
      fakeSymbolMap = empty
    }

insertVariable :: Ident -> BasicType -> SymbolTable -> SymbolTable
insertVariable sym ty st@SymbolTable {symbolMap, nextOffsetVar} =
  let alreadyInSymbols = Data.Map.member sym symbolMap
      newSymbolName = getIdentName sym
      newSymbol = Variable {variableName = [newSymbolName], variableOffset = nextOffsetVar, variableType = ty}
      newSymbols = insert sym newSymbol symbolMap
      newOffset = nextOffsetVar + fromIntegral (sizeOfTy ty)
   in if alreadyInSymbols
        then st
        else st {symbolMap = newSymbols, nextOffsetVar = newOffset}

insertFakeVariable :: String -> BasicType -> SymbolTable -> SymbolTable
insertFakeVariable name ty st@SymbolTable {fakeSymbolMap, nextOffsetVar} =
  let alreadyInFakeSymbols = Data.Map.member name fakeSymbolMap
      newFakeSymbol = Variable {variableName = name, variableOffset = nextOffsetVar, variableType = ty}
      newFakeSymbols = insert name newFakeSymbol fakeSymbolMap
      newOffset = nextOffsetVar + fromIntegral (sizeOfTy ty)
   in if alreadyInFakeSymbols
        then st
        else st {fakeSymbolMap = newFakeSymbols, nextOffsetVar = newOffset}

insertStringLiteral :: String -> SymbolTable -> SymbolTable
insertStringLiteral str st@SymbolTable {stringLiteralMap, nextOffsetString} =
  let alreadyInLiterals = Data.Map.member str stringLiteralMap
      newOffset = nextOffsetString + fromIntegral (length str + stringHeaderSize)
   in if alreadyInLiterals
        then st
        else st {stringLiteralMap = insert str nextOffsetString stringLiteralMap, nextOffsetString = newOffset}

insertUsedLabel :: GotoTarget -> Bool -> SymbolTable -> SymbolTable
insertUsedLabel label isFunctionCall st@SymbolTable {usedLabels} =
  let alreadyInUsedLabels = Data.Map.member label usedLabels
      newUsedLabels =
        if alreadyInUsedLabels
          then usedLabels
          else insert label (if isFunctionCall then IsFunctionCall else IsBranch) usedLabels
   in st {usedLabels = newUsedLabels}

lookupSymbol :: Ident -> SymbolTable -> Maybe Variable
lookupSymbol name SymbolTable {symbolMap} =
  Data.Map.lookup name symbolMap