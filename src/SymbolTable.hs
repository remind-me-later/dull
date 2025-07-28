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
    lookupFakeSymbol,
    insertNumberLiteral,
  )
where

import Ast.Types (GotoTarget, Ident (..), getIdentName)
import Data.Char (toUpper)
import Data.List (intercalate, sortBy)
import Data.Map
import Data.Word (Word16)
import Numeric (showHex)
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
      BasicStringType -> "$ : 0x" ++ showHexAllCaps variableOffset
      BasicNumericType -> " : 0x" ++ showHexAllCaps variableOffset
      BasicNumArrType {numericArrSize} ->
        "(" ++ show numericArrSize ++ ") : 0x" ++ showHexAllCaps variableOffset
      BasicStrArrType {strArrSize, strArrLength} ->
        "$("
          ++ show strArrSize
          ++ ")*"
          ++ show strArrLength
          ++ " : 0x"
          ++ showHexAllCaps variableOffset
    where
      showHexAllCaps w = Prelude.map toUpper (showHex w "")

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
      numberLiteralMap :: Map Double Word16,
      nextOffset :: Word16,
      usedLabels :: Map GotoTarget GotoTargetData,
      startAddress :: Word16
    } ->
    SymbolTable
  deriving (Eq)

instance Show SymbolTable where
  show SymbolTable {symbolMap, stringLiteralMap, usedLabels, fakeSymbolMap, numberLiteralMap} =
    "vars: {"
      ++ intercalate ", " (show <$> orderedSymbols)
      ++ "}\n"
      ++ "strings: {"
      ++ intercalate ", " (showStringLit <$> stringLits)
      ++ "}\n"
      ++ "number literals: {"
      ++ intercalate ", " (showStringLit <$> numLits)
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
      numLits =
        sortBy
          (\(n1, _) (n2, _) -> compare n1 n2)
          (toList numberLiteralMap)
      fakeSymbols' =
        sortBy
          (\s1 s2 -> compare (variableOffset s1) (variableOffset s2))
          (elems fakeSymbolMap)
      showStringLit (str, offset) =
        "("
          ++ show str
          ++ ", 0x"
          ++ Prelude.map toUpper (showHex offset "")
          ++ ")"

emptySymbolTable :: Word16 -> SymbolTable
emptySymbolTable startAddress =
  SymbolTable
    { symbolMap = empty,
      nextOffset = startAddress,
      stringLiteralMap = empty,
      usedLabels = mempty,
      fakeSymbolMap = empty,
      startAddress = startAddress,
      numberLiteralMap = empty
    }

insertVariable :: Ident -> BasicType -> SymbolTable -> SymbolTable
insertVariable sym ty st@SymbolTable {symbolMap, nextOffset} =
  let alreadyInSymbols = Data.Map.member sym symbolMap
      newSymbolName = getIdentName sym
      newSymbol = Variable {variableName = [newSymbolName], variableOffset = nextOffset, variableType = ty}
      newSymbols = insert sym newSymbol symbolMap
      newOffset = nextOffset + fromIntegral (sizeOfTy ty)
   in if alreadyInSymbols
        then st
        else st {symbolMap = newSymbols, nextOffset = newOffset}

insertFakeVariable :: String -> BasicType -> SymbolTable -> SymbolTable
insertFakeVariable name ty st@SymbolTable {fakeSymbolMap, nextOffset} =
  let alreadyInFakeSymbols = Data.Map.member name fakeSymbolMap
      newFakeSymbol = Variable {variableName = name, variableOffset = nextOffset, variableType = ty}
      newFakeSymbols = insert name newFakeSymbol fakeSymbolMap
      newOffset = nextOffset + fromIntegral (sizeOfTy ty)
   in if alreadyInFakeSymbols
        then st
        else st {fakeSymbolMap = newFakeSymbols, nextOffset = newOffset}

insertStringLiteral :: String -> SymbolTable -> SymbolTable
insertStringLiteral str st@SymbolTable {stringLiteralMap, nextOffset} =
  let alreadyInLiterals = Data.Map.member str stringLiteralMap
      newOffset = nextOffset + fromIntegral (length str + stringHeaderSize)
   in if alreadyInLiterals
        then st
        else st {stringLiteralMap = insert str nextOffset stringLiteralMap, nextOffset = newOffset}

insertNumberLiteral :: Double -> SymbolTable -> SymbolTable
insertNumberLiteral num st@SymbolTable {numberLiteralMap, nextOffset} =
  let alreadyInLiterals = Data.Map.member num numberLiteralMap
      newOffset = nextOffset + fromIntegral (sizeOfTy BasicNumericType)
   in if alreadyInLiterals
        then st
        else st {numberLiteralMap = insert num nextOffset numberLiteralMap, nextOffset = newOffset}

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

lookupFakeSymbol :: String -> SymbolTable -> Maybe Variable
lookupFakeSymbol name SymbolTable {fakeSymbolMap} =
  Data.Map.lookup name fakeSymbolMap