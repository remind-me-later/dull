module Hir.Translate where

import Ast.Types
import Control.Monad.State
import Data.Map qualified
import Hir.Types
import SymbolTable
import TypeSystem (BasicType)

newtype TranslationState = TranslationState
  { labelToInt :: Data.Map.Map GotoTarget Int
  }

lookupLabelInState :: GotoTarget -> TranslationState -> Maybe Int
lookupLabelInState label (TranslationState labelMap) =
  Data.Map.lookup label labelMap

symbolTableUsedLabelsToInt :: SymbolTable -> Data.Map.Map GotoTarget Int
symbolTableUsedLabelsToInt symbolTable =
  Data.Map.fromList
    [ (label, idx)
    | (idx, label) <- zip [0 ..] (Data.Map.keys $ usedLabels symbolTable)
    ]

translateLine :: Line BasicType -> State TranslationState [HirStmt]
translateLine Line {lineNumber, lineLabel, lineStmts} = do
  label <- case lineLabel of
    Just lbl -> do
      labelIdx <- gets (lookupLabelInState (GoToLabel lbl))
      case labelIdx of
        Just idx -> return [HirLabel idx]
        Nothing -> return []
    Nothing -> return []
  number <- do
    lineNumberIdx <- gets (lookupLabelInState (GoToLine lineNumber))
    case lineNumberIdx of
      Just idx -> return [HirLabel idx]
      Nothing -> return []
  let stmts = map HirStmt lineStmts
  return $ number ++ label ++ stmts

translateProgram :: Program BasicType -> SymbolTable -> HirProgram
translateProgram (Program programLines) symbolTable =
  let programLines' = Data.Map.elems programLines
      initialState = TranslationState $ symbolTableUsedLabelsToInt symbolTable
      translatedStmts = evalState (mapM translateLine programLines') initialState
      falttenedStmts = concat translatedStmts
   in HirProgram falttenedStmts