module Hir.Translate where

import Ast.Types
import Control.Monad.State
import Data.Map qualified
import Hir.Types
import SymbolTable
import TypeSystem (BasicType (..))

data TranslationState = TranslationState
  { labelToInt :: Data.Map.Map GotoTarget Int,
    nextLabelIdx :: Int
  }

lookupLabelInState :: GotoTarget -> TranslationState -> Maybe Int
lookupLabelInState label (TranslationState {labelToInt}) =
  Data.Map.lookup label labelToInt

insertGotoLabelInState :: TranslationState -> (TranslationState, Int)
insertGotoLabelInState state' =
  let nextIdx = nextLabelIdx state'
      newLabelMap = Data.Map.insert (GoToLine nextIdx) nextIdx (labelToInt state')
      newState = state' {labelToInt = newLabelMap, nextLabelIdx = nextIdx + 1}
   in (newState, nextIdx)

symbolTableUsedLabelsToInt :: SymbolTable -> (Data.Map.Map GotoTarget Int, Int)
symbolTableUsedLabelsToInt symbolTable =
  let labels = Data.Map.keys $ usedLabels symbolTable
      labelMap = Data.Map.fromList $ zip labels [0 ..]
      nextIdx = length labels
   in (labelMap, nextIdx)

lookupLabelPanics :: GotoTarget -> TranslationState -> Int
lookupLabelPanics label state' =
  case lookupLabelInState label state' of
    Just idx -> idx
    Nothing -> error $ "Label not found: " ++ show label

translateStmt :: Stmt BasicType -> State TranslationState [HirStmt]
translateStmt stmt = case stmt of
  GoToStmt target -> do
    labelIdx <- gets (lookupLabelPanics target)
    return [HirGoto labelIdx]
  GoSubStmt target -> do
    labelIdx <- gets (lookupLabelPanics target)
    return [HirGosub labelIdx]
  IfThenStmt condition stmt' ->
    case stmt' of
      GoToStmt target -> do
        labelIdx <- gets (lookupLabelPanics target)
        return [HirCondGoto condition labelIdx]
      GoSubStmt target -> do
        labelIdx <- gets (lookupLabelPanics target)
        return [HirCondGosub condition labelIdx]
      s ->
        -- Reverse the condition of the if and transform it into a conditional goto
        let newCondition =
              Expr
                { exprInner = UnaryExpr UnaryNotOp condition,
                  exprType = BasicNumericType
                }
         in -- we have to create a new label for the if statement
            do
              (newState, labelIdx) <- gets insertGotoLabelInState
              put newState
              return [HirCondGoto newCondition labelIdx, HirStmt s, HirLabel labelIdx]
  LetStmt {letAssignments} -> do
    let assignments = map HirAssign letAssignments
    return assignments
  PrintStmt {printKind, printExprs, printEnding, printUsingClause} -> do
    let -- All print expressions have line ending with no newline, except the last one
        -- which has the specified ending.
        hirPrints =
          map
            ( \expr ->
                HirPrint
                  { hirPrintKind = printKind,
                    hirPrintExpression = expr,
                    hirPrintEnding = PrintEndingNoNewLine
                  }
            )
            (init printExprs)
            ++ [ HirPrint
                   { hirPrintKind = printKind,
                     hirPrintExpression = last printExprs,
                     hirPrintEnding = printEnding
                   }
               ]
    let usingClause = case printUsingClause of
          Just u -> [HirUsing u]
          Nothing -> []
    return $ usingClause ++ hirPrints
  UsingStmt usingClause -> return [HirUsing usingClause]
  InputStmt {inputPrintExpr, inputDestination} -> do
    let inputStmt = HirInput {hirInputDestination = inputDestination}
    case inputPrintExpr of
      Just expr ->
        return
          [ HirPrint
              { hirPrintKind = PrintKindPrint,
                hirPrintExpression =
                  Expr
                    { exprInner = StrLitExpr expr,
                      exprType = BasicStringType
                    },
                hirPrintEnding = PrintEndingNoNewLine
              },
            inputStmt
          ]
      Nothing -> return [inputStmt]
  _ -> return [HirStmt stmt]

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
  stmts <- mapM translateStmt lineStmts
  return $ number ++ label ++ concat stmts

translateProgram :: Program BasicType -> SymbolTable -> HirProgram
translateProgram (Program programLines) symbolTable =
  let programLines' = Data.Map.elems programLines
      (labelMap, nextIdx) = symbolTableUsedLabelsToInt symbolTable
      initialState = TranslationState {labelToInt = labelMap, nextLabelIdx = nextIdx}
      -- Translate each line of the program
      translatedStmts = evalState (mapM translateLine programLines') initialState
      falttenedStmts = concat translatedStmts
   in HirProgram falttenedStmts