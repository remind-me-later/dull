module Hir.Translate where

import Ast.Types
import Control.Monad.State
import Data.Map qualified
import Hir.Types
import SymbolTable
import TypeSystem (BasicType (..))

data TranslationState = TranslationState
  { labelToInt :: Data.Map.Map GotoTarget Int,
    nextLabelIdx :: Int,
    forStartToLabel :: Data.Map.Map NumIdent (Int, Expr BasicType)
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

beginForLoop :: NumIdent -> Expr BasicType -> TranslationState -> (TranslationState, Int)
beginForLoop ident cond state' =
  let nextIdx = nextLabelIdx state'
      newForStartMap = Data.Map.insert ident (nextIdx, cond) (forStartToLabel state')
      newState = state' {forStartToLabel = newForStartMap, nextLabelIdx = nextIdx + 1}
   in (newState, nextIdx)

lookupForStartLabel :: NumIdent -> TranslationState -> (Int, Expr BasicType)
lookupForStartLabel ident state' =
  case Data.Map.lookup ident (forStartToLabel state') of
    Just idx -> idx
    Nothing -> error $ "For loop start label not found for: " ++ show ident

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
  ForStmt {forAssignment, forToExpr} -> do
    let forIdent = case assignmentLValue forAssignment of
          LValueIdent (IdentNumIdent ident) -> ident
          _ -> error "For loop assignment must be an identifier"
    -- Begin the for loop by inserting a label and storing the start condition
    (newState, labelIdx) <- gets (beginForLoop forIdent forToExpr)
    put newState

    return [HirLabel labelIdx, HirAssign forAssignment]
  NextStmt {nextIdent} -> do
    (startLabelIdx, startCond) <- gets (lookupForStartLabel nextIdent)
    let identExpr =
          Expr
            { exprInner =
                LValueExpr (LValueIdent (IdentNumIdent nextIdent)),
              exprType = BasicNumericType
            }
        nextAssignment =
          Assignment
            { assignmentLValue = LValueIdent (IdentNumIdent nextIdent),
              assignmentExpr =
                Expr
                  { exprInner =
                      BinExpr
                        identExpr
                        AddOp
                        (Expr {exprInner = NumLitExpr 1, exprType = BasicNumericType}),
                    exprType = BasicNumericType
                  },
              assignmentType = BasicNumericType
            }
        newJumpCondition =
          Expr
            { exprInner = BinExpr identExpr LessThanOrEqualOp startCond,
              exprType = BasicNumericType
            }
    -- Create a conditional goto to the start of the for loop
    return [HirAssign nextAssignment, HirCondGoto newJumpCondition startLabelIdx]
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
  GprintStmt {gprintExprs, gprintEnding} -> do
    let hirGPrints =
          map
            ( \expr ->
                HirGPrint
                  { hirGPrintExpr = expr,
                    hirGPrintEnding = PrintEndingNoNewLine
                  }
            )
            (init gprintExprs)
            ++ [ HirGPrint
                   { hirGPrintExpr = last gprintExprs,
                     hirGPrintEnding = gprintEnding
                   }
               ]
    return hirGPrints
  Comment -> return []
  ReturnStmt -> return [HirReturn]
  DimStmt _ -> return [] -- Already in symbol table
  EndStmt -> return [HirStmt EndStmt]
  ReadStmt _ -> error "Unimplemented: ReadStmt"
  DataStmt _ -> error "Unimplemented: DataStmt"
  RestoreStmt _ -> error "Unimplemented: RestoreStmt"
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
      initialState = TranslationState {labelToInt = labelMap, nextLabelIdx = nextIdx, forStartToLabel = Data.Map.empty}
      -- Translate each line of the program
      translatedStmts = evalState (mapM translateLine programLines') initialState
      falttenedStmts = concat translatedStmts
   in HirProgram falttenedStmts