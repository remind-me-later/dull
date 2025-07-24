module Hir.Translate where

import Ast.Types
import Control.Monad.State
import Data.Map qualified
import Data.Maybe (fromMaybe)
import Hir.Types
import SymbolTable
import TypeSystem (BasicType (..))

userWaitTimeVarIdent :: Ident
userWaitTimeVarIdent = IdentNumIdent (NumIdent "_USER_WAIT_TIME")

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

translateStringVariableOrLiteral :: StringVariableOrLiteral -> State TranslationState [HirInst]
translateStringVariableOrLiteral (StringVariable var) =
  return [HirPushLValue (LValueIdent (IdentStrIdent var))]
translateStringVariableOrLiteral (StringLiteral str) =
  return [HirPushStrLit str]

translateFunction :: Function BasicType -> State TranslationState [HirInst]
translateFunction function = case function of
  MidFun {midFunStringExpr, midFunStartExpr, midFunLengthExpr} -> do
    stringInsts <- translateStringVariableOrLiteral midFunStringExpr
    startInsts <- translateExpr midFunStartExpr
    lengthInsts <- translateExpr midFunLengthExpr
    return $ stringInsts ++ startInsts ++ lengthInsts ++ [HirIntrinsicFun HirMidFun]
  LeftFun {leftFunStringExpr, leftFunLengthExpr} -> do
    stringInsts <- translateStringVariableOrLiteral leftFunStringExpr
    lengthInsts <- translateExpr leftFunLengthExpr
    return $ stringInsts ++ lengthInsts ++ [HirIntrinsicFun HirLeftFun]
  RightFun {rightFunStringExpr, rightFunLengthExpr} -> do
    stringInsts <- translateStringVariableOrLiteral rightFunStringExpr
    lengthInsts <- translateExpr rightFunLengthExpr
    return $ stringInsts ++ lengthInsts ++ [HirIntrinsicFun HirRightFun]
  AsciiFun {asciiFunArgument} -> do
    argInsts <- translateStringVariableOrLiteral asciiFunArgument
    return $ argInsts ++ [HirIntrinsicFun HirAsciiFun]
  PointFun {pointFunPositionExpr} -> do
    posInsts <- translateExpr pointFunPositionExpr
    return $ posInsts ++ [HirIntrinsicFun HirPointFun]
  RndFun {rndRangeEnd} -> do
    return [HirPushNumLit (fromIntegral rndRangeEnd), HirIntrinsicFun HirRndFun]
  IntFun {intFunExpr} -> do
    exprInsts <- translateExpr intFunExpr
    return $ exprInsts ++ [HirIntrinsicFun HirIntFun]
  SgnFun {sgnFunExpr} -> do
    exprInsts <- translateExpr sgnFunExpr
    return $ exprInsts ++ [HirIntrinsicFun HirSgnFun]

translateExpr :: Expr BasicType -> State TranslationState [HirInst]
translateExpr Expr {exprInner, exprType = _} = case exprInner of
  NumLitExpr n -> return [HirPushNumLit n]
  StrLitExpr s -> return [HirPushStrLit s]
  LValueExpr lvalue -> return [HirPushLValue lvalue]
  BinExpr left op right -> do
    leftInsts <- translateExpr left
    rightInsts <- translateExpr right
    return $ leftInsts ++ rightInsts ++ [HirBinOp op]
  UnaryExpr op expr' -> do
    exprInsts <- translateExpr expr'
    return $ exprInsts ++ [HirUnaryOp op]
  FunCallExpr function -> translateFunction function

translateStmt :: Stmt BasicType -> State TranslationState [HirInst]
translateStmt stmt = case stmt of
  GoToStmt target -> do
    labelIdx <- gets (lookupLabelPanics target)
    return [HirGoto labelIdx]
  GoSubStmt target -> do
    labelIdx <- gets (lookupLabelPanics target)
    return [HirCall labelIdx]
  IfThenStmt condition stmt' ->
    case stmt' of
      GoToStmt target -> do
        labelIdx <- gets (lookupLabelPanics target)
        conditionInsts <- translateExpr condition
        return $ conditionInsts ++ [HirCondGoto labelIdx]
      GoSubStmt target -> do
        labelIdx <- gets (lookupLabelPanics target)
        conditionInsts <- translateExpr condition
        return $ conditionInsts ++ [HirCondCall labelIdx]
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
              translateInner <- translateStmt s
              conditionInsts <- translateExpr newCondition
              return $ conditionInsts ++ (HirCondGoto labelIdx : (translateInner ++ [HirLabel labelIdx]))
  ForStmt {forAssignment, forToExpr} -> do
    let forIdent = case assignmentLValue forAssignment of
          LValueIdent (IdentNumIdent ident) -> ident
          _ -> error "For loop assignment must be an identifier"
    -- Begin the for loop by inserting a label and storing the start condition
    (newState, labelIdx) <- gets (beginForLoop forIdent forToExpr)
    put newState

    return [HirAssign forAssignment, HirLabel labelIdx]
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
    conditionInsts <- translateExpr newJumpCondition
    return $ HirAssign nextAssignment : (conditionInsts ++ [HirCondGoto startLabelIdx])
  LetStmt {letAssignments} -> do
    let assignments = map HirAssign letAssignments
    return assignments
  PrintStmt {printKind, printExprs, printEnding, printUsingClause} -> do
    let -- All print expressions have line ending with no newline, except the last one
        -- which has the specified ending.
        hirPrints =
          map
            ( \expr ->
                HirIntrinsicCall $
                  HirPrint
                    { hirPrintExpression = expr
                    }
            )
            (init printExprs)
            ++ [ HirIntrinsicCall $
                   HirPrint
                     { hirPrintExpression = last printExprs
                     }
               ]
        usingClause = case printUsingClause of
          Just u -> [HirIntrinsicCall $ HirUsing u]
          Nothing -> []
        putchar = case printEnding of
          PrintEndingNewLine ->
            [ HirIntrinsicCall HirCls,
              HirIntrinsicCall $
                HirCursor
                  { hirCursorExpr = Expr {exprInner = NumLitExpr 0, exprType = BasicNumericType}
                  }
            ]
          _ -> []
        printInstrs = usingClause ++ hirPrints ++ putchar
        defaultPauseWaitTime =
          Expr
            { exprInner = NumLitExpr 64, -- FIXME: 1 second, find out what the default is
              exprType = BasicNumericType
            }
        endResult = case printKind of
          PrintKindPrint -> printInstrs
          PrintKindPause ->
            [HirIntrinsicCall HirWait {hirWaitTimeExpr = defaultPauseWaitTime}]
              ++ printInstrs
              ++ [ HirIntrinsicCall
                     HirWait
                       { hirWaitTimeExpr =
                           Expr
                             { exprInner = LValueExpr (LValueIdent userWaitTimeVarIdent),
                               exprType = BasicNumericType
                             }
                       }
                 ]

    return $ endResult
  UsingStmt usingClause -> return [HirIntrinsicCall $ HirUsing usingClause]
  InputStmt {inputPrintExpr, inputDestination} ->
    let inputStmt = HirIntrinsicCall HirInput {hirInputDestination = inputDestination}
     in case inputPrintExpr of
          Just expr ->
            return
              [ HirIntrinsicCall
                  HirPrint
                    { hirPrintExpression =
                        Expr
                          { exprInner = StrLitExpr expr,
                            exprType = BasicStringType
                          }
                    },
                inputStmt
              ]
          Nothing -> return [inputStmt]
  GprintStmt {gprintExprs, gprintEnding} -> do
    let hirGPrints =
          map
            (\expr -> HirIntrinsicCall HirGPrint {hirGPrintExpr = expr})
            (init gprintExprs)
            ++ [ HirIntrinsicCall
                   HirGPrint
                     { hirGPrintExpr = last gprintExprs
                     }
               ]
            ++ case gprintEnding of
              PrintEndingNewLine ->
                [ HirIntrinsicCall HirCls,
                  HirIntrinsicCall
                    HirGCursor
                      { hirGCursorExpr = Expr {exprInner = NumLitExpr 0, exprType = BasicNumericType}
                      }
                ]
              PrintEndingNoNewLine -> []
    return hirGPrints
  Comment -> return []
  ReturnStmt -> return [HirReturn]
  DimStmt _ -> return [] -- Already in symbol table
  EndStmt -> return [HirIntrinsicCall HirEnd]
  WaitStmt {waitForExpr} -> do
    -- Save the last wait time set by the user in a special variable
    let infiniteWaitTime =
          Expr
            { exprInner =
                UnaryExpr
                  UnaryMinusOp
                  Expr
                    { exprInner = NumLitExpr 1,
                      exprType = BasicNumericType
                    },
              exprType = BasicNumericType
            }
        unmaybeWait = fromMaybe infiniteWaitTime waitForExpr
        waitTimeStore =
          [ HirAssign
              ( Assignment
                  { assignmentLValue = LValueIdent userWaitTimeVarIdent,
                    assignmentExpr = unmaybeWait,
                    assignmentType = BasicNumericType
                  }
              )
          ]

    return $ waitTimeStore ++ [HirIntrinsicCall HirWait {hirWaitTimeExpr = unmaybeWait}]
  PokeStmt {pokeKind, pokeExprs} -> do
    let hirPokes =
          map
            ( \expr ->
                HirIntrinsicCall
                  HirPoke
                    { hirPokeMemoryArea = pokeKind,
                      hirPokeValue = expr
                    }
            )
            pokeExprs
    return hirPokes
  RandomStmt -> return [HirIntrinsicCall HirRandom]
  ClsStmt -> return [HirIntrinsicCall HirCls]
  ClearStmt -> return [HirIntrinsicCall HirClear]
  CursorStmt {cursorExpr} -> return [HirIntrinsicCall HirCursor {hirCursorExpr = cursorExpr}]
  GCursorStmt {gCursorExpr} -> return [HirIntrinsicCall HirGCursor {hirGCursorExpr = gCursorExpr}]
  ReadStmt _ -> error "Unimplemented: ReadStmt"
  DataStmt _ -> error "Unimplemented: DataStmt"
  RestoreStmt _ -> error "Unimplemented: RestoreStmt"
  BeepStmt
    { beepStmtRepetitionsExpr,
      beepStmtOptionalParams
    } ->
      return
        [ HirIntrinsicCall
            HirBeepStmt
              { hirBeepStmtRepetitionsExpr = beepStmtRepetitionsExpr,
                hirBeepStmtOptionalParams = beepStmtOptionalParams
              }
        ]

translateLine :: Line BasicType -> State TranslationState [HirInst]
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
      flattenedStmts =
        HirAssign
          ( Assignment
              { assignmentLValue = LValueIdent userWaitTimeVarIdent,
                assignmentExpr =
                  Expr
                    { exprInner =
                        UnaryExpr
                          UnaryMinusOp
                          (Expr {exprInner = NumLitExpr 1, exprType = BasicNumericType}),
                      exprType = BasicNumericType
                    },
                assignmentType = BasicNumericType
              }
          )
          : concat translatedStmts
   in HirProgram flattenedStmts