module Hir.Translate where

import Ast.Types
import Control.Monad.State
import Data.Map qualified
import Data.Maybe (fromMaybe)
import Data.Word (Word16)
import Hir.Types
import SymbolTable
import TypeSystem (BasicType (..))

data TranslationState = TranslationState
  { labelToInt :: Data.Map.Map GotoTarget Int,
    nextLabelIdx :: Int,
    forStartToLabel :: Data.Map.Map Ident (Int, Expr BasicType),
    symbolTable :: SymbolTable
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

beginForLoop :: Ident -> Expr BasicType -> TranslationState -> (TranslationState, Int)
beginForLoop ident cond state' =
  let nextIdx = nextLabelIdx state'
      newForStartMap = Data.Map.insert ident (nextIdx, cond) (forStartToLabel state')
      newState = state' {forStartToLabel = newForStartMap, nextLabelIdx = nextIdx + 1}
   in (newState, nextIdx)

lookupForStartLabel :: Ident -> TranslationState -> (Int, Expr BasicType)
lookupForStartLabel ident state' =
  case Data.Map.lookup ident (forStartToLabel state') of
    Just idx -> idx
    Nothing -> error $ "For loop start label not found for: " ++ show ident

lookupStringLiteralOffset :: String -> TranslationState -> Word16
lookupStringLiteralOffset str state' =
  case Data.Map.lookup str (stringLiteralMap (symbolTable state')) of
    Just offset -> offset
    Nothing -> error $ "String literal not found: " ++ str

hirUserSetSleepTimeFakeVarName :: String
hirUserSetSleepTimeFakeVarName = "user_set_sleep_time"

translateIdent :: Ident -> State TranslationState ([HirInst], BasicType)
translateIdent Ident {identName, identHasDollar} =
  let newIdent =
        HirBasicIdent
          { hirIdentName = identName,
            hirIdentHasDollar = identHasDollar
          }
   in return
        ( [HirPush $ HirOperandVarAddr newIdent],
          if identHasDollar then BasicStringType else BasicNumericType
        )

translateLValue :: LValue BasicType -> State TranslationState ([HirInst], BasicType)
translateLValue (LValueIdent ident) = translateIdent ident
translateLValue LValueArrayAccess {lValueArrayIdent, lValueArrayIndex} = do
  (newIdent, ty) <- translateIdent lValueArrayIdent
  indexInsts <- translateExpr lValueArrayIndex

  return
    ( indexInsts
        ++ newIdent
        ++ [ HirOp HirAddOp,
             HirDeref
           ],
      ty
    )
translateLValue (LValuePseudoVar pseudoVar) =
  -- TODO: fix this, we should translate psuedo vars into intrinsic calls
  case pseudoVar of
    TimePseudoVar ->
      return ([HirPush $ HirOperandVarAddr HirFakeIdent {hirFakeIdentName = "time"}], BasicNumericType)
    InkeyPseudoVar ->
      return ([HirPush $ HirOperandVarAddr HirFakeIdent {hirFakeIdentName = "inkey"}], BasicStringType)

translateStringVariableOrLiteral :: StringVariableOrLiteral -> State TranslationState [HirInst]
translateStringVariableOrLiteral (StringVariable var) = do
  (r, _) <- translateIdent var
  return r
translateStringVariableOrLiteral (StringLiteral str) = return [HirPush $ HirOperandStrLitAddr str]

translateFunction :: Function BasicType -> State TranslationState [HirInst]
translateFunction function = case function of
  MidFun {midFunStringExpr, midFunStartExpr, midFunLengthExpr} -> do
    stringInsts <- translateStringVariableOrLiteral midFunStringExpr
    startInsts <- translateExpr midFunStartExpr
    lengthInsts <- translateExpr midFunLengthExpr
    return $ stringInsts ++ startInsts ++ lengthInsts ++ [HirOp HirMidOp]
  LeftFun {leftFunStringExpr, leftFunLengthExpr} -> do
    stringInsts <- translateStringVariableOrLiteral leftFunStringExpr
    lengthInsts <- translateExpr leftFunLengthExpr
    return $ stringInsts ++ lengthInsts ++ [HirOp HirLeftOp]
  RightFun {rightFunStringExpr, rightFunLengthExpr} -> do
    stringInsts <- translateStringVariableOrLiteral rightFunStringExpr
    lengthInsts <- translateExpr rightFunLengthExpr
    return $ stringInsts ++ lengthInsts ++ [HirOp HirRightOp]
  AsciiFun {asciiFunArgument} -> do
    argInsts <- translateStringVariableOrLiteral asciiFunArgument
    return $ argInsts ++ [HirOp HirAsciiOp]
  PointFun {pointFunPositionExpr} -> do
    posInsts <- translateExpr pointFunPositionExpr
    return $ posInsts ++ [HirOp HirPointOp]
  RndFun {rndRangeEnd} ->
    return [HirPush $ HirOperandNumLit (fromIntegral rndRangeEnd), HirOp HirRndOp]
  IntFun {intFunExpr} -> do
    exprInsts <- translateExpr intFunExpr
    return $ exprInsts ++ [HirOp HirIntOp]
  SgnFun {sgnFunExpr} -> do
    exprInsts <- translateExpr sgnFunExpr
    return $ exprInsts ++ [HirOp HirSgnOp]

translateBinOp :: BinOperator -> HirInst
translateBinOp AddOp = HirOp HirAddOp
translateBinOp SubtractOp = HirOp HirSubOp
translateBinOp MultiplyOp = HirOp HirMulOp
translateBinOp DivideOp = HirOp HirDivOp
translateBinOp CaretOp = HirOp HirExponentOp
translateBinOp AndOp = HirOp HirAndOp
translateBinOp OrOp = HirOp HirOrOp
translateBinOp EqualOp = HirOp HirEqOp
translateBinOp NotEqualOp = HirOp HirNeqOp
translateBinOp LessThanOp = HirOp HirLtOp
translateBinOp LessThanOrEqualOp = HirOp HirLeqOp
translateBinOp GreaterThanOp = HirOp HirGtOp
translateBinOp GreaterThanOrEqualOp = HirOp HirGeqOp

translateUnaryOp :: UnaryOperator -> [HirInst]
translateUnaryOp UnaryMinusOp = do
  -- 0 - x
  [HirPush $ HirOperandNumLit 0, HirOp HirSubOp]
translateUnaryOp UnaryNotOp = do
  -- 0 is false, anything else is true
  [HirPush $ HirOperandNumLit 0, HirOp HirEqOp]
translateUnaryOp UnaryPlusOp = do
  -- x
  []

translateExpr :: Expr BasicType -> State TranslationState [HirInst]
translateExpr Expr {exprInner, exprType = _} = case exprInner of
  NumLitExpr n -> return [HirPush $ HirOperandNumLit n]
  StrLitExpr s -> return [HirPush $ HirOperandStrLitAddr s]
  LValueExpr lvalue -> do
    (lvalueAddr, _) <- translateLValue lvalue
    return $ lvalueAddr ++ [HirDeref]
  BinExpr left op right -> do
    leftInsts <- translateExpr left
    rightInsts <- translateExpr right
    return $ leftInsts ++ rightInsts ++ [translateBinOp op]
  UnaryExpr op expr' -> do
    exprInsts <- translateExpr expr'
    return $ exprInsts ++ translateUnaryOp op
  FunCallExpr function -> translateFunction function

translateAssignment :: Assignment BasicType -> State TranslationState [HirInst]
translateAssignment Assignment {assignmentLValue, assignmentExpr, assignmentType = _} = do
  (lvalueAddr, _) <- translateLValue assignmentLValue
  exprInsts <- translateExpr assignmentExpr
  return $ lvalueAddr ++ exprInsts ++ [HirAssign]

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
          LValueIdent ident -> ident
          _ -> error "For loop assignment must be an identifier"
    -- Begin the for loop by inserting a label and storing the start condition
    (newState, labelIdx) <- gets (beginForLoop forIdent forToExpr)
    put newState
    hirAssignInsts <- translateAssignment forAssignment

    return $ hirAssignInsts ++ [HirLabel labelIdx]
  NextStmt {nextIdent} -> do
    (startLabelIdx, startCond) <- gets (lookupForStartLabel nextIdent)
    let identExpr =
          Expr
            { exprInner =
                LValueExpr (LValueIdent nextIdent),
              exprType = BasicNumericType
            }
        nextAssignment =
          Assignment
            { assignmentLValue = LValueIdent nextIdent,
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
    assignInsts <- translateAssignment nextAssignment
    return $ assignInsts ++ conditionInsts ++ [HirCondGoto startLabelIdx]
  LetStmt {letAssignments} -> do
    assignments <- mapM translateAssignment letAssignments
    return $ concat assignments
  PrintStmt {printKind, printExprs, printEnding, printUsingClause} -> do
    -- All print expressions have line ending with no newline, except the last one
    -- which has the specified ending.
    hirPrints <-
      mapM
        ( \expr -> do
            exprInsts <- translateExpr expr
            let printInsts =
                  case exprType expr of
                    BasicNumericType -> [HirIntrinsicCall HirPrintNum]
                    BasicStringType -> [HirIntrinsicCall HirPrintStr]
                    _ -> error $ "Unsupported print expression type: " ++ show (exprType expr)
            return $ exprInsts ++ printInsts
        )
        (init printExprs)

    lastExprInsts <- translateExpr (last printExprs)
    let lastPrintInst =
          case exprType (last printExprs) of
            BasicNumericType -> HirIntrinsicCall HirPrintNum
            BasicStringType -> HirIntrinsicCall HirPrintStr
            _ -> error $ "Unsupported last print expression type: " ++ show (exprType (last printExprs))
    let hirPrints' = concat hirPrints ++ lastExprInsts ++ [lastPrintInst]
    let usingClause = case printUsingClause of
          Just (UsingClause u) -> [HirIntrinsicCall $ HirUsing u]
          Nothing -> []
    putchar <- case printEnding of
      PrintEndingNewLine -> do
        cursorInsts <- translateExpr Expr {exprInner = NumLitExpr 0, exprType = BasicNumericType}

        let sleepInsts = case printKind of
              PrintKindPrint -> HirPush $ HirOperandVarAddr HirFakeIdent {hirFakeIdentName = hirUserSetSleepTimeFakeVarName}
              PrintKindPause -> HirPush $ HirOperandNumLit 64 -- FIXME: more or less a second, research true value
        return $
          [ sleepInsts,
            HirIntrinsicCall HirSleep,
            HirIntrinsicCall HirCls
          ]
            ++ cursorInsts
            ++ [HirIntrinsicCall HirCursor]
      _ -> return []

    return $ usingClause ++ hirPrints' ++ putchar
  UsingStmt (UsingClause u) -> return [HirIntrinsicCall $ HirUsing u]
  InputStmt {inputPrintExpr, inputDestination} -> do
    (inputDestAddr, ty) <- translateLValue inputDestination
    let inputInst = case ty of
          BasicNumericType -> [HirIntrinsicCall HirInputNum]
          BasicStringType -> [HirIntrinsicCall HirInputStr]
          _ -> error $ "Unsupported input destination type: " ++ show ty

    let inputStmt = inputDestAddr ++ inputInst

    case inputPrintExpr of
      Just expr -> do
        printExprInsts <-
          translateExpr
            Expr
              { exprInner = StrLitExpr expr,
                exprType = BasicStringType
              }
        return $ printExprInsts ++ [HirIntrinsicCall HirPrintStr] ++ inputStmt
      Nothing -> return inputStmt
  GprintStmt {gprintExprs, gprintEnding} -> do
    gprintInsts <-
      mapM
        ( \e -> do
            translatedExpr <- translateExpr e
            let gprintExprInsts =
                  case exprType e of
                    BasicNumericType -> [HirIntrinsicCall HirGPrintNum]
                    BasicStringType -> [HirIntrinsicCall HirGPrintStr]
                    _ -> error $ "Unsupported gprint expression type: " ++ show (exprType e)
            return $ translatedExpr ++ gprintExprInsts
        )
        gprintExprs
    let gprintsInsts' = concat gprintInsts

    ending <- case gprintEnding of
      PrintEndingNewLine -> do
        exprInsts <- translateExpr Expr {exprInner = NumLitExpr 0, exprType = BasicNumericType}
        return $
          [ HirPush $ HirOperandVarAddr HirFakeIdent {hirFakeIdentName = hirUserSetSleepTimeFakeVarName},
            HirIntrinsicCall HirSleep,
            HirIntrinsicCall HirCls
          ]
            ++ exprInsts
            ++ [HirIntrinsicCall HirGCursor]
      PrintEndingNoNewLine -> return []

    return $ gprintsInsts' ++ ending
  Comment -> return []
  ReturnStmt -> return [HirReturn]
  DimStmt _ -> return [] -- Already in symbol table
  EndStmt -> return [HirReturn]
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

    unmaybeWait <- translateExpr (fromMaybe infiniteWaitTime waitForExpr)

    return $
      [ HirPush $
          HirOperandVarAddr
            HirFakeIdent
              { hirFakeIdentName = hirUserSetSleepTimeFakeVarName
              }
      ]
        ++ unmaybeWait
        ++ [HirAssign]
  PokeStmt {pokeKind, pokeExprs} -> do
    -- The first expression is the address to begin poking
    -- The rest are the values to poke
    pokeAddressInsts <- translateExpr (head pokeExprs)
    pokeValuesInsts <- mapM translateExpr (tail pokeExprs)
    let pokeValuesInsts' =
          concatMap
            ( \inst ->
                inst
                  ++ [ HirIntrinsicCall
                         ( HirPoke
                             { hirPokeMemoryArea =
                                 case pokeKind of
                                   Me0 -> 0
                                   Me1 -> 1
                             }
                         )
                     ]
            )
            pokeValuesInsts
    return $
      pokeAddressInsts
        ++ [HirIntrinsicCall HirSetPokeAddress]
        ++ pokeValuesInsts'
  RandomStmt -> return [HirIntrinsicCall HirRandom]
  ClsStmt -> return [HirIntrinsicCall HirCls]
  ClearStmt -> return [HirIntrinsicCall HirClear]
  CursorStmt {cursorExpr} -> do
    exprInsts <- translateExpr cursorExpr
    return $ exprInsts ++ [HirIntrinsicCall HirCursor]
  GCursorStmt {gCursorExpr} -> do
    gCursorExprInsts <- translateExpr gCursorExpr
    return $ gCursorExprInsts ++ [HirIntrinsicCall HirGCursor]
  ReadStmt _ -> error "Unimplemented: ReadStmt"
  DataStmt _ -> error "Unimplemented: DataStmt"
  RestoreStmt _ -> error "Unimplemented: RestoreStmt"
  BeepStmt
    { beepStmtRepetitionsExpr,
      beepStmtOptionalParams
    } ->
      case beepStmtOptionalParams of
        Just (BeepOptionalParams {beepFrequency, beepDuration}) -> do
          repetitionsInsts <- translateExpr beepStmtRepetitionsExpr
          frequencyInsts <- translateExpr beepFrequency
          durationInsts <- translateExpr beepDuration
          return $
            repetitionsInsts
              ++ frequencyInsts
              ++ durationInsts
              ++ [HirIntrinsicCall $ HirBeepStmt True]
        Nothing -> do
          repetitionsInsts <- translateExpr beepStmtRepetitionsExpr
          return $ repetitionsInsts ++ [HirIntrinsicCall $ HirBeepStmt False]

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

translateProgram' :: Program BasicType -> State TranslationState HirProgram
translateProgram' (Program programLines) = do
  let programLines' = Data.Map.elems programLines

  translatedLines <- mapM translateLine programLines'

  return HirProgram {hirProgramStatements = concat translatedLines}

translateProgram :: Program BasicType -> SymbolTable -> HirProgram
translateProgram program symbolTable =
  let (labelMap, nextIdx) = symbolTableUsedLabelsToInt symbolTable
      initialState =
        TranslationState
          { labelToInt = labelMap,
            nextLabelIdx = nextIdx,
            forStartToLabel = Data.Map.empty,
            symbolTable = symbolTable
          }
   in evalState (translateProgram' program) initialState