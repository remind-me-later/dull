module Ir.Translate where

import Ast.Types
import Control.Monad.State
import Data.Map qualified
import Data.Maybe (fromMaybe)
import Data.Word (Word16)
import Ir.Types
import SymbolTable
import TypeSystem (BasicType (..))

data TranslationState = TranslationState
  { labelToInt :: Data.Map.Map GotoTarget Int,
    nextLabelIdx :: Int,
    forStartToLabel :: Data.Map.Map Ident (Int, Expr BasicType),
    symbolTable :: SymbolTable
  }

insertFakeIdent :: String -> TranslationState -> TranslationState
insertFakeIdent name state' =
  let newState = state' {symbolTable = insertFakeVariable name BasicNumericType (symbolTable state')}
   in newState

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

irUserSetSleepTimeFakeVarName :: String
irUserSetSleepTimeFakeVarName = "user_set_sleep_time"

translateIdent :: Ident -> State TranslationState ([IrInst], BasicType)
translateIdent id'@Ident {identHasDollar} = do
  var <- gets (lookupSymbol id' . symbolTable)
  let varOffset =
        case var of
          Just Variable {variableOffset} -> variableOffset
          Nothing -> error $ "Variable not found: " ++ show id'

  return
    ( [IrLdImmIndirectIntoAlX varOffset],
      if identHasDollar then BasicStringType else BasicNumericType
    )

translateIdentAddr :: Ident -> State TranslationState ([IrInst], BasicType)
translateIdentAddr id'@Ident {identHasDollar} = do
  var <- gets (lookupSymbol id' . symbolTable)
  let varOffset =
        case var of
          Just Variable {variableOffset} -> variableOffset
          Nothing -> error $ "Variable not found: " ++ show id'

  return
    ( [IrImmToYreg varOffset],
      if identHasDollar then BasicStringType else BasicNumericType
    )

translateLValueRead :: LValue BasicType -> State TranslationState ([IrInst], BasicType)
translateLValueRead (LValueIdent ident) = translateIdent ident
translateLValueRead LValueArrayAccess {lValueArrayIdent, lValueArrayIndex} = do
  (identAddr, ty) <- translateIdentAddr lValueArrayIdent
  indexInsts <- translateExpr lValueArrayIndex

  return
    ( identAddr
        ++ [IrAlXToAlY]
        ++ indexInsts
        ++ [IrFun IrAddOp, IrAddrInAlXToYreg, IrAddrInYregIntoAlX],
      ty
    )
translateLValueRead (LValuePseudoVar pseudoVar) =
  -- TODO: fix this, we should translate psuedo vars into intrinsic calls
  case pseudoVar of
    TimePseudoVar -> do
      var <- gets (lookupFakeSymbol "time" . symbolTable)
      let varOffset =
            case var of
              Just Variable {variableOffset} -> variableOffset
              Nothing -> error "Time pseudo variable not found"
      return
        ( [IrLdImmIndirectIntoAlX varOffset],
          BasicNumericType
        )
    InkeyPseudoVar -> do
      var <- gets (lookupFakeSymbol "inkey" . symbolTable)
      let varOffset =
            case var of
              Just Variable {variableOffset} -> variableOffset
              Nothing -> error "Inkey pseudo variable not found"
      return
        ( [IrLdImmIndirectIntoAlX varOffset],
          BasicStringType
        )

translateStringVariableOrLiteral :: StringVariableOrLiteral -> State TranslationState [IrInst]
translateStringVariableOrLiteral (StringVariable var) = do
  (r, _) <- translateIdent var
  return r
translateStringVariableOrLiteral (StringLiteral str) = do
  strOffset <- gets (lookupStringLiteralOffset str)
  return [IrLdImmIndirectIntoAlX strOffset]

translateFunction :: Function BasicType -> State TranslationState [IrInst]
translateFunction function = case function of
  MidFun {midFunStringExpr, midFunStartExpr, midFunLengthExpr} -> do
    stringInsts <- translateStringVariableOrLiteral midFunStringExpr
    startInsts <- translateExpr midFunStartExpr
    lengthInsts <- translateExpr midFunLengthExpr
    return $ stringInsts ++ startInsts ++ lengthInsts ++ [IrFun IrMidOp]
  LeftFun {leftFunStringExpr, leftFunLengthExpr} -> do
    stringInsts <- translateStringVariableOrLiteral leftFunStringExpr
    lengthInsts <- translateExpr leftFunLengthExpr
    return $ stringInsts ++ lengthInsts ++ [IrFun IrLeftOp]
  RightFun {rightFunStringExpr, rightFunLengthExpr} -> do
    stringInsts <- translateStringVariableOrLiteral rightFunStringExpr
    lengthInsts <- translateExpr rightFunLengthExpr
    return $ stringInsts ++ lengthInsts ++ [IrFun IrRightOp]
  AsciiFun {asciiFunArgument} -> do
    argInsts <- translateStringVariableOrLiteral asciiFunArgument
    return $ argInsts ++ [IrFun IrAsciiOp]
  PointFun {pointFunPositionExpr} -> do
    posInsts <- translateExpr pointFunPositionExpr
    return $ posInsts ++ [IrFun IrPointOp]
  RndFun {rndRangeEnd} ->
    return
      [ IrLdImmIntoAlX (fromIntegral rndRangeEnd),
        IrFun IrRndOp
      ]
  IntFun {intFunExpr} -> do
    exprInsts <- translateExpr intFunExpr
    return $ exprInsts ++ [IrFun IrIntOp]
  SgnFun {sgnFunExpr} -> do
    exprInsts <- translateExpr sgnFunExpr
    return $ exprInsts ++ [IrFun IrSgnOp]

translateBinOp :: BinOperator -> IrInst
translateBinOp AddOp = IrFun IrAddOp
translateBinOp SubtractOp = IrFun IrSubOp
translateBinOp MultiplyOp = IrFun IrMulOp
translateBinOp DivideOp = IrFun IrDivOp
translateBinOp CaretOp = IrFun IrExponentOp
translateBinOp AndOp = IrFun IrAndOp
translateBinOp OrOp = IrFun IrOrOp
translateBinOp EqualOp = IrFun IrEqOp
translateBinOp NotEqualOp = IrFun IrNeqOp
translateBinOp LessThanOp = IrFun IrLtOp
translateBinOp LessThanOrEqualOp = IrFun IrLeqOp
translateBinOp GreaterThanOp = IrFun IrGtOp
translateBinOp GreaterThanOrEqualOp = IrFun IrGeqOp

translateUnaryOp :: UnaryOperator -> [IrInst]
translateUnaryOp UnaryMinusOp = do
  -- 0 - x
  [ -- AL-Y = AL-X
    IrAlXToAlY,
    -- AL-X = 0
    IrLdImmIntoAlX 0,
    -- AL-X = AL-X - AL-Y
    IrFun IrSubOp
    ]
translateUnaryOp UnaryNotOp = do
  -- 0 is false, anything else is true
  [ IrLdImmIntoAlY 0,
    IrFun IrEqOp
    ]
translateUnaryOp UnaryPlusOp = do
  -- x
  []

translateExpr :: Expr BasicType -> State TranslationState [IrInst]
translateExpr Expr {exprInner, exprType = _} = case exprInner of
  NumLitExpr n ->
    return [IrLdImmIntoAlX n]
  StrLitExpr s -> do
    strOffset <- gets (lookupStringLiteralOffset s)
    return [IrLdImmIndirectIntoAlX strOffset]
  LValueExpr lvalue -> do
    (lvalue', _) <- translateLValueRead lvalue
    return lvalue'
  BinExpr left op right -> do
    leftInsts <- translateExpr left
    rightInsts <- translateExpr right
    return $ leftInsts ++ [IrAlXToAlY] ++ rightInsts ++ [translateBinOp op]
  UnaryExpr op expr' -> do
    exprInsts <- translateExpr expr'
    return $ exprInsts ++ translateUnaryOp op
  FunCallExpr function -> translateFunction function

translateLValueIntoYreg :: LValue BasicType -> State TranslationState ([IrInst], BasicType)
translateLValueIntoYreg assignmentLValue = do
  case assignmentLValue of
    LValueIdent id'@Ident {identHasDollar} -> do
      var <- gets (lookupSymbol id' . symbolTable)
      let varOffset =
            case var of
              Just Variable {variableOffset} -> variableOffset
              Nothing -> error $ "Variable not found: " ++ show id'
      return
        ( [IrImmToYreg varOffset],
          if identHasDollar then BasicStringType else BasicNumericType
        )
    LValueArrayAccess {lValueArrayIdent, lValueArrayIndex} -> do
      (identAddr, ty) <- translateIdentAddr lValueArrayIdent
      indexInsts <- translateExpr lValueArrayIndex

      return
        ( identAddr
            ++ [IrAlXToAlY]
            ++ indexInsts
            ++ [IrFun IrAddOp, IrStAlXInYreg],
          ty
        )
    LValuePseudoVar pseudoVar -> do
      case pseudoVar of
        TimePseudoVar -> do
          var <- gets (lookupFakeSymbol "time" . symbolTable)
          let varOffset =
                case var of
                  Just Variable {variableOffset} -> variableOffset
                  Nothing -> error "Time pseudo variable not found"
          return
            ( [IrImmToYreg varOffset],
              BasicNumericType
            )
        InkeyPseudoVar -> do
          var <- gets (lookupFakeSymbol "inkey" . symbolTable)
          let varOffset =
                case var of
                  Just Variable {variableOffset} -> variableOffset
                  Nothing -> error "Inkey pseudo variable not found"
          return
            ( [IrImmToYreg varOffset],
              BasicStringType
            )

translateAssignment :: Assignment BasicType -> State TranslationState [IrInst]
translateAssignment Assignment {assignmentLValue, assignmentExpr, assignmentType = _} = do
  (lvalueInsts, _) <- translateLValueIntoYreg assignmentLValue
  exprInsts <- translateExpr assignmentExpr
  return $ lvalueInsts ++ exprInsts ++ [IrStAlXInYreg]

translateStmt :: Stmt BasicType -> State TranslationState [IrInst]
translateStmt stmt = case stmt of
  GoToStmt target -> do
    labelIdx <- gets (lookupLabelPanics target)
    return [IrGoto labelIdx]
  GoSubStmt target -> do
    labelIdx <- gets (lookupLabelPanics target)
    return [IrCall labelIdx]
  IfThenStmt condition stmt' ->
    case stmt' of
      GoToStmt target -> do
        labelIdx <- gets (lookupLabelPanics target)
        conditionInsts <- translateExpr condition
        return $ conditionInsts ++ [IrCondGoto labelIdx]
      GoSubStmt target -> do
        labelIdx <- gets (lookupLabelPanics target)
        conditionInsts <- translateExpr condition
        return $ conditionInsts ++ [IrCondCall labelIdx]
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
              return $ conditionInsts ++ (IrCondGoto labelIdx : (translateInner ++ [IrLabel labelIdx]))
  ForStmt {forAssignment, forToExpr} -> do
    let forIdent = case assignmentLValue forAssignment of
          LValueIdent ident -> ident
          _ -> error "For loop assignment must be an identifier"
    -- Begin the for loop by inserting a label and storing the start condition
    (newState, labelIdx) <- gets (beginForLoop forIdent forToExpr)
    put newState
    irAssignInsts <- translateAssignment forAssignment

    return $ irAssignInsts ++ [IrLabel labelIdx]
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
    return $ assignInsts ++ conditionInsts ++ [IrCondGoto startLabelIdx]
  LetStmt {letAssignments} -> do
    assignments <- mapM translateAssignment letAssignments
    return $ concat assignments
  PrintStmt {printKind, printExprs, printEnding, printUsingClause} -> do
    -- All print expressions have line ending with no newline, except the last one
    -- which has the specified ending.
    irPrints <-
      mapM
        ( \expr -> do
            exprInsts <- translateExpr expr
            let printInsts =
                  case exprType expr of
                    BasicNumericType -> [IrIntrinsicCall IrPrintNum]
                    BasicStringType -> [IrIntrinsicCall IrPrintStr]
                    _ -> error $ "Unsupported print expression type: " ++ show (exprType expr)
            return $ exprInsts ++ printInsts
        )
        (init printExprs)

    lastExprInsts <- translateExpr (last printExprs)
    let lastPrintInst =
          case exprType (last printExprs) of
            BasicNumericType -> IrIntrinsicCall IrPrintNum
            BasicStringType -> IrIntrinsicCall IrPrintStr
            _ -> error $ "Unsupported last print expression type: " ++ show (exprType (last printExprs))
    let irPrints' = concat irPrints ++ lastExprInsts ++ [lastPrintInst]
    let usingClause = case printUsingClause of
          Just (UsingClause u) -> [IrIntrinsicCall $ IrUsing u]
          Nothing -> []
    putchar <- case printEnding of
      PrintEndingNewLine -> do
        cursorInsts <- translateExpr Expr {exprInner = NumLitExpr 0, exprType = BasicNumericType}

        sleepInsts <- case printKind of
          PrintKindPrint -> do
            var <- gets (lookupFakeSymbol irUserSetSleepTimeFakeVarName . symbolTable)
            let varOffset =
                  case var of
                    Just Variable {variableOffset} -> variableOffset
                    Nothing -> error "User set sleep time variable not found"
            return [IrLdImmIndirectIntoAlX varOffset]
          PrintKindPause ->
            -- FIXME: more or less a second, research true value
            return [IrLdImmIntoAlX 64]
        return $
          sleepInsts
            ++ [ IrIntrinsicCall IrSleep,
                 IrIntrinsicCall IrCls
               ]
            ++ cursorInsts
            ++ [IrIntrinsicCall IrCursor]
      _ -> return []

    return $ usingClause ++ irPrints' ++ putchar
  UsingStmt (UsingClause u) -> return [IrIntrinsicCall $ IrUsing u]
  InputStmt {inputPrintExpr, inputDestination} -> do
    (inputDest, ty) <- translateLValueIntoYreg inputDestination
    let inputInst = case ty of
          BasicNumericType -> [IrIntrinsicCall IrInputNum]
          BasicStringType -> [IrIntrinsicCall IrInputStr]
          _ -> error $ "Unsupported input destination type: " ++ show ty

    let inputStmt = inputDest ++ inputInst

    case inputPrintExpr of
      Just expr -> do
        printExprInsts <-
          translateExpr
            Expr
              { exprInner = StrLitExpr expr,
                exprType = BasicStringType
              }
        return $ printExprInsts ++ [IrIntrinsicCall IrPrintStr] ++ inputStmt
      Nothing -> return inputStmt
  GprintStmt {gprintExprs, gprintEnding} -> do
    gprintInsts <-
      mapM
        ( \e -> do
            translatedExpr <- translateExpr e
            let gprintExprInsts =
                  case exprType e of
                    BasicNumericType -> [IrIntrinsicCall IrGPrintNum]
                    BasicStringType -> [IrIntrinsicCall IrGPrintStr]
                    _ -> error $ "Unsupported gprint expression type: " ++ show (exprType e)
            return $ translatedExpr ++ gprintExprInsts
        )
        gprintExprs
    let gprintsInsts' = concat gprintInsts

    ending <- case gprintEnding of
      PrintEndingNewLine -> do
        exprInsts <- translateExpr Expr {exprInner = NumLitExpr 0, exprType = BasicNumericType}
        var <- gets (lookupFakeSymbol irUserSetSleepTimeFakeVarName . symbolTable)
        let varOffset =
              case var of
                Just Variable {variableOffset} -> variableOffset
                Nothing -> error "User set sleep time variable not found"
        return $
          [ IrLdImmIndirectIntoAlX varOffset,
            IrIntrinsicCall IrSleep,
            IrIntrinsicCall IrCls
          ]
            ++ exprInsts
            ++ [IrIntrinsicCall IrGCursor]
      PrintEndingNoNewLine -> return []

    return $ gprintsInsts' ++ ending
  Comment -> return []
  ReturnStmt -> return [IrReturn]
  DimStmt _ -> return [] -- Already in symbol table
  EndStmt -> return [IrReturn]
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
    var <- gets (lookupFakeSymbol irUserSetSleepTimeFakeVarName . symbolTable)
    let varOffset =
          case var of
            Just Variable {variableOffset} -> variableOffset
            Nothing -> error "User set sleep time variable not found"

    return $
      unmaybeWait
        ++ [ IrLdImmIndirectIntoAlX varOffset,
             IrIntrinsicCall IrSleep
           ]
  PokeStmt {pokeKind, pokeExprs} -> do
    -- The first expression is the address to begin poking
    -- The rest are the values to poke
    pokeAddressInsts <- translateExpr (head pokeExprs)
    pokeValuesInsts <- mapM translateExpr (tail pokeExprs)
    let pokeValuesInsts' =
          concatMap
            ( \inst ->
                inst
                  ++ [ IrIntrinsicCall
                         ( IrPoke
                             { irPokeMemoryArea =
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
        ++ [IrIntrinsicCall IrSetPokeAddress]
        ++ pokeValuesInsts'
  RandomStmt -> return [IrIntrinsicCall IrRandom]
  ClsStmt -> return [IrIntrinsicCall IrCls]
  ClearStmt -> return [IrIntrinsicCall IrClear]
  CursorStmt {cursorExpr} -> do
    exprInsts <- translateExpr cursorExpr
    return $ exprInsts ++ [IrIntrinsicCall IrCursor]
  GCursorStmt {gCursorExpr} -> do
    gCursorExprInsts <- translateExpr gCursorExpr
    return $ gCursorExprInsts ++ [IrIntrinsicCall IrGCursor]
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
              ++ [IrIntrinsicCall $ IrBeepStmt True]
        Nothing -> do
          repetitionsInsts <- translateExpr beepStmtRepetitionsExpr
          return $ repetitionsInsts ++ [IrIntrinsicCall $ IrBeepStmt False]

translateLine :: Line BasicType -> State TranslationState [IrInst]
translateLine Line {lineNumber, lineLabel, lineStmts} = do
  label <- case lineLabel of
    Just lbl -> do
      labelIdx <- gets (lookupLabelInState (GoToLabel lbl))
      case labelIdx of
        Just idx -> return [IrLabel idx]
        Nothing -> return []
    Nothing -> return []
  number <- do
    lineNumberIdx <- gets (lookupLabelInState (GoToLine lineNumber))
    case lineNumberIdx of
      Just idx -> return [IrLabel idx]
      Nothing -> return []
  stmts <- mapM translateStmt lineStmts
  return $ number ++ label ++ concat stmts

translateProgram' :: Program BasicType -> State TranslationState IrProgram
translateProgram' (Program programLines) = do
  -- insert fake identifiers for special variables
  let fakeVars = ["time", "inkey", irUserSetSleepTimeFakeVarName]
  mapM_ (modify . insertFakeIdent) fakeVars

  let programLines' = Data.Map.elems programLines

  translatedLines <- mapM translateLine programLines'

  return IrProgram {irProgramStatements = concat translatedLines}

translateProgram :: Program BasicType -> SymbolTable -> (IrProgram, SymbolTable, Int)
translateProgram program symbolTable' =
  let (labelMap, nextIdx) = symbolTableUsedLabelsToInt symbolTable'
      initialState =
        TranslationState
          { labelToInt = labelMap,
            nextLabelIdx = nextIdx,
            forStartToLabel = Data.Map.empty,
            symbolTable = symbolTable'
          }
      (irProgram, finalState) = runState (translateProgram' program) initialState
   in (irProgram, symbolTable finalState, nextLabelIdx finalState)