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

lookupNumberLiteralOffset :: Double -> TranslationState -> Word16
lookupNumberLiteralOffset num state' =
  case Data.Map.lookup num (numberLiteralMap (symbolTable state')) of
    Just offset -> offset
    Nothing -> error $ "Number literal not found: " ++ show num

irUserSetSleepTimeFakeVarName :: String
irUserSetSleepTimeFakeVarName = "user_set_sleep_time"

loadIndirectIntoAlXThroughYReg :: Word16 -> [IrInst]
loadIndirectIntoAlXThroughYReg addr =
  [ IrLoadImmediateToYReg addr,
    IrLoadFromYRegToAlX
  ]

translateIdent :: Ident -> State TranslationState ([IrInst], BasicType)
translateIdent id'@Ident {identHasDollar} = do
  var <- gets (lookupSymbol id' . symbolTable)
  let varOffset =
        case var of
          Just Variable {variableOffset} -> variableOffset
          Nothing -> error $ "Variable not found: " ++ show id'

  return
    ( loadIndirectIntoAlXThroughYReg varOffset,
      if identHasDollar then BasicStringType else BasicNumericType
    )

loadImmediateToAlx :: Double -> State TranslationState [IrInst]
loadImmediateToAlx imm = do
  immAddr <- gets (lookupNumberLiteralOffset imm)
  return [IrLoadImmediateToYReg immAddr, IrLoadFromYRegToAlX]

loadImmediateToAlY :: Double -> State TranslationState [IrInst]
loadImmediateToAlY imm = do
  immAddr <- gets (lookupNumberLiteralOffset imm)
  return [IrLoadImmediateToYReg immAddr, IrLoadFromYRegToAlX, IrCopyAlXToAlY]

translateIdentAddrToAlX :: Ident -> State TranslationState ([IrInst], BasicType)
translateIdentAddrToAlX id'@Ident {identHasDollar} = do
  var <- gets (lookupSymbol id' . symbolTable)
  let varOffset =
        case var of
          Just Variable {variableOffset} -> variableOffset
          Nothing -> error $ "Variable not found: " ++ show id'
  insts <- loadImmediateToAlx (fromIntegral varOffset)

  return
    ( insts,
      if identHasDollar then BasicStringType else BasicNumericType
    )

translateLValueRead :: LValue BasicType -> State TranslationState ([IrInst], BasicType)
translateLValueRead (LValueIdent ident) = translateIdent ident
translateLValueRead LValueArrayAccess {lValueArrayIdent, lValueArrayIndex} = do
  (identAddrToAlX, ty) <- translateIdentAddrToAlX lValueArrayIdent
  indexInsts <- translateExpr lValueArrayIndex

  return
    ( identAddrToAlX
        ++ [IrPushAlX]
        ++ indexInsts
        ++ [IrPopAlY, IrCallFunction IrAddOp, IrLoadAddressFromAlXToYReg, IrLoadFromYRegToAlX],
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
        ( loadIndirectIntoAlXThroughYReg varOffset,
          BasicNumericType
        )
    InkeyPseudoVar -> do
      var <- gets (lookupFakeSymbol "inkey" . symbolTable)
      let varOffset =
            case var of
              Just Variable {variableOffset} -> variableOffset
              Nothing -> error "Inkey pseudo variable not found"
      return
        ( loadIndirectIntoAlXThroughYReg varOffset,
          BasicStringType
        )

translateStringVariableOrLiteral :: StringVariableOrLiteral -> State TranslationState [IrInst]
translateStringVariableOrLiteral (StringVariable var) = do
  (r, _) <- translateIdent var
  return r
translateStringVariableOrLiteral (StringLiteral str) = do
  strOffset <- gets (lookupStringLiteralOffset str)
  return $ loadIndirectIntoAlXThroughYReg strOffset

translateFunction :: Function BasicType -> State TranslationState [IrInst]
translateFunction function = case function of
  MidFun {midFunStringExpr, midFunStartExpr, midFunLengthExpr} -> do
    stringInsts <- translateStringVariableOrLiteral midFunStringExpr
    startInsts <- translateExpr midFunStartExpr
    lengthInsts <- translateExpr midFunLengthExpr
    return $ stringInsts ++ startInsts ++ lengthInsts ++ [IrCallFunction IrMidOp]
  LeftFun {leftFunStringExpr, leftFunLengthExpr} -> do
    stringInsts <- translateStringVariableOrLiteral leftFunStringExpr
    lengthInsts <- translateExpr leftFunLengthExpr
    return $ stringInsts ++ lengthInsts ++ [IrCallFunction IrLeftOp]
  RightFun {rightFunStringExpr, rightFunLengthExpr} -> do
    stringInsts <- translateStringVariableOrLiteral rightFunStringExpr
    lengthInsts <- translateExpr rightFunLengthExpr
    return $ stringInsts ++ lengthInsts ++ [IrCallFunction IrRightOp]
  AsciiFun {asciiFunArgument} -> do
    argInsts <- translateStringVariableOrLiteral asciiFunArgument
    return $ argInsts ++ [IrCallFunction IrAsciiOp]
  PointFun {pointFunPositionExpr} -> do
    posInsts <- translateExpr pointFunPositionExpr
    return $ posInsts ++ [IrCallFunction IrPointOp]
  RndFun {rndRangeEnd} -> do
    -- FIXME: wrong, rndRangeEnd should be an expression
    insts <- loadImmediateToAlx (fromIntegral rndRangeEnd)
    return $ insts ++ [IrCallFunction IrRndOp]
  IntFun {intFunExpr} -> do
    exprInsts <- translateExpr intFunExpr
    return $ exprInsts ++ [IrCallFunction IrIntOp]
  SgnFun {sgnFunExpr} -> do
    exprInsts <- translateExpr sgnFunExpr
    return $ exprInsts ++ [IrCallFunction IrSgnOp]

translateBinOp :: BinOperator -> IrInst
translateBinOp AddOp = IrCallFunction IrAddOp
translateBinOp SubtractOp = IrCallFunction IrSubOp
translateBinOp MultiplyOp = IrCallFunction IrMulOp
translateBinOp DivideOp = IrCallFunction IrDivOp
translateBinOp CaretOp = IrCallFunction IrExponentOp
translateBinOp AndOp = IrCallFunction IrAndOp
translateBinOp OrOp = IrCallFunction IrOrOp
translateBinOp EqualOp = IrCallFunction IrEqOp
translateBinOp NotEqualOp = IrCallFunction IrNeqOp
translateBinOp LessThanOp = IrCallFunction IrLtOp
translateBinOp LessThanOrEqualOp = IrCallFunction IrLeqOp
translateBinOp GreaterThanOp = IrCallFunction IrGtOp
translateBinOp GreaterThanOrEqualOp = IrCallFunction IrGeqOp

translateUnaryOp :: UnaryOperator -> State TranslationState [IrInst]
translateUnaryOp UnaryMinusOp = do
  insts <- loadImmediateToAlx 0
  -- 0 - x
  return $
    [ -- AL-Y = AL-X
      IrCopyAlXToAlY
    ]
      -- AL-X = 0
      ++ insts
      -- AL-X = AL-X - AL-Y
      ++ [IrCallFunction IrSubOp]
translateUnaryOp UnaryNotOp = do
  insts <- loadImmediateToAlY 0
  -- 0 is false, anything else is true
  return $ insts ++ [IrCallFunction IrEqOp]
translateUnaryOp UnaryPlusOp = do
  -- x
  return []

translateExpr :: Expr BasicType -> State TranslationState [IrInst]
translateExpr Expr {exprInner, exprType = _} = case exprInner of
  NumLitExpr n -> loadImmediateToAlx n
  StrLitExpr s -> do
    strOffset <- gets (lookupStringLiteralOffset s)
    return $ loadIndirectIntoAlXThroughYReg strOffset
  LValueExpr lvalue -> do
    (lvalue', _) <- translateLValueRead lvalue
    return lvalue'
  BinExpr left op right -> do
    leftInsts <- translateExpr left
    rightInsts <- translateExpr right
    return $ leftInsts ++ [IrCopyAlXToAlY] ++ rightInsts ++ [translateBinOp op]
  UnaryExpr op expr' -> do
    exprInsts <- translateExpr expr'
    unaryOpInsts <- translateUnaryOp op
    return $ exprInsts ++ unaryOpInsts
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
        ( [IrLoadImmediateToYReg varOffset],
          if identHasDollar then BasicStringType else BasicNumericType
        )
    LValueArrayAccess {lValueArrayIdent, lValueArrayIndex} -> do
      (identAddrToAlX, ty) <- translateIdentAddrToAlX lValueArrayIdent
      indexInsts <- translateExpr lValueArrayIndex

      return
        ( identAddrToAlX
            ++ [IrPushAlX]
            ++ indexInsts
            ++ [IrPopAlY, IrCallFunction IrAddOp, IrLoadAddressFromAlXToYReg],
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
            ( [IrLoadImmediateToYReg varOffset],
              BasicNumericType
            )
        InkeyPseudoVar -> do
          var <- gets (lookupFakeSymbol "inkey" . symbolTable)
          let varOffset =
                case var of
                  Just Variable {variableOffset} -> variableOffset
                  Nothing -> error "Inkey pseudo variable not found"
          return
            ( [IrLoadImmediateToYReg varOffset],
              BasicStringType
            )

translateAssignment :: Assignment BasicType -> State TranslationState [IrInst]
translateAssignment Assignment {assignmentLValue, assignmentExpr, assignmentType = _} = do
  (lvalueInsts, _) <- translateLValueIntoYreg assignmentLValue
  exprInsts <- translateExpr assignmentExpr
  return $ lvalueInsts ++ [IrPushYReg] ++ exprInsts ++ [IrPopYReg, IrStoreAlXToYReg]

-- Assume sleep time is in AL-X
sleepInstructions :: [IrInst]
sleepInstructions =
  [ IrCopyAlXToAReg,
    IrCopyARegToTimer0,
    IrHalt
  ]

cursorPtrAddress :: Word16
cursorPtrAddress = 0x7875

inputBufferAddress :: Word16
inputBufferAddress = 0x7BB0

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
        return $ conditionInsts ++ [IrConditionalGoto labelIdx]
      GoSubStmt target -> do
        labelIdx <- gets (lookupLabelPanics target)
        conditionInsts <- translateExpr condition
        return $ conditionInsts ++ [IrConditionalCall labelIdx]
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
              return $ conditionInsts ++ (IrConditionalGoto labelIdx : (translateInner ++ [IrLabel labelIdx]))
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
    return $ assignInsts ++ conditionInsts ++ [IrConditionalGoto startLabelIdx]
  LetStmt {letAssignments} -> do
    assignments <- mapM translateAssignment letAssignments
    return $ concat assignments
  PrintStmt {printKind, printExprs, printEnding, printUsingClause} -> do
    -- All print expressions have line ending with no newline, except the last one
    -- which has the specified ending.
    let exprTyToInst ty = case ty of
          BasicNumericType ->
            [ IrCallIntrinsic IrNumberToString,
              IrCallIntrinsic IrPrintString
            ]
          BasicStringType ->
            [ IrCallIntrinsic IrLoadStringHeader,
              IrCallIntrinsic IrPrintString
            ]
          _ -> error $ "Unsupported print expression type: " ++ show ty

    irPrints <-
      mapM
        ( \expr -> do
            exprInsts <- translateExpr expr
            return $ exprInsts ++ exprTyToInst (exprType expr)
        )
        (init printExprs)

    lastExprInsts <- translateExpr (last printExprs)
    let lastPrintInst = exprTyToInst $ exprType (last printExprs)
    let irPrints' = concat irPrints ++ lastExprInsts ++ lastPrintInst
    let usingClause = case printUsingClause of
          Just (UsingClause u) -> [IrCallIntrinsic $ IrFormatUsing u]
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
            return $ loadIndirectIntoAlXThroughYReg varOffset
          PrintKindPause ->
            -- FIXME: more or less a second, research true value
            loadImmediateToAlx 64
        return $
          sleepInsts
            ++ sleepInstructions
            ++ [IrCallIntrinsic IrClearScreen]
            ++ cursorInsts
            ++ [IrStoreAlXToAddress 0x7875]
      _ -> return []

    return $ usingClause ++ irPrints' ++ putchar
  UsingStmt (UsingClause u) -> return [IrCallIntrinsic $ IrFormatUsing u]
  InputStmt {inputPrintExpr, inputDestination} -> do
    (inputDest, ty) <- translateLValueIntoYreg inputDestination
    let inputInst = case ty of
          BasicNumericType ->
            [ IrCallIntrinsic IrInputString,
              IrCallIntrinsic IrStringToNumber,
              IrStoreAlXToYReg
            ]
          BasicStringType ->
            [ IrCallIntrinsic IrInputString,
              IrLoadImmediateToUReg inputBufferAddress,
              IrCallIntrinsic IrStoreStringHeader
            ]
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
        return $
          printExprInsts
            ++ [ IrCallIntrinsic IrLoadStringHeader,
                 IrCallIntrinsic IrPrintString
               ]
            ++ inputStmt
      Nothing -> return inputStmt
  GprintStmt {gprintExprs, gprintEnding} -> do
    gprintInsts <-
      mapM
        ( \e -> do
            translatedExpr <- translateExpr e
            let gprintExprInsts =
                  case exprType e of
                    BasicNumericType -> [IrCallIntrinsic IrGraphicPrintNumber]
                    BasicStringType -> [IrCallIntrinsic IrGraphicPrintString]
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
          loadIndirectIntoAlXThroughYReg varOffset
            ++ sleepInstructions
            ++ [IrCallIntrinsic IrClearScreen]
            ++ exprInsts
            ++ [IrCallIntrinsic IrSetGraphicCursor]
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
        ++ loadIndirectIntoAlXThroughYReg varOffset
        ++ sleepInstructions
  PokeStmt {pokeKind, pokeExprs} -> do
    -- The first expression is the address to begin poking
    -- The rest are the values to poke
    pokeAddressInsts <- translateExpr (head pokeExprs)
    pokeValuesInsts <- mapM translateExpr (tail pokeExprs)
    let pokeValuesInsts' =
          concatMap
            ( \inst ->
                inst
                  ++ [ IrCallIntrinsic
                         ( IrPokeMemory
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
        ++ [IrCallIntrinsic IrSetPokeAddress]
        ++ pokeValuesInsts'
  RandomStmt -> return [IrCallIntrinsic IrRandomize]
  ClsStmt -> do
    insts <- loadImmediateToAlx 0
    return $
      [IrCallIntrinsic IrClearScreen]
        ++ insts
        ++ [IrStoreAlXToAddress cursorPtrAddress]
  ClearStmt -> return [IrCallIntrinsic IrClearVariables]
  CursorStmt {cursorExpr} -> do
    exprInsts <- translateExpr cursorExpr
    return $ exprInsts ++ [IrStoreAlXToAddress cursorPtrAddress]
  GCursorStmt {gCursorExpr} -> do
    gCursorExprInsts <- translateExpr gCursorExpr
    return $ gCursorExprInsts ++ [IrCallIntrinsic IrSetGraphicCursor]
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
              ++ [IrCallIntrinsic $ IrBeep True]
        Nothing -> do
          repetitionsInsts <- translateExpr beepStmtRepetitionsExpr
          return $ repetitionsInsts ++ [IrCallIntrinsic $ IrBeep False]

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