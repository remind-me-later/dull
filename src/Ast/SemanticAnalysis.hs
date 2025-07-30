module Ast.SemanticAnalysis
  ( analyzeProgram,
  )
where

import Ast.Types
import Control.Monad (unless, when)
import Control.Monad.State
import Data.Functor (void)
import Data.Maybe (fromMaybe, isJust)
import SymbolTable
import TypeSystem

type TypedProgram = Program BasicType

type TypedLine = Line BasicType

type TypedStmt = Stmt BasicType

type TypedExpr = Expr BasicType

type TypedExprInner = ExprInner BasicType

type TypedAssignment = Assignment BasicType

type TypedLValue = LValue BasicType

type TypedFunction = Function BasicType

data SemanticAnalysisState where
  SemanticAnalysisState ::
    { symbolTable :: SymbolTable
    } ->
    SemanticAnalysisState
  deriving (Show, Eq)

emptySemanticAnalysisState :: SemanticAnalysisState
emptySemanticAnalysisState =
  SemanticAnalysisState {symbolTable = emptySymbolTable}

insertVariableInState :: Ident -> BasicType -> SemanticAnalysisState -> SemanticAnalysisState
insertVariableInState sym ty (SemanticAnalysisState symTable) =
  SemanticAnalysisState {symbolTable = insertVariable sym ty symTable}

lookupSymbolInState :: Ident -> SemanticAnalysisState -> Variable
lookupSymbolInState name (SemanticAnalysisState symTable) =
  case lookupSymbol name symTable of
    Just sym -> sym
    Nothing -> error $ "Symbol not found: " ++ show name

analyzeIdentAndInsertIntoTable :: Ident -> State SemanticAnalysisState BasicType
analyzeIdentAndInsertIntoTable id'@Ident {identHasDollar} =
  let ty = if identHasDollar then BasicStringType else BasicNumericType
   in do
        modify (insertVariableInState id' ty)
        return ty

analyzePsuedoVar :: PseudoVariable -> State SemanticAnalysisState BasicType
analyzePsuedoVar TimePseudoVar = return BasicNumericType
analyzePsuedoVar InkeyPseudoVar = return BasicStringType

analyzeLValue :: RawLValue -> State SemanticAnalysisState (TypedLValue, BasicType)
analyzeLValue (LValueIdent ident) = do
  ty <- analyzeIdentAndInsertIntoTable ident
  return (LValueIdent ident, ty)
analyzeLValue (LValueArrayAccess ident expr) = do
  exprType' <- analyzeExpr expr
  case Ast.Types.exprType exprType' of
    BasicNumericType -> do
      -- Arrays must be declared before use with the DIM statement
      symbol <- gets (lookupSymbolInState ident)
      let ty = SymbolTable.variableType symbol
      case ty of
        BasicNumArrType {numericArrSize} ->
          if numericArrSize >= 0
            then return (LValueArrayAccess {lValueArrayIdent = ident, lValueArrayIndex = exprType'}, BasicNumericType)
            else error $ "Array " ++ show ident ++ " has invalid size: " ++ show numericArrSize
        BasicStrArrType {strArrLength, strArrSize} ->
          if strArrSize >= 0 && strArrLength >= 0
            then return (LValueArrayAccess {lValueArrayIdent = ident, lValueArrayIndex = exprType'}, BasicStringType)
            else
              error $
                "String array "
                  ++ show ident
                  ++ " has invalid size or length: "
                  ++ show strArrSize
                  ++ ", "
                  ++ show strArrLength
        t -> error $ "Array " ++ show ident ++ " is not a numeric or string array: " ++ show t
    _ -> error "Array index must be numeric"
analyzeLValue (LValuePseudoVar pseudoVar) = do
  ty <- analyzePsuedoVar pseudoVar
  return (LValuePseudoVar pseudoVar, ty)
analyzeLValue (LValueFixedMemoryAreaVar name hasDollar) =
  let ty = if hasDollar then BasicStringType else BasicNumericType
   in return (LValueFixedMemoryAreaVar {lValueFixedMemoryAreaVarName = name, lValueFixedMemoryAreaHasDollar = hasDollar}, ty)

analyzeStrVariableOrLiteral :: StringVariableOrLiteral -> State SemanticAnalysisState BasicType
analyzeStrVariableOrLiteral (StringLiteral _) = return BasicStringType
analyzeStrVariableOrLiteral (StringVariable ident) = do
  analyzedId <- analyzeIdentAndInsertIntoTable ident
  case analyzedId of
    BasicStringType -> return BasicStringType
    BasicNumericType -> error "String variable expected, but numeric variable found"
    _ -> error "Unexpected type for string variable"

analyzeFunction :: RawFunction -> State SemanticAnalysisState (TypedFunction, BasicType)
analyzeFunction ident = case ident of
  MidFun {midFunStringExpr, midFunStartExpr, midFunLengthExpr} -> do
    strType <- analyzeStrVariableOrLiteral midFunStringExpr
    startType <- analyzeExpr midFunStartExpr
    lengthType <- analyzeExpr midFunLengthExpr
    if strType == BasicStringType && Ast.Types.exprType startType == BasicNumericType && Ast.Types.exprType lengthType == BasicNumericType
      then
        return
          ( MidFun
              { midFunStringExpr,
                midFunStartExpr = startType,
                midFunLengthExpr = lengthType
              },
            BasicStringType
          )
      else error "MID$ function requires a string and numeric expressions for start and length"
  LeftFun {leftFunStringExpr, leftFunLengthExpr} -> do
    strType <- analyzeStrVariableOrLiteral leftFunStringExpr
    lengthType <- analyzeExpr leftFunLengthExpr
    if strType == BasicStringType && Ast.Types.exprType lengthType == BasicNumericType
      then
        return
          ( LeftFun {leftFunStringExpr, leftFunLengthExpr = lengthType},
            BasicStringType
          )
      else error "LEFT$ function requires a string and a numeric expression for length"
  RightFun {rightFunStringExpr, rightFunLengthExpr} -> do
    strType <- analyzeStrVariableOrLiteral rightFunStringExpr
    lengthType <- analyzeExpr rightFunLengthExpr
    if strType == BasicStringType && Ast.Types.exprType lengthType == BasicNumericType
      then
        return
          ( RightFun {rightFunStringExpr, rightFunLengthExpr = lengthType},
            BasicStringType
          )
      else error "RIGHT$ function requires a string and a numeric expression for length"
  AsciiFun {asciiFunArgument} -> do
    exprType <- analyzeExpr asciiFunArgument
    if Ast.Types.exprType exprType == BasicStringType
      then return (AsciiFun {asciiFunArgument = exprType}, BasicNumericType)
      else error "ASC function requires a string argument"
  PointFun {pointFunPositionExpr} -> do
    posType <- analyzeExpr pointFunPositionExpr
    if Ast.Types.exprType posType == BasicNumericType
      then return (PointFun {pointFunPositionExpr = posType}, BasicNumericType)
      else error "POINT function requires a numeric expression for position"
  RndFun {rndRangeEnd} -> do
    return (RndFun {rndRangeEnd}, BasicNumericType)
  IntFun {intFunExpr} -> do
    exprType <- analyzeExpr intFunExpr
    if Ast.Types.exprType exprType == BasicNumericType
      then return (IntFun {intFunExpr = exprType}, BasicNumericType)
      else error "INT function requires a numeric expression"
  SgnFun {sgnFunExpr} -> do
    exprType <- analyzeExpr sgnFunExpr
    if Ast.Types.exprType exprType == BasicNumericType
      then return (SgnFun {sgnFunExpr = exprType}, BasicNumericType)
      else error "SGN function requires a numeric expression"
  StatusFun {statusFunArg} ->
    return (StatusFun {statusFunArg}, BasicNumericType)
  ValFun {valFunExpr} -> do
    exprType <- analyzeExpr valFunExpr

    if Ast.Types.exprType exprType == BasicStringType
      then return (ValFun {valFunExpr = exprType}, BasicNumericType)
      else error "VAL function requires a string expression"
  StrFun {strFunExpr} -> do
    exprType <- analyzeExpr strFunExpr
    if Ast.Types.exprType exprType == BasicNumericType
      then return (StrFun {strFunExpr = exprType}, BasicStringType)
      else error "STR$ function requires a numeric expression"

analyzeExprInner :: RawExprInner -> State SemanticAnalysisState (TypedExprInner, BasicType)
analyzeExprInner (DecNumLitExpr num) = return (DecNumLitExpr num, BasicNumericType)
analyzeExprInner (HexNumLitExpr num) = return (HexNumLitExpr num, BasicNumericType)
analyzeExprInner (StrLitExpr str) = return (StrLitExpr str, BasicStringType)
analyzeExprInner (LValueExpr lValue) = do
  (typedLValue, ty) <- analyzeLValue lValue
  return (LValueExpr typedLValue, ty)
analyzeExprInner (UnaryExpr op expr) = do
  analyzedExpr <- analyzeExpr expr
  let exprType = Ast.Types.exprType analyzedExpr
  case op of
    UnaryMinusOp ->
      if exprType == BasicNumericType
        then return (UnaryExpr UnaryMinusOp analyzedExpr, BasicNumericType)
        else error "Negation can only be applied to numeric expressions"
    UnaryPlusOp ->
      if exprType == BasicNumericType
        then return (UnaryExpr UnaryPlusOp analyzedExpr, BasicNumericType)
        else error "Unary plus can only be applied to numeric expressions"
    UnaryNotOp ->
      if exprType == BasicNumericType
        then return (UnaryExpr UnaryNotOp analyzedExpr, BasicStringType)
        else error "Not can only be applied to numeric expressions"
analyzeExprInner (BinExpr left op right) = do
  analyzedLeft <- analyzeExpr left
  analyzedRight <- analyzeExpr right
  let leftType = Ast.Types.exprType analyzedLeft
      rightType = Ast.Types.exprType analyzedRight
  case op of
    -- Arithmetic operations
    AddOp ->
      if leftType == BasicNumericType && rightType == BasicNumericType
        then return (BinExpr analyzedLeft AddOp analyzedRight, BasicNumericType)
        else
          -- if leftType == BasicStringType && rightType == BasicStringType
          --   then return (BinExpr analyzedLeft AddOp analyzedRight, BasicStringType)
          --   else error "Addition can only be applied to numeric or string expressions"
          error "Unimplemented string concatenation"
    SubtractOp ->
      if leftType == BasicNumericType && rightType == BasicNumericType
        then return (BinExpr analyzedLeft SubtractOp analyzedRight, BasicNumericType)
        else error "Subtraction can only be applied to numeric expressions"
    MultiplyOp ->
      if leftType == BasicNumericType && rightType == BasicNumericType
        then return (BinExpr analyzedLeft MultiplyOp analyzedRight, BasicNumericType)
        else error "Multiplication can only be applied to numeric expressions"
    DivideOp ->
      if leftType == BasicNumericType && rightType == BasicNumericType
        then return (BinExpr analyzedLeft DivideOp analyzedRight, BasicNumericType)
        else error "Division can only be applied to numeric expressions"
    CaretOp ->
      if leftType == BasicNumericType && rightType == BasicNumericType
        then return (BinExpr analyzedLeft CaretOp analyzedRight, BasicNumericType)
        else error "Exponentiation can only be applied to numeric expressions"
    -- Comparison operations
    EqualOp ->
      if leftType == rightType
        then return (BinExpr analyzedLeft EqualOp analyzedRight, BasicNumericType)
        else error "Equality can only be applied to expressions of the same type"
    LessThanOp ->
      if leftType == rightType
        then return (BinExpr analyzedLeft LessThanOp analyzedRight, BasicNumericType)
        else error "Less than can only be applied to expressions of the same type"
    GreaterThanOp ->
      if leftType == rightType
        then return (BinExpr analyzedLeft GreaterThanOp analyzedRight, BasicNumericType)
        else error "Greater than can only be applied to expressions of the same type"
    LessThanOrEqualOp ->
      if leftType == BasicNumericType && rightType == BasicNumericType
        then return (BinExpr analyzedLeft LessThanOrEqualOp analyzedRight, BasicNumericType)
        else error "Less than or equal can only be applied to numeric expressions"
    GreaterThanOrEqualOp ->
      if leftType == rightType
        then return (BinExpr analyzedLeft GreaterThanOrEqualOp analyzedRight, BasicNumericType)
        else error "Greater than or equal can only be applied to expressions of the same type"
    NotEqualOp ->
      if leftType == rightType
        then return (BinExpr analyzedLeft NotEqualOp analyzedRight, BasicNumericType)
        else error "Not equal can only be applied to expressions of the same type"
    -- Logical operations
    AndOp ->
      if leftType == BasicNumericType && rightType == BasicNumericType
        then return (BinExpr analyzedLeft AndOp analyzedRight, BasicNumericType)
        else error "And can only be applied to numeric (bool) expressions"
    OrOp ->
      if leftType == BasicNumericType && rightType == BasicNumericType
        then return (BinExpr analyzedLeft OrOp analyzedRight, BasicNumericType)
        else error "Or can only be applied to numeric (bTypedExprool) expressions"
analyzeExprInner (FunCallExpr ident) = do
  (analyzedFunction, ty) <- analyzeFunction ident
  return (FunCallExpr analyzedFunction, ty)

analyzeExpr :: RawExpr -> State SemanticAnalysisState TypedExpr
analyzeExpr Expr {exprInner} = do
  (analyzedInner, innerType) <- analyzeExprInner exprInner
  return Expr {exprInner = analyzedInner, Ast.Types.exprType = innerType}

-- We have to add the arrays to the symbol table
analyzeDimAndInsertIntoTable :: DimInner -> State SemanticAnalysisState BasicType
analyzeDimAndInsertIntoTable (DimInner {dimIdent, dimSize, dimStringLength}) = do
  case identHasDollar dimIdent of
    False -> do
      let exprType = BasicNumArrType {numericArrSize = dimSize}

      when (isJust dimStringLength) $
        error "Numeric arrays cannot have a string length"

      modify (insertVariableInState dimIdent exprType)
      return exprType
    True -> do
      let concreteLength = fromMaybe defaultStringLength dimStringLength
          exprType = BasicStrArrType {strArrSize = dimSize, strArrLength = concreteLength}
      modify (insertVariableInState dimIdent exprType)
      return exprType

analyzeAssignment :: RawAssignment -> State SemanticAnalysisState TypedAssignment
analyzeAssignment (Assignment lValue expr _) = do
  (typedLValue, lValueType) <- analyzeLValue lValue
  analyzedExpr <- analyzeExpr expr
  let exprTy = Ast.Types.exprType analyzedExpr
  if lValueType == exprTy
    then
      return
        ( Assignment
            { assignmentLValue = typedLValue,
              assignmentExpr = analyzedExpr,
              assignmentType = exprTy
            }
        )
    else error $ "Type mismatch in assignment: " ++ show lValueType ++ " vs " ++ show (Ast.Types.exprType analyzedExpr)

analyzeBeepOptionalParams :: BeepOptionalParams () -> State SemanticAnalysisState (BeepOptionalParams BasicType)
analyzeBeepOptionalParams (BeepOptionalParams frequency duration) = do
  analyzedFrequency <- analyzeExpr frequency
  analyzedDuration <- analyzeExpr duration
  if Ast.Types.exprType analyzedFrequency == BasicNumericType && Ast.Types.exprType analyzedDuration == BasicNumericType
    then return (BeepOptionalParams {beepFrequency = analyzedFrequency, beepDuration = analyzedDuration})
    else error "Beep optional parameters must be numeric expressions"

analyzeStmt :: RawStmt -> State SemanticAnalysisState TypedStmt
analyzeStmt (LetStmt assignments) = do
  analyzedAssignments <- mapM analyzeAssignment assignments
  return (LetStmt analyzedAssignments)
analyzeStmt (IfThenStmt condition thenStmt) = do
  conditionType <- analyzeExpr condition

  when (Ast.Types.exprType conditionType /= BasicNumericType) $
    error "Condition in IF statement must be numeric"

  thenStmt' <- analyzeStmt thenStmt
  return (IfThenStmt conditionType thenStmt')
analyzeStmt (PrintStmt printCommaFormat) = do
  case printCommaFormat of
    PrintCommaFormat {printCommaFormatExpr1, printCommaFormatExpr2} -> do
      expr1Type <- analyzeExpr printCommaFormatExpr1
      expr2Type <- analyzeExpr printCommaFormatExpr2

      -- FIXME: this form should be made up of a tring and a numeric expression, but check

      return
        ( PrintStmt
            { printCommaFormat = PrintCommaFormat {printCommaFormatExpr1 = expr1Type, printCommaFormatExpr2 = expr2Type}
            }
        )
    PrintSemicolonFormat {printSemicolonFormatUsingClause, printSemicolonFormatExprs, printSemicolonFormatEnding} -> do
      analyzedExprs <- mapM analyzeExpr printSemicolonFormatExprs

      return
        ( PrintStmt
            { printCommaFormat =
                PrintSemicolonFormat
                  { printSemicolonFormatUsingClause,
                    printSemicolonFormatExprs = analyzedExprs,
                    printSemicolonFormatEnding = printSemicolonFormatEnding
                  }
            }
        )
analyzeStmt (PauseStmt pauseCommaFormat) = do
  case pauseCommaFormat of
    PrintCommaFormat {printCommaFormatExpr1, printCommaFormatExpr2} -> do
      expr1Type <- analyzeExpr printCommaFormatExpr1
      expr2Type <- analyzeExpr printCommaFormatExpr2

      return
        ( PauseStmt
            { pauseCommaFormat = PrintCommaFormat {printCommaFormatExpr1 = expr1Type, printCommaFormatExpr2 = expr2Type}
            }
        )
    PrintSemicolonFormat {printSemicolonFormatUsingClause, printSemicolonFormatExprs, printSemicolonFormatEnding} -> do
      analyzedExprs <- mapM analyzeExpr printSemicolonFormatExprs

      return
        ( PauseStmt
            { pauseCommaFormat =
                PrintSemicolonFormat
                  { printSemicolonFormatUsingClause,
                    printSemicolonFormatExprs = analyzedExprs,
                    printSemicolonFormatEnding = printSemicolonFormatEnding
                  }
            }
        )
analyzeStmt (UsingStmt u) = return (UsingStmt u)
analyzeStmt (InputStmt inputPrintExpr inputDestination) = do
  -- add string literals to the symbol table
  case inputPrintExpr of
    Just s -> void $ analyzeStrVariableOrLiteral (StringLiteral s)
    Nothing -> return ()
  (analyzedLValue, _) <- analyzeLValue inputDestination
  return (InputStmt {inputPrintExpr = inputPrintExpr, inputDestination = analyzedLValue})
analyzeStmt EndStmt = return EndStmt
analyzeStmt Comment = return Comment
analyzeStmt (ForStmt forAssignment forToExpr stepExpr) = do
  forType <- analyzeAssignment forAssignment
  toType <- analyzeExpr forToExpr

  when (assignmentType forType /= BasicNumericType || Ast.Types.exprType toType /= BasicNumericType) $
    error "For statement requires numeric expressions for assignment and to"

  case stepExpr of
    Just step -> do
      stepType <- analyzeExpr step
      when (Ast.Types.exprType stepType /= BasicNumericType) $
        error "For statement requires a numeric expression for step"
      return (ForStmt {forAssignment = forType, forToExpr = toType, forStepExpr = Just stepType})
    Nothing -> return (ForStmt {forAssignment = forType, forToExpr = toType, forStepExpr = Nothing})
analyzeStmt (NextStmt nextIdent) = do
  let nextType = BasicNumericType -- Next always refers to a numeric identifier
  modify (insertVariableInState nextIdent nextType)
  return NextStmt {nextIdent = nextIdent}
analyzeStmt ClearStmt = return ClearStmt
analyzeStmt (GoToStmt gotoTarget) = do
  exprType <- analyzeExpr gotoTarget

  return (GoToStmt {gotoTarget = exprType})
analyzeStmt (GoSubStmt gosubTarget) = do
  exprType <- analyzeExpr gosubTarget

  return (GoSubStmt {gosubTarget = exprType})
analyzeStmt (WaitStmt waitForExpr) = do
  case waitForExpr of
    Just expr -> do
      exprType <- analyzeExpr expr
      if Ast.Types.exprType exprType == BasicNumericType
        then return WaitStmt {waitForExpr = Just exprType}
        else error "Wait statement requires a numeric expression"
    Nothing -> return WaitStmt {waitForExpr = Nothing} -- No expression means wait indefinitely
analyzeStmt ClsStmt = return ClsStmt
analyzeStmt RandomStmt = return RandomStmt
analyzeStmt (GprintStmt gprintExprs gprintSeparator) = do
  analyzedExprs <- mapM analyzeExpr gprintExprs

  return (GprintStmt {gprintExprs = analyzedExprs, gprintSeparator = gprintSeparator})
analyzeStmt (GCursorStmt gCursorExpr) = do
  gCursorType <- analyzeExpr gCursorExpr

  when (Ast.Types.exprType gCursorType /= BasicNumericType) $
    error "GCursor statement requires a numeric expression"

  return (GCursorStmt {gCursorExpr = gCursorType})
analyzeStmt (CursorStmt cursorExpr) = do
  cursorType <- analyzeExpr cursorExpr

  when (Ast.Types.exprType cursorType /= BasicNumericType) $
    error "Cursor statement requires a numeric expression"

  return (CursorStmt {cursorExpr = cursorType})
analyzeStmt (BeepStmt repetitionEcpr optionalParams) = do
  beepType <- analyzeExpr repetitionEcpr

  when (Ast.Types.exprType beepType /= BasicNumericType) $
    error "Beep statement requires a numeric expression for repetitions"

  analyzedOptionalParams <- case optionalParams of
    Just params -> Just <$> analyzeBeepOptionalParams params
    Nothing -> return Nothing

  return (BeepStmt {beepStmtRepetitionsExpr = beepType, beepStmtOptionalParams = analyzedOptionalParams})
analyzeStmt ReturnStmt = return ReturnStmt
analyzeStmt (PokeStmt pokeKind pokeExprs) = do
  exprTypes <- mapM analyzeExpr pokeExprs

  unless (all (\et -> Ast.Types.exprType et == BasicNumericType) exprTypes) $
    error "Poke statement requires numeric expressions"

  return (PokeStmt {pokeKind = pokeKind, pokeExprs = exprTypes})
analyzeStmt (DimStmt dimKind) = do
  _ <- analyzeDimAndInsertIntoTable dimKind
  return (DimStmt dimKind)
-- FIXME: check that data, read and restore are typed correctly
analyzeStmt (ReadStmt readStmtDestinations) = do
  analyzedLvalues <- mapM analyzeLValue readStmtDestinations
  return (ReadStmt {readStmtDestinations = fst <$> analyzedLvalues})
analyzeStmt (DataStmt exprs) = do
  analyzedExprs <- mapM analyzeExpr exprs
  return (DataStmt analyzedExprs)
analyzeStmt (RestoreStmt restoreLineOrLabelExpr) = do
  expr <- analyzeExpr restoreLineOrLabelExpr
  return (RestoreStmt {restoreLineOrLabelExpr = expr})
analyzeStmt ArunStmt = return ArunStmt
analyzeStmt LockStmt = return LockStmt
analyzeStmt UnlockStmt = return UnlockStmt
analyzeStmt (OnGoToStmt onGotoExpr onGotoTargets) = do
  exprType <- analyzeExpr onGotoExpr

  when (Ast.Types.exprType exprType /= BasicNumericType) $
    error "On Goto statement requires a numeric expression"

  return (OnGoToStmt {onGotoExpr = exprType, onGotoTargets = onGotoTargets})
analyzeStmt (OnGoSubStmt onGoSubExpr onGoSubTargets) = do
  exprType <- analyzeExpr onGoSubExpr
  when (Ast.Types.exprType exprType /= BasicNumericType) $
    error "On GoSub statement requires a numeric expression"

  return (OnGoSubStmt {onGosubExpr = exprType, onGosubTargets = onGoSubTargets})
analyzeStmt (CallStmt callExpr) = do
  analyzedCallExpr <- analyzeExpr callExpr
  case Ast.Types.exprType analyzedCallExpr of
    BasicNumericType -> return (CallStmt {callExpression = analyzedCallExpr})
    _ -> error "Call statement requires a numeric expression"
analyzeStmt (BeepOnOffStmt beepOn) = return (BeepOnOffStmt {beepOn = beepOn})

analyzeLine :: RawLine -> State SemanticAnalysisState TypedLine
analyzeLine (Line lineNumber lineLabel lineStmts) = do
  case lineLabel of
    Just label -> do
      -- Insert the label into the symbol table with the current line number
      modify (\s -> s {symbolTable = insertLabel label lineNumber (symbolTable s)})
    Nothing -> return ()

  analyzedStmts <- mapM analyzeStmt lineStmts
  return (Line {lineNumber = lineNumber, lineLabel = lineLabel, lineStmts = analyzedStmts})

analyzeProgram' :: RawProgram -> State SemanticAnalysisState TypedProgram
analyzeProgram' (Program lines') = do
  analyzedLines <- mapM analyzeLine lines'
  return (Program {programLines = analyzedLines})

analyzeProgram :: RawProgram -> (TypedProgram, SymbolTable)
analyzeProgram prog =
  let (analyzedProg, finalState) = runState (analyzeProgram' prog) emptySemanticAnalysisState
   in (analyzedProg, symbolTable finalState)
