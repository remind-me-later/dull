module Ast.SemanticAnalysis
  ( analyzeProgram,
  )
where

import Ast.Types
import Control.Monad (unless, when)
import Control.Monad.State
import Data.Maybe (isJust)
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
analyzeLValue (LValueArrayAccess ident index) = do
  indexType' <- analyzeExpr index
  case Ast.Types.exprType indexType' of
    BasicNumericType -> do
      case identHasDollar ident of
        False ->
          return (LValueArrayAccess {lValueArrayIdent = ident, lValueArrayIndex = indexType'}, BasicNumericType)
        True ->
          return (LValueArrayAccess {lValueArrayIdent = ident, lValueArrayIndex = indexType'}, BasicStringType)
    _ -> error "Array index must be numeric"
analyzeLValue (LValue2DArrayAccess ident rowIndex colIndex) = do
  rowIndexType <- analyzeExpr rowIndex
  colIndexType <- analyzeExpr colIndex
  case (Ast.Types.exprType rowIndexType, Ast.Types.exprType colIndexType) of
    (BasicNumericType, BasicNumericType) -> do
      case identHasDollar ident of
        False ->
          return (LValue2DArrayAccess {lValue2DArrayIdent = ident, lValue2DArrayRowIndex = rowIndexType, lValue2DArrayColIndex = colIndexType}, BasicNumericType)
        True ->
          return (LValue2DArrayAccess {lValue2DArrayIdent = ident, lValue2DArrayRowIndex = rowIndexType, lValue2DArrayColIndex = colIndexType}, BasicStringType)
    _ -> error "Row and column indices must be numeric"
analyzeLValue (LValuePseudoVar pseudoVar) = do
  ty <- analyzePsuedoVar pseudoVar
  return (LValuePseudoVar pseudoVar, ty)
analyzeLValue (LValueFixedMemoryAreaVar indexExpr hasDollar) = do
  let ty = if hasDollar then BasicStringType else BasicNumericType
  indexType <- analyzeExpr indexExpr
  case Ast.Types.exprType indexType of
    BasicNumericType ->
      return
        ( LValueFixedMemoryAreaVar {lValueFixedMemoryAreaIndex = indexType, lValueFixedMemoryAreaHasDollar = hasDollar},
          ty
        )
    _ -> error "Fixed memory area index must be numeric"

analyzeFunction :: RawFunction -> State SemanticAnalysisState (TypedFunction, BasicType)
analyzeFunction ident = case ident of
  MidFun {midFunStringExpr, midFunStartExpr, midFunLengthExpr} -> do
    strType <- analyzeExpr midFunStringExpr
    startType <- analyzeExpr midFunStartExpr
    lengthType <- analyzeExpr midFunLengthExpr
    if Ast.Types.exprType strType == BasicStringType && Ast.Types.exprType startType == BasicNumericType && Ast.Types.exprType lengthType == BasicNumericType
      then
        return
          ( MidFun
              { midFunStringExpr = strType,
                midFunStartExpr = startType,
                midFunLengthExpr = lengthType
              },
            BasicStringType
          )
      else error "MID$ function requires a string and numeric expressions for start and length"
  LeftFun {leftFunStringExpr, leftFunLengthExpr} -> do
    strType <- analyzeExpr leftFunStringExpr
    lengthType <- analyzeExpr leftFunLengthExpr
    if Ast.Types.exprType strType == BasicStringType && Ast.Types.exprType lengthType == BasicNumericType
      then
        return
          ( LeftFun {leftFunStringExpr = strType, leftFunLengthExpr = lengthType},
            BasicStringType
          )
      else error "LEFT$ function requires a string and a numeric expression for length"
  RightFun {rightFunStringExpr, rightFunLengthExpr} -> do
    strType <- analyzeExpr rightFunStringExpr
    lengthType <- analyzeExpr rightFunLengthExpr
    if Ast.Types.exprType strType == BasicStringType && Ast.Types.exprType lengthType == BasicNumericType
      then
        return
          ( RightFun {rightFunStringExpr = strType, rightFunLengthExpr = lengthType},
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
    rangeEndType <- analyzeExpr rndRangeEnd
    if Ast.Types.exprType rangeEndType == BasicNumericType
      then return (RndFun {rndRangeEnd = rangeEndType}, BasicNumericType)
      else error "RND function requires a numeric expression for range end"
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
  StatusFun {statusFunArg} -> do
    argType <- analyzeExpr statusFunArg

    when (Ast.Types.exprType argType /= BasicNumericType) $
      error "STATUS function requires a numeric expression"

    return (StatusFun {statusFunArg = argType}, BasicNumericType)
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
  ChrFun {chrFunExpr} -> do
    exprType <- analyzeExpr chrFunExpr
    if Ast.Types.exprType exprType == BasicNumericType
      then return (ChrFun {chrFunExpr = exprType}, BasicStringType)
      else error "CHR$ function requires a numeric expression"
  AbsFun {absFunExpr} -> do
    exprType <- analyzeExpr absFunExpr
    if Ast.Types.exprType exprType == BasicNumericType
      then return (AbsFun {absFunExpr = exprType}, BasicNumericType)
      else error "ABS function requires a numeric expression"
  LenFun {lenFunExpr} -> do
    exprType <- analyzeExpr lenFunExpr
    if Ast.Types.exprType exprType == BasicStringType
      then return (LenFun {lenFunExpr = exprType}, BasicNumericType)
      else error "LEN function requires a string expression"
  PeekFun {peekMemoryArea, peekFunAddress} -> do
    addressType <- analyzeExpr peekFunAddress
    if Ast.Types.exprType addressType == BasicNumericType
      then return (PeekFun {peekMemoryArea = peekMemoryArea, peekFunAddress = addressType}, BasicNumericType)
      else error "PEEK function requires a numeric expression for address"
  LnFun {lnFunExpr} -> do
    exprType <- analyzeExpr lnFunExpr
    if Ast.Types.exprType exprType == BasicNumericType
      then return (LnFun {lnFunExpr = exprType}, BasicNumericType)
      else error "LN function requires a numeric expression"
  LogFun {logFunExpr} -> do
    exprType <- analyzeExpr logFunExpr
    if Ast.Types.exprType exprType == BasicNumericType
      then return (LogFun {logFunExpr = exprType}, BasicNumericType)
      else error "LOG function requires a numeric expression"
  DmsFun {dmsFunExpr} -> do
    exprType <- analyzeExpr dmsFunExpr
    if Ast.Types.exprType exprType == BasicNumericType
      then return (DmsFun {dmsFunExpr = exprType}, BasicNumericType)
      else error "DMS function requires a numeric expression"
  DegFun {degFunExpr} -> do
    exprType <- analyzeExpr degFunExpr
    if Ast.Types.exprType exprType == BasicNumericType
      then return (DegFun {degFunExpr = exprType}, BasicNumericType)
      else error "DEG function requires a numeric expression"
  TanFun {tanFunExpr} -> do
    exprType <- analyzeExpr tanFunExpr
    if Ast.Types.exprType exprType == BasicNumericType
      then return (TanFun {tanFunExpr = exprType}, BasicNumericType)
      else error "TAN function requires a numeric expression"
  CosFun {cosFunExpr} -> do
    exprType <- analyzeExpr cosFunExpr
    if Ast.Types.exprType exprType == BasicNumericType
      then return (CosFun {cosFunExpr = exprType}, BasicNumericType)
      else error "COS function requires a numeric expression"
  SinFun {sinFunExpr} -> do
    exprType <- analyzeExpr sinFunExpr
    if Ast.Types.exprType exprType == BasicNumericType
      then return (SinFun {sinFunExpr = exprType}, BasicNumericType)
      else error "SIN function requires a numeric expression"
  SqrtFun {sqrtFunExpr} -> do
    exprType <- analyzeExpr sqrtFunExpr
    if Ast.Types.exprType exprType == BasicNumericType
      then return (SqrtFun {sqrtFunExpr = exprType}, BasicNumericType)
      else error "√ function requires a numeric expression"

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
          if leftType == BasicStringType && rightType == BasicStringType
            then return (BinExpr analyzedLeft AddOp analyzedRight, BasicStringType)
            else error "Addition can only be applied to numeric or string expressions"
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
      if leftType == rightType
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
analyzeDimAndInsertIntoTable :: DimInner () -> State SemanticAnalysisState (DimInner BasicType)
analyzeDimAndInsertIntoTable (DimInner1D {dimIdent, dimSize, dimStringLength}) = do
  typedDimSize <- analyzeExpr dimSize

  when (Ast.Types.exprType typedDimSize /= BasicNumericType) $
    error "Array size must be numeric"

  typedDimStringLength <- case dimStringLength of
    Just strLen -> do
      analyzedStrLen <- analyzeExpr strLen
      if Ast.Types.exprType analyzedStrLen == BasicNumericType
        then return (Just analyzedStrLen)
        else error "String length must be numeric"
    Nothing -> return Nothing

  case identHasDollar dimIdent of
    False -> do
      when (isJust dimStringLength) $
        error "Numeric arrays cannot have a string length"

      modify (insertVariableInState dimIdent BasicNumArrType)
    True -> modify (insertVariableInState dimIdent BasicStrArrType)

  return (DimInner1D {dimIdent, dimSize = typedDimSize, dimStringLength = typedDimStringLength})
analyzeDimAndInsertIntoTable (DimInner2D {dimIdent, dimRows, dimCols, dimStringLength}) = do
  typedDimRows <- analyzeExpr dimRows

  typedDimCols <- analyzeExpr dimCols

  when (Ast.Types.exprType typedDimRows /= BasicNumericType) $
    error "Array row count must be numeric"
  when (Ast.Types.exprType typedDimCols /= BasicNumericType) $
    error "Array column count must be numeric"

  typedDimStringLength <- case dimStringLength of
    Just strLen -> do
      analyzedStrLen <- analyzeExpr strLen
      if Ast.Types.exprType analyzedStrLen == BasicNumericType
        then return (Just analyzedStrLen)
        else error "String length must be numeric"
    Nothing -> return Nothing

  case identHasDollar dimIdent of
    False -> do
      when (isJust dimStringLength) $
        error "Numeric 2D arrays cannot have a string length"

      modify (insertVariableInState dimIdent BasicNum2DArrType)
    True -> modify (insertVariableInState dimIdent BasicStr2DArrType)

  return (DimInner2D {dimIdent, dimRows = typedDimRows, dimCols = typedDimCols, dimStringLength = typedDimStringLength})

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
  case duration of
    Just dur -> do
      analyzedDuration <- analyzeExpr dur
      if Ast.Types.exprType analyzedFrequency == BasicNumericType && Ast.Types.exprType analyzedDuration == BasicNumericType
        then return (BeepOptionalParams {beepFrequency = analyzedFrequency, beepDuration = Just analyzedDuration})
        else error "Beep optional parameters must be numeric expressions"
    Nothing -> return (BeepOptionalParams {beepFrequency = analyzedFrequency, beepDuration = Nothing})

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
analyzeStmt (PrintStmt printCommaFormat) = case printCommaFormat of
  Nothing -> return (PrintStmt Nothing)
  Just commaFormat -> do
    case commaFormat of
      PrintCommaFormat {printCommaFormatExpr1, printCommaFormatExpr2} -> do
        expr1Type <- analyzeExpr printCommaFormatExpr1
        expr2Type <- analyzeExpr printCommaFormatExpr2

        -- FIXME: this form should be made up of a tring and a numeric expression, but check

        return
          ( PrintStmt
              { printCommaFormat = Just PrintCommaFormat {printCommaFormatExpr1 = expr1Type, printCommaFormatExpr2 = expr2Type}
              }
          )
      PrintSemicolonFormat {printSemicolonFormatUsingClause, printSemicolonFormatExprs, printSemicolonFormatEnding} -> do
        analyzedExprs <- mapM analyzeExpr printSemicolonFormatExprs

        return
          ( PrintStmt
              { printCommaFormat =
                  Just
                    PrintSemicolonFormat
                      { printSemicolonFormatUsingClause,
                        printSemicolonFormatExprs = analyzedExprs,
                        printSemicolonFormatEnding = printSemicolonFormatEnding
                      }
              }
          )
analyzeStmt (PauseStmt pauseCommaFormat) = case pauseCommaFormat of
  Nothing -> return (PauseStmt Nothing)
  Just commaFormat -> do
    case commaFormat of
      PrintCommaFormat {printCommaFormatExpr1, printCommaFormatExpr2} -> do
        expr1Type <- analyzeExpr printCommaFormatExpr1
        expr2Type <- analyzeExpr printCommaFormatExpr2

        return
          ( PauseStmt
              { pauseCommaFormat = Just PrintCommaFormat {printCommaFormatExpr1 = expr1Type, printCommaFormatExpr2 = expr2Type}
              }
          )
      PrintSemicolonFormat {printSemicolonFormatUsingClause, printSemicolonFormatExprs, printSemicolonFormatEnding} -> do
        analyzedExprs <- mapM analyzeExpr printSemicolonFormatExprs

        return
          ( PauseStmt
              { pauseCommaFormat =
                  Just
                    PrintSemicolonFormat
                      { printSemicolonFormatUsingClause,
                        printSemicolonFormatExprs = analyzedExprs,
                        printSemicolonFormatEnding = printSemicolonFormatEnding
                      }
              }
          )
analyzeStmt (LPrintStmt maybeCommaFormat) = case maybeCommaFormat of
  Just commaFormat -> do
    case commaFormat of
      PrintCommaFormat {printCommaFormatExpr1, printCommaFormatExpr2} -> do
        expr1Type <- analyzeExpr printCommaFormatExpr1
        expr2Type <- analyzeExpr printCommaFormatExpr2
        return
          ( LPrintStmt
              { lprintCommaFormat =
                  Just
                    ( PrintCommaFormat
                        { printCommaFormatExpr1 = expr1Type,
                          printCommaFormatExpr2 = expr2Type
                        }
                    )
              }
          )
      PrintSemicolonFormat
        { printSemicolonFormatUsingClause,
          printSemicolonFormatExprs,
          printSemicolonFormatEnding
        } -> do
          analyzedExprs <- mapM analyzeExpr printSemicolonFormatExprs
          return
            ( LPrintStmt
                { lprintCommaFormat =
                    Just
                      ( PrintSemicolonFormat
                          { printSemicolonFormatUsingClause,
                            printSemicolonFormatExprs = analyzedExprs,
                            printSemicolonFormatEnding
                          }
                      )
                }
            )
  Nothing -> return (LPrintStmt {lprintCommaFormat = Nothing})
analyzeStmt (UsingStmt u) = return (UsingStmt u)
analyzeStmt (InputStmt inputPrintExpr inputDestination) = do
  (analyzedLValue, _) <- analyzeLValue inputDestination
  return (InputStmt {inputPrintExpr = inputPrintExpr, inputDestination = analyzedLValue})
analyzeStmt EndStmt = return EndStmt
analyzeStmt Comment {commentText} = return Comment {commentText = commentText}
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
analyzeStmt (WaitStmt waitForExpr) = case waitForExpr of
  Just expr -> do
    exprType <- analyzeExpr expr
    if Ast.Types.exprType exprType == BasicNumericType
      then return WaitStmt {waitForExpr = Just exprType}
      else error "Wait statement requires a numeric expression"
  Nothing -> return WaitStmt {waitForExpr = Nothing} -- No expression means wait indefinitely
analyzeStmt ClsStmt = return ClsStmt
analyzeStmt RandomStmt = return RandomStmt
analyzeStmt (GprintStmt gprintExprs) = do
  analyzedExprs <- mapM (analyzeExpr . fst) gprintExprs
  return (GprintStmt {gprintExprs = zip analyzedExprs (snd <$> gprintExprs)})
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
analyzeStmt (DimStmt decls) = do
  dimDecls' <- mapM analyzeDimAndInsertIntoTable decls
  return (DimStmt {dimDecls = dimDecls'})
-- FIXME: check that data, read and restore are typed correctly
analyzeStmt (ReadStmt readStmtDestinations) = do
  analyzedLvalues <- mapM analyzeLValue readStmtDestinations
  return (ReadStmt {readStmtDestinations = fst <$> analyzedLvalues})
analyzeStmt (DataStmt exprs) = do
  analyzedExprs <- mapM analyzeExpr exprs
  return (DataStmt analyzedExprs)
analyzeStmt (RestoreStmt restoreLineOrLabelExpr) = case restoreLineOrLabelExpr of
  Just expr -> do
    analyzedExpr <- analyzeExpr expr
    return (RestoreStmt {restoreLineOrLabelExpr = Just analyzedExpr})
  Nothing -> return (RestoreStmt {restoreLineOrLabelExpr = Nothing})
analyzeStmt ArunStmt = return ArunStmt
analyzeStmt LockStmt = return LockStmt
analyzeStmt UnlockStmt = return UnlockStmt
analyzeStmt (OnGoToStmt onGotoExpr onGotoTargets) = do
  exprType <- analyzeExpr onGotoExpr

  when (Ast.Types.exprType exprType /= BasicNumericType) $
    error "On Goto statement requires a numeric expression"

  onGotoTargets' <- mapM analyzeExpr onGotoTargets
  return (OnGoToStmt {onGotoExpr = exprType, onGotoTargets = onGotoTargets'})
analyzeStmt (OnGoSubStmt onGoSubExpr onGoSubTargets) = do
  exprType <- analyzeExpr onGoSubExpr

  when (Ast.Types.exprType exprType /= BasicNumericType) $
    error "On GoSub statement requires a numeric expression"

  onGoSubTargets' <- mapM analyzeExpr onGoSubTargets
  return (OnGoSubStmt {onGosubExpr = exprType, onGosubTargets = onGoSubTargets'})
analyzeStmt (OnErrorGotoStmt target) = do
  targetType <- analyzeExpr target
  return (OnErrorGotoStmt {onErrorGotoTarget = targetType})
analyzeStmt (CallStmt callExpr maybeCallVariable) = do
  analyzedCallExpr <- analyzeExpr callExpr
  case Ast.Types.exprType analyzedCallExpr of
    BasicNumericType ->
      case maybeCallVariable of
        Just callVar -> do
          (callVarType, _ty) <- analyzeLValue callVar
          return (CallStmt {callExpression = analyzedCallExpr, maybeCallVariable = Just callVarType})
        Nothing -> return (CallStmt {callExpression = analyzedCallExpr, maybeCallVariable = Nothing})
    _ -> error "Call statement requires a numeric expression"
analyzeStmt (BeepOnOffStmt beepOn) = return (BeepOnOffStmt {beepOn = beepOn})
analyzeStmt TextStmt = return TextStmt
analyzeStmt GraphStmt = return GraphStmt
analyzeStmt (ColorStmt colorExpr) = do
  analyzedColorExpr <- analyzeExpr colorExpr
  case Ast.Types.exprType analyzedColorExpr of
    BasicNumericType -> return (ColorStmt {colorExpr = analyzedColorExpr})
    _ -> error "Color statement requires a numeric expression"
analyzeStmt (CSizeStmt cSizeExpr) = do
  analyzedCSizeExpr <- analyzeExpr cSizeExpr
  case Ast.Types.exprType analyzedCSizeExpr of
    BasicNumericType -> return (CSizeStmt {characterSizeExpr = analyzedCSizeExpr})
    _ -> error "CSize statement requires a numeric expression"
analyzeStmt (LfStmt lfExpr) = do
  analyzedLfExpr <- analyzeExpr lfExpr
  case Ast.Types.exprType analyzedLfExpr of
    BasicNumericType -> return (LfStmt {lineFeedExpr = analyzedLfExpr})
    _ -> error "LF statement requires a numeric expression"
analyzeStmt RadianStmt = return RadianStmt

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
