module Ast.SemanticAnalysis
  ( analyzeProgram,
  )
where

import Ast.Types
import Control.Monad (unless, when)
import Control.Monad.State
import Data.Maybe (fromMaybe)
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

insertSymbolInState :: Ident -> BasicType -> SemanticAnalysisState -> SemanticAnalysisState
insertSymbolInState sym ty (SemanticAnalysisState symTable) =
  SemanticAnalysisState {symbolTable = insertSymbol sym ty symTable}

insertStringLiteralInState :: String -> SemanticAnalysisState -> SemanticAnalysisState
insertStringLiteralInState str (SemanticAnalysisState symTable) =
  SemanticAnalysisState {symbolTable = insertStringLiteral str symTable}

lookupSymbolInState :: Ident -> SemanticAnalysisState -> Variable
lookupSymbolInState name (SemanticAnalysisState symTable) =
  case lookupSymbol name symTable of
    Just sym -> sym
    Nothing -> error $ "Symbol not found: " ++ show name

insertUsedLabelInState :: GotoTarget -> Bool -> SemanticAnalysisState -> SemanticAnalysisState
insertUsedLabelInState label isFunctionCall (SemanticAnalysisState symTable) =
  SemanticAnalysisState {symbolTable = insertUsedLabel label isFunctionCall symTable}

analyzeStrIdent :: StrIdent -> State SemanticAnalysisState BasicType
analyzeStrIdent id'@(StrIdent _) = do
  modify (insertSymbolInState (IdentStrIdent id') BasicStringType)
  return BasicStringType

analyzeNumericIdent :: NumIdent -> State SemanticAnalysisState BasicType
analyzeNumericIdent id'@(NumIdent _) = do
  modify (insertSymbolInState (IdentNumIdent id') BasicNumericType)
  return BasicNumericType

analyzeIdent :: Ident -> State SemanticAnalysisState BasicType
analyzeIdent (IdentStrIdent id') = analyzeStrIdent id'
analyzeIdent (IdentNumIdent id') = analyzeNumericIdent id'

analyzePsuedoVar :: PseudoVariable -> State SemanticAnalysisState BasicType
analyzePsuedoVar TimePseudoVar = return BasicNumericType
analyzePsuedoVar InkeyPseudoVar = return BasicStringType

analyzeLValue :: RawLValue -> State SemanticAnalysisState (TypedLValue, BasicType)
analyzeLValue (LValueIdent ident) = do
  ty <- analyzeIdent ident
  return (LValueIdent ident, ty)
analyzeLValue (LValueArrayAccess ident expr) = do
  exprType' <- analyzeExpr expr
  case Ast.Types.exprType exprType' of
    BasicNumericType -> do
      -- Arrays must be declared before use with the DIM statement
      symbol <- gets (lookupSymbolInState ident)
      case SymbolTable.variableType symbol of
        BasicNumArrType {numericArrSize} ->
          if numericArrSize >= 0
            then return (LValueArrayAccess {lValueArrayIdent = ident, lValueArrayIndex = exprType'}, BasicNumericType)
            else error $ "Array " ++ show ident ++ " is not declared or has invalid size"
        BasicStrArrType {strArrLength, strArrSize} ->
          if strArrSize >= 0 && strArrLength >= 0
            then return (LValueArrayAccess {lValueArrayIdent = ident, lValueArrayIndex = exprType'}, BasicStringType)
            else error $ "String array " ++ show ident ++ " is not declared or has invalid size"
        _ -> error $ "Array " ++ show ident ++ " is not a numeric or string array"
    _ -> error "Array index must be numeric"
analyzeLValue (LValuePseudoVar pseudoVar) = do
  ty <- analyzePsuedoVar pseudoVar
  return (LValuePseudoVar pseudoVar, ty)

analyzeStrVariableOrLiteral :: StringVariableOrLiteral -> State SemanticAnalysisState BasicType
analyzeStrVariableOrLiteral (StringLiteral lit) = do
  modify (insertStringLiteralInState lit)
  return BasicStringType
analyzeStrVariableOrLiteral (StringVariable ident) = analyzeStrIdent ident

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
      else error "Mid function requires a string and numeric expressions for start and length"
  LeftFun {leftFunStringExpr, leftFunLengthExpr} -> do
    strType <- analyzeStrVariableOrLiteral leftFunStringExpr
    lengthType <- analyzeExpr leftFunLengthExpr
    if strType == BasicStringType && Ast.Types.exprType lengthType == BasicNumericType
      then
        return
          ( LeftFun {leftFunStringExpr, leftFunLengthExpr = lengthType},
            BasicStringType
          )
      else error "Left function requires a string and a numeric expression for length"
  RightFun {rightFunStringExpr, rightFunLengthExpr} -> do
    strType <- analyzeStrVariableOrLiteral rightFunStringExpr
    lengthType <- analyzeExpr rightFunLengthExpr
    if strType == BasicStringType && Ast.Types.exprType lengthType == BasicNumericType
      then
        return
          ( RightFun {rightFunStringExpr, rightFunLengthExpr = lengthType},
            BasicStringType
          )
      else error "Right function requires a string and a numeric expression for length"
  AsciiFun {asciiFunArgument} -> do
    argType <- analyzeStrVariableOrLiteral asciiFunArgument
    if argType == BasicStringType
      then return (AsciiFun {asciiFunArgument}, BasicNumericType)
      else error "Ascii function requires a string argument"
  PointFun {pointFunPositionExpr} -> do
    posType <- analyzeExpr pointFunPositionExpr
    if Ast.Types.exprType posType == BasicNumericType
      then return (PointFun {pointFunPositionExpr = posType}, BasicNumericType)
      else error "Point function requires a numeric expression for position"
  RndFun {rndRangeEnd} -> do
    return (RndFun {rndRangeEnd}, BasicNumericType)
  IntFun {intFunExpr} -> do
    exprType <- analyzeExpr intFunExpr
    if Ast.Types.exprType exprType == BasicNumericType
      then return (IntFun {intFunExpr = exprType}, BasicNumericType)
      else error "Int function requires a numeric expression"
  SgnFun {sgnFunExpr} -> do
    exprType <- analyzeExpr sgnFunExpr
    if Ast.Types.exprType exprType == BasicNumericType
      then return (SgnFun {sgnFunExpr = exprType}, BasicNumericType)
      else error "Sgn function requires a numeric expression"

analyzeExprInner :: RawExprInner -> State SemanticAnalysisState (TypedExprInner, BasicType)
analyzeExprInner (NumLitExpr num) = return (NumLitExpr num, BasicNumericType)
analyzeExprInner (StrLitExpr str) = do
  -- Insert the string literal into the symbol table
  modify (insertStringLiteralInState str)
  return (StrLitExpr str, BasicStringType)
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
analyzeDimKind :: DimKind -> State SemanticAnalysisState BasicType
analyzeDimKind (DimNumeric varName size) =
  let exprType = BasicNumArrType {numericArrSize = size}
      newIdent = IdentNumIdent varName
   in do
        modify (insertSymbolInState newIdent exprType)
        return BasicNumericType
analyzeDimKind (DimString varName size length') =
  let concreteLength = fromMaybe defaultStringLength length'
      exprType = BasicStrArrType {strArrSize = size, strArrLength = concreteLength}
      newIdent = IdentStrIdent varName
   in do
        modify (insertSymbolInState newIdent exprType)
        return BasicStringType

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

analyzeStmt :: RawStmt -> State SemanticAnalysisState TypedStmt
analyzeStmt (LetStmt assignments) = do
  analyzedAssignments <- mapM analyzeAssignment assignments
  return (LetStmt analyzedAssignments)
analyzeStmt (IfThenStmt condition thenStmt) = do
  conditionType <- analyzeExpr condition

  when (Ast.Types.exprType conditionType /= BasicNumericType) $
    error "Condition in IF statement must be numeric"

  analyzeStmt thenStmt
analyzeStmt (PrintStmt printKind printExprs printEnding printUsingClause) = do
  -- mapM_ analyzeExpr printExprs
  exprTypes <- mapM analyzeExpr printExprs
  return
    ( PrintStmt
        { printKind = printKind,
          printExprs = exprTypes,
          printEnding = printEnding,
          printUsingClause = printUsingClause
        }
    )
analyzeStmt (UsingStmt u) = return (UsingStmt u)
analyzeStmt (InputStmt inputPrintExpr inputDestination) = do
  case inputPrintExpr of
    Just expr -> do
      analyzedExpr <- analyzeExpr expr
      _ <- analyzeIdent inputDestination
      return (InputStmt {inputPrintExpr = Just analyzedExpr, inputDestination = inputDestination})
    Nothing -> do
      _ <- analyzeIdent inputDestination
      return (InputStmt {inputPrintExpr = Nothing, inputDestination = inputDestination})
analyzeStmt EndStmt = return EndStmt
analyzeStmt Comment = return Comment
analyzeStmt (ForStmt forAssignment forToExpr) = do
  forType <- analyzeAssignment forAssignment
  toType <- analyzeExpr forToExpr

  when (assignmentType forType /= BasicNumericType || Ast.Types.exprType toType /= BasicNumericType) $
    error "For statement requires numeric expressions for assignment and to"

  return (ForStmt forType toType)
analyzeStmt (NextStmt nextIdent) = do
  let nextType = BasicNumericType -- Next always refers to a numeric identifier
  modify (insertSymbolInState (IdentNumIdent nextIdent) nextType)
  return NextStmt {nextIdent = nextIdent}
analyzeStmt ClearStmt = return ClearStmt
analyzeStmt (GoToStmt gotoTarget) = do
  modify (insertUsedLabelInState gotoTarget False)
  return (GoToStmt {gotoTarget = gotoTarget})
analyzeStmt (GoSubStmt gosubTarget) = do
  modify (insertUsedLabelInState gosubTarget True)
  return (GoSubStmt {gosubTarget = gosubTarget})
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
analyzeStmt (GprintStmt gprintExprs gprintEnding) = do
  analyzedExprs <- mapM analyzeExpr gprintExprs

  return (GprintStmt {gprintExprs = analyzedExprs, gprintEnding = gprintEnding})
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
analyzeStmt (BeepStmt beepExprs) = do
  exprTypes <- mapM analyzeExpr beepExprs

  unless (all (\et -> Ast.Types.exprType et == BasicNumericType) exprTypes) $
    error "Beep statement requires numeric expressions"

  return (BeepStmt {beepExprs = exprTypes})
analyzeStmt ReturnStmt = return ReturnStmt
analyzeStmt (PokeStmt pokeKind pokeExprs) = do
  exprTypes <- mapM analyzeExpr pokeExprs

  unless (all (\et -> Ast.Types.exprType et == BasicNumericType) exprTypes) $
    error "Poke statement requires numeric expressions"

  return (PokeStmt {pokeKind = pokeKind, pokeExprs = exprTypes})
analyzeStmt (DimStmt dimKind) = do
  _ <- analyzeDimKind dimKind
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

analyzeLine :: RawLine -> State SemanticAnalysisState TypedLine
analyzeLine (Line lineNumber lineLabel lineStmts) = do
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
