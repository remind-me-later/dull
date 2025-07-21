module Ast.SemanticAnalysis where

import Ast.Types
import Control.Monad (unless, when)
import Control.Monad.State
import Data.Functor (($>))
import SymbolTable
import TypeSystem

data SemanticAnalysisState where
  SemanticAnalysisState ::
    { symbolTable :: SymbolTable
    } ->
    SemanticAnalysisState
  deriving (Show, Eq)

emptySemanticAnalysisState :: SemanticAnalysisState
emptySemanticAnalysisState =
  SemanticAnalysisState {symbolTable = emptySymbolTable}

insertSymbolInState :: Ident -> ExprType -> SemanticAnalysisState -> SemanticAnalysisState
insertSymbolInState sym ty (SemanticAnalysisState symTable) =
  SemanticAnalysisState {symbolTable = insertSymbol sym ty symTable}

lookupSymbolInState :: Ident -> SemanticAnalysisState -> Symbol
lookupSymbolInState name (SemanticAnalysisState symTable) =
  case lookupSymbol name symTable of
    Just sym -> sym
    Nothing -> error $ "Symbol not found: " ++ show name

analyzeStrIdent :: StrIdent -> State SemanticAnalysisState ExprType
analyzeStrIdent (StrIdent _) = return ExprStringType

analyzeNumericIdent :: NumIdent -> State SemanticAnalysisState ExprType
analyzeNumericIdent (NumIdent _) = return ExprNumericType

analyzeIdent :: Ident -> State SemanticAnalysisState ExprType
analyzeIdent (IdentStrIdent id') = analyzeStrIdent id'
analyzeIdent (IdentNumIdent id') = analyzeNumericIdent id'

analyzePsuedoVar :: PseudoVariable -> State SemanticAnalysisState ExprType
analyzePsuedoVar TimePseudoVar = return ExprNumericType
analyzePsuedoVar InkeyPseudoVar = return ExprStringType

analyzeLValue :: LValue -> State SemanticAnalysisState ExprType
analyzeLValue (LValueIdent ident) = analyzeIdent ident
analyzeLValue (LValueArrayAccess ident expr) = do
  exprType' <- analyzeExpr expr
  case Ast.Types.exprType exprType' of
    ExprNumericType -> do
      -- Arrays must be declared before use with the DIM statement
      symbol <- gets (lookupSymbolInState ident)
      case SymbolTable.exprType symbol of
        ExprArrType {exprArrType} -> return exprArrType
        _ -> error $ "Symbol " ++ symbolName symbol ++ " is not an array"
    _ -> error "Array index must be numeric"
analyzeLValue (LValuePseudoVar pseudoVar) = analyzePsuedoVar pseudoVar

analyzeStrVariableOrLiteral :: StringVariableOrLiteral -> State SemanticAnalysisState ExprType
analyzeStrVariableOrLiteral (StringLiteral _) = return ExprStringType
analyzeStrVariableOrLiteral (StringVariable ident) = analyzeStrIdent ident

analyzeFunction :: Function -> State SemanticAnalysisState ExprType
analyzeFunction (MidFun strExpr startExpr lengthExpr) = do
  strType <- analyzeStrVariableOrLiteral strExpr
  startType <- analyzeExpr startExpr
  lengthType <- analyzeExpr lengthExpr
  if strType == ExprStringType && Ast.Types.exprType startType == ExprNumericType && Ast.Types.exprType lengthType == ExprNumericType
    then return ExprStringType
    else error "Mid function requires a string and numeric expressions for start and length"
analyzeFunction (LeftFun strExpr lengthExpr) = do
  strType <- analyzeStrVariableOrLiteral strExpr
  lengthType <- analyzeExpr lengthExpr
  if strType == ExprStringType && Ast.Types.exprType lengthType == ExprNumericType
    then return ExprStringType
    else error "Left function requires a string and a numeric expression for length"
analyzeFunction (RightFun strExpr lengthExpr) = do
  strType <- analyzeStrVariableOrLiteral strExpr
  lengthType <- analyzeExpr lengthExpr
  if strType == ExprStringType && Ast.Types.exprType lengthType == ExprNumericType
    then return ExprStringType
    else error "Right function requires a string and a numeric expression for length"
analyzeFunction (AsciiFun arg) = do
  argType <- analyzeStrVariableOrLiteral arg
  if argType == ExprStringType
    then return ExprNumericType
    else error "Ascii function requires a string argument"
analyzeFunction (PointFun posExpr) = do
  posType <- analyzeExpr posExpr
  if Ast.Types.exprType posType == ExprNumericType
    then return ExprNumericType
    else error "Point function requires a numeric expression for position"
analyzeFunction (RndFun rangeEnd) =
  if rangeEnd > 0
    then return ExprNumericType
    else error "Rnd function requires a positive integer as range end"
analyzeFunction (IntFun expr) = do
  exprType <- analyzeExpr expr
  if Ast.Types.exprType exprType == ExprNumericType
    then return ExprNumericType
    else error "Int function requires a numeric expression"
analyzeFunction (SgnFun expr) = do
  exprType <- analyzeExpr expr
  if Ast.Types.exprType exprType == ExprNumericType
    then return ExprNumericType
    else error "Sgn function requires a numeric expression"

analyzeExprInner :: ExprInner -> State SemanticAnalysisState (ExprInner, ExprType)
analyzeExprInner (NumLitExpr num) = return (NumLitExpr num, ExprNumericType)
analyzeExprInner (StrLitExpr str) = return (StrLitExpr str, ExprStringType)
analyzeExprInner (LValueExpr lValue) = do
  exprType <- analyzeLValue lValue
  return (LValueExpr lValue, exprType)
analyzeExprInner (UnaryExpr op expr) = do
  analyzedExpr <- analyzeExpr expr
  let exprType = Ast.Types.exprType analyzedExpr
  case op of
    UnaryMinusOp ->
      if exprType == ExprNumericType
        then return (UnaryExpr UnaryMinusOp analyzedExpr, ExprNumericType)
        else error "Negation can only be applied to numeric expressions"
    UnaryPlusOp ->
      if exprType == ExprNumericType
        then return (UnaryExpr UnaryPlusOp analyzedExpr, ExprNumericType)
        else error "Unary plus can only be applied to numeric expressions"
    UnaryNotOp ->
      if exprType == ExprNumericType
        then return (UnaryExpr UnaryNotOp analyzedExpr, ExprStringType)
        else error "Not can only be applied to numeric expressions"
analyzeExprInner (BinExpr left op right) = do
  analyzedLeft <- analyzeExpr left
  analyzedRight <- analyzeExpr right
  let leftType = Ast.Types.exprType analyzedLeft
      rightType = Ast.Types.exprType analyzedRight
  case op of
    AddOp ->
      if leftType == ExprNumericType && rightType == ExprNumericType
        then return (BinExpr analyzedLeft AddOp analyzedRight, ExprNumericType)
        else
          if leftType == ExprStringType && rightType == ExprStringType
            then return (BinExpr analyzedLeft AddOp analyzedRight, ExprStringType)
            else error "Addition can only be applied to numeric or string expressions"
    SubtractOp ->
      if leftType == ExprNumericType && rightType == ExprNumericType
        then return (BinExpr analyzedLeft SubtractOp analyzedRight, ExprNumericType)
        else error "Subtraction can only be applied to numeric expressions"
    MultiplyOp ->
      if leftType == ExprNumericType && rightType == ExprNumericType
        then return (BinExpr analyzedLeft MultiplyOp analyzedRight, ExprNumericType)
        else error "Multiplication can only be applied to numeric expressions"
    DivideOp ->
      if leftType == ExprNumericType && rightType == ExprNumericType
        then return (BinExpr analyzedLeft DivideOp analyzedRight, ExprNumericType)
        else error "Division can only be applied to numeric expressions"
    AndOp ->
      if leftType == ExprStringType && rightType == ExprStringType
        then return (BinExpr analyzedLeft AndOp analyzedRight, ExprStringType)
        else error "And can only be applied to string expressions"
    OrOp ->
      if leftType == ExprStringType && rightType == ExprStringType
        then return (BinExpr analyzedLeft OrOp analyzedRight, ExprStringType)
        else error "Or can only be applied to string expressions"
    EqualOp ->
      if leftType == rightType
        then return (BinExpr analyzedLeft EqualOp analyzedRight, ExprStringType)
        else error "Equality can only be applied to expressions of the same type"
    LessThanOp ->
      if leftType == ExprNumericType && rightType == ExprNumericType
        then return (BinExpr analyzedLeft LessThanOp analyzedRight, ExprStringType)
        else error "Less than can only be applied to numeric expressions"
    GreaterThanOp ->
      if leftType == ExprNumericType && rightType == ExprNumericType
        then return (BinExpr analyzedLeft GreaterThanOp analyzedRight, ExprStringType)
        else error "Greater than can only be applied to numeric expressions"
    LessThanOrEqualOp ->
      if leftType == ExprNumericType && rightType == ExprNumericType
        then return (BinExpr analyzedLeft LessThanOrEqualOp analyzedRight, ExprStringType)
        else error "Less than or equal can only be applied to numeric expressions"
    GreaterThanOrEqualOp ->
      if leftType == ExprNumericType && rightType == ExprNumericType
        then return (BinExpr analyzedLeft GreaterThanOrEqualOp analyzedRight, ExprStringType)
        else error "Greater than or equal can only be applied to numeric expressions"
    NotEqualOp ->
      if leftType == rightType
        then return (BinExpr analyzedLeft NotEqualOp analyzedRight, ExprStringType)
        else error "Not equal can only be applied to expressions of the same type"
    CaretOp ->
      if leftType == ExprStringType && rightType == ExprStringType
        then return (BinExpr analyzedLeft CaretOp analyzedRight, ExprStringType)
        else error "Caret can only be applied to string expressions"
analyzeExprInner (FunCallExpr ident) = case ident of
  MidFun {midFunStringExpr, midFunStartExpr, midFunLengthExpr} -> do
    strType <- analyzeStrVariableOrLiteral midFunStringExpr
    startType <- analyzeExpr midFunStartExpr
    lengthType <- analyzeExpr midFunLengthExpr
    if strType == ExprStringType && Ast.Types.exprType startType == ExprNumericType && Ast.Types.exprType lengthType == ExprNumericType
      then return (FunCallExpr ident, ExprStringType)
      else error "Mid function requires a string and numeric expressions for start and length"
  LeftFun {leftFunStringExpr, leftFunLengthExpr} -> do
    strType <- analyzeStrVariableOrLiteral leftFunStringExpr
    lengthType <- analyzeExpr leftFunLengthExpr
    if strType == ExprStringType && Ast.Types.exprType lengthType == ExprNumericType
      then return (FunCallExpr ident, ExprStringType)
      else error "Left function requires a string and a numeric expression for length"
  RightFun {rightFunStringExpr, rightFunLengthExpr} -> do
    strType <- analyzeStrVariableOrLiteral rightFunStringExpr
    lengthType <- analyzeExpr rightFunLengthExpr
    if strType == ExprStringType && Ast.Types.exprType lengthType == ExprNumericType
      then return (FunCallExpr ident, ExprStringType)
      else error "Right function requires a string and a numeric expression for length"
  AsciiFun {asciiFunArgument} -> do
    argType <- analyzeStrVariableOrLiteral asciiFunArgument
    if argType == ExprStringType
      then return (FunCallExpr ident, ExprNumericType)
      else error "Ascii function requires a string argument"
  PointFun {pointFunPositionExpr} -> do
    posType <- analyzeExpr pointFunPositionExpr
    if Ast.Types.exprType posType == ExprNumericType
      then return (FunCallExpr ident, ExprNumericType)
      else error "Point function requires a numeric expression for position"
  RndFun {} -> do
    return (FunCallExpr ident, ExprNumericType)
  IntFun {intFunExpr} -> do
    exprType <- analyzeExpr intFunExpr
    if Ast.Types.exprType exprType == ExprNumericType
      then return (FunCallExpr ident, ExprNumericType)
      else error "Int function requires a numeric expression"
  SgnFun {sgnFunExpr} -> do
    exprType <- analyzeExpr sgnFunExpr
    if Ast.Types.exprType exprType == ExprNumericType
      then return (FunCallExpr ident, ExprNumericType)
      else error "Sgn function requires a numeric expression"

analyzeExpr :: Expr -> State SemanticAnalysisState Expr
analyzeExpr Expr {exprInner} = do
  (analyzedInner, innerType) <- analyzeExprInner exprInner
  return Expr {exprInner = analyzedInner, Ast.Types.exprType = innerType}

-- We have to add the arrays to the symbol table
analyzeDimKind :: DimKind -> State SemanticAnalysisState ExprType
analyzeDimKind (DimNumeric varName size) = do
  let exprType =
        ExprArrType
          { exprArrType = ExprNumericType,
            exprArrSize = size,
            exprArrLength = sizeOfTy ExprNumericType
          }
      newIdent = IdentNumIdent varName
  modify (insertSymbolInState newIdent exprType)
  return ExprNumericType
analyzeDimKind (DimString varName size length') = do
  let exprType =
        ExprArrType
          { exprArrType = ExprStringType,
            exprArrSize = size,
            exprArrLength = length'
          }
      newIdent = IdentStrIdent varName
  modify (insertSymbolInState newIdent exprType)
  return ExprStringType

analyzeAssignment :: Assignment -> State SemanticAnalysisState ExprType
analyzeAssignment (Assignment lValue expr) = do
  lValueType <- analyzeLValue lValue
  exprType <- analyzeExpr expr
  if lValueType == Ast.Types.exprType exprType
    then return lValueType
    else error $ "Type mismatch in assignment: " ++ show lValueType ++ " vs " ++ show (Ast.Types.exprType exprType)

analyzeStmt :: Stmt -> State SemanticAnalysisState ()
analyzeStmt (LetStmt assignments) = mapM_ analyzeAssignment assignments
analyzeStmt (IfThenStmt condition thenStmt) = do
  conditionType <- analyzeExpr condition

  when (Ast.Types.exprType conditionType /= ExprNumericType) $
    error "Condition in IF statement must be numeric"

  analyzeStmt thenStmt
analyzeStmt (PrintStmt _printKind printExprs _printEnding _printUsingClause) =
  mapM_ analyzeExpr printExprs
analyzeStmt (UsingStmt _) = return () -- Using clause is not analyzed here
analyzeStmt (InputStmt inputPrintExpr inputDestination) = do
  case inputPrintExpr of
    Just expr -> do
      _ <- analyzeExpr expr
      return ()
    Nothing -> return ()

  analyzeIdent inputDestination $> ()
analyzeStmt EndStmt = return ()
analyzeStmt Comment = return ()
analyzeStmt (ForStmt forAssignment forToExpr) = do
  forType <- analyzeAssignment forAssignment
  toType <- analyzeExpr forToExpr

  when (forType /= ExprNumericType || Ast.Types.exprType toType /= ExprNumericType) $
    error "For statement requires numeric expressions for assignment and to"
analyzeStmt (NextStmt nextIdent) = do
  let nextType = ExprNumericType -- Next always refers to a numeric identifier
  modify (insertSymbolInState (IdentNumIdent nextIdent) nextType)
analyzeStmt ClearStmt = return ()
analyzeStmt (GoToStmt _gotoTarget) = return ()
analyzeStmt (GoSubStmt _gosubTarget) = return ()
analyzeStmt (WaitStmt waitForExpr) = do
  case waitForExpr of
    Just expr -> do
      exprType <- analyzeExpr expr
      if Ast.Types.exprType exprType == ExprNumericType
        then return ()
        else error "Wait statement requires a numeric expression"
    Nothing -> return () -- No expression means wait indefinitely
analyzeStmt ClsStmt = return ()
analyzeStmt RandomStmt = return ()
analyzeStmt (GprintStmt gprintExprs _gprintEnding) = do
  mapM_ analyzeExpr gprintExprs
analyzeStmt (GCursorStmt gCursorExpr) = do
  gCursorType <- analyzeExpr gCursorExpr

  when (Ast.Types.exprType gCursorType /= ExprNumericType) $
    error "GCursor statement requires a numeric expression"
analyzeStmt (CursorStmt cursorExpr) = do
  cursorType <- analyzeExpr cursorExpr

  when (Ast.Types.exprType cursorType /= ExprNumericType) $
    error "Cursor statement requires a numeric expression"
analyzeStmt (BeepStmt beepExprs) = do
  exprTypes <- mapM analyzeExpr beepExprs

  unless (all (\et -> Ast.Types.exprType et == ExprNumericType) exprTypes) $
    error "Beep statement requires numeric expressions"
analyzeStmt ReturnStmt = return ()
analyzeStmt (PokeStmt _pokeKind pokeExprs) = do
  exprTypes <- mapM analyzeExpr pokeExprs

  unless (all (\et -> Ast.Types.exprType et == ExprNumericType) exprTypes) $
    error "Poke statement requires numeric expressions"
analyzeStmt (DimStmt dimKind) = analyzeDimKind dimKind $> ()
-- FIXME: check that data, read and restore are typed correctly
analyzeStmt (ReadStmt readStmtDestinations) = do
  mapM_ analyzeLValue readStmtDestinations
analyzeStmt (DataStmt exprs) = do
  mapM_ analyzeExpr exprs
analyzeStmt (RestoreStmt restoreLineOrLabelExpr) = do
  _ <- analyzeExpr restoreLineOrLabelExpr
  return ()

analyzeLine :: Line -> State SemanticAnalysisState ()
analyzeLine (Line _lineNumber _lineLabel lineStmts) = mapM_ analyzeStmt lineStmts

analyzeProgram :: Program -> State SemanticAnalysisState ()
analyzeProgram (Program lines') = mapM_ analyzeLine lines'
