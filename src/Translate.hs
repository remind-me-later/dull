module Translate where

import Ast.Types
import Data.List (intercalate)
import Data.Word (Word8)
import TypeSystem (BasicType)

translateDecimalNumber :: DecimalNumber -> [Word8]
translateDecimalNumber n = map (fromIntegral . fromEnum) (show n)

translateBinaryNumber :: BinaryNumber -> [Word8]
translateBinaryNumber n = map (fromIntegral . fromEnum) (show n)

translateFunction :: Function BasicType -> [Word8]
translateFunction MidFun {midFunStringExpr, midFunStartExpr, midFunLengthExpr} =
  -- Code: 0xF17B
  [0xF1, 0x7B, fromIntegral $ fromEnum '(']
    ++ translateExpr midFunStringExpr
    ++ [fromIntegral $ fromEnum ',']
    ++ translateExpr midFunStartExpr
    ++ [fromIntegral $ fromEnum ',']
    ++ translateExpr midFunLengthExpr
    ++ [fromIntegral $ fromEnum ')']
translateFunction LeftFun {leftFunStringExpr, leftFunLengthExpr} =
  -- Code: 0xF17A
  [0xF1, 0x7A, fromIntegral $ fromEnum '(']
    ++ translateExpr leftFunStringExpr
    ++ [fromIntegral $ fromEnum ',']
    ++ translateExpr leftFunLengthExpr
    ++ [fromIntegral $ fromEnum ')']
translateFunction RightFun {rightFunStringExpr, rightFunLengthExpr} =
  -- Code: 0xF172
  [0xF1, 0x72, fromIntegral $ fromEnum '(']
    ++ translateExpr rightFunStringExpr
    ++ [fromIntegral $ fromEnum ',']
    ++ translateExpr rightFunLengthExpr
    ++ [fromIntegral $ fromEnum ')']
translateFunction AsciiFun {asciiFunArgument} =
  -- Code: 0xF160
  [0xF1, 0x60] ++ translateExpr asciiFunArgument
translateFunction PointFun {pointFunPositionExpr} =
  -- Code: 0xF168
  [0xF1, 0x68] ++ translateExpr pointFunPositionExpr
translateFunction RndFun {rndRangeEnd} =
  -- Code: 0xF17C
  [0xF1, 0x7C] ++ translateExpr rndRangeEnd
translateFunction IntFun {intFunExpr} =
  -- Code: 0xF171
  [0xF1, 0x71] ++ translateExpr intFunExpr
translateFunction SgnFun {sgnFunExpr} =
  -- Code: 0xF179
  [0xF1, 0x79] ++ translateExpr sgnFunExpr
translateFunction StatusFun {statusFunArg} =
  -- Code: 0xF167
  [0xF1, 0x67, statusFunArg] -- statusFunArg is a Word8
translateFunction ValFun {valFunExpr} =
  -- Code: 0xF162
  [0xF1, 0x62] ++ translateExpr valFunExpr
translateFunction StrFun {strFunExpr} =
  -- Code: 0xF161
  [0xF1, 0x61] ++ translateExpr strFunExpr
translateFunction ChrFun {chrFunExpr} =
  -- Code: 0xF163
  [0xF1, 0x63] ++ translateExpr chrFunExpr
translateFunction AbsFun {absFunExpr} =
  -- Code: 0xF170
  [0xF1, 0x70] ++ translateExpr absFunExpr
translateFunction LenFun {lenFunExpr} =
  -- Code: 0xF164
  [0xF1, 0x64] ++ translateExpr lenFunExpr
translateFunction PeekFun {peekMemoryArea, peekFunAddress} =
  -- Peek code: 0xF16F
  -- Peek# code: 0xF16E
  [0xF1, if peekMemoryArea == Me0 then 0x6F else 0x6E]
    ++ translateExpr peekFunAddress

translatePseudoVariable :: PseudoVariable -> [Word8]
translatePseudoVariable TimePseudoVar = [0xF1, 0x5B]
translatePseudoVariable InkeyPseudoVar = [0xF1, 0x5C]

translateIdent :: Ident -> [Word8]
translateIdent ident = map (fromIntegral . fromEnum) (show ident)

translateLValue :: LValue BasicType -> [Word8]
translateLValue lvalue = map (fromIntegral . fromEnum) (show lvalue)

unaryOperatorByteSize :: UnaryOperator -> Int
unaryOperatorByteSize UnaryMinusOp = 1
unaryOperatorByteSize UnaryPlusOp = 0
unaryOperatorByteSize UnaryNotOp = 2

translateUnaryOperator :: UnaryOperator -> [Word8]
translateUnaryOperator UnaryMinusOp = [fromIntegral $ fromEnum '-']
translateUnaryOperator UnaryPlusOp = [] -- No byte for UnaryPlusOp
translateUnaryOperator UnaryNotOp =
  -- 0xF16D
  [0xF1, 0x6D]

translateBinOperator :: BinOperator -> [Word8]
translateBinOperator OrOp =
  -- 0xF151
  [0xF1, 0x51]
translateBinOperator AndOp =
  -- 0xF150
  [0xF1, 0x50]
translateBinOperator op = map (fromIntegral . fromEnum) (show op)

-- Helper functions for precedence-aware printing
showExprWithContext :: Int -> Bool -> Expr BasicType -> [Word8]
showExprWithContext parentPrec isRightSide (Expr {exprInner = inner}) =
  showExprInnerWithContext parentPrec isRightSide inner

showExprInnerWithContext :: Int -> Bool -> ExprInner BasicType -> [Word8]
showExprInnerWithContext _ _ (UnaryExpr op expr) =
  translateUnaryOperator op ++ showExprWithContext 7 False expr -- Unary has highest precedence
showExprInnerWithContext parentPrec isRightSide (BinExpr left op right) =
  let myPrec = precedence op
      leftAssoc = isLeftAssociative op
      needsParens =
        myPrec < parentPrec
          || myPrec == parentPrec && isRightSide && leftAssoc
          || myPrec == parentPrec && not isRightSide && not leftAssoc
      leftStr = showExprWithContext myPrec False left
      rightStr = showExprWithContext myPrec True right
      result = leftStr ++ translateBinOperator op ++ rightStr
   in if needsParens
        then
          [fromIntegral (fromEnum '(')] ++ result ++ [fromIntegral (fromEnum ')')]
        else result
showExprInnerWithContext _ _ (DecNumLitExpr n) = translateDecimalNumber n
showExprInnerWithContext _ _ (HexNumLitExpr h) = translateBinaryNumber h
showExprInnerWithContext _ _ (LValueExpr lval) = translateLValue lval
showExprInnerWithContext _ _ (StrLitExpr s) =
  [fromIntegral $ fromEnum '"'] ++ map (fromIntegral . fromEnum) s ++ [fromIntegral $ fromEnum '"']
showExprInnerWithContext _ _ (FunCallExpr f) = translateFunction f

translateExprInner :: ExprInner BasicType -> [Word8]
translateExprInner (UnaryExpr op expr) =
  translateUnaryOperator op ++ showExprWithContext 7 False expr
translateExprInner (BinExpr left op right) =
  showExprInnerWithContext 0 False (BinExpr left op right)
translateExprInner (DecNumLitExpr n) = translateDecimalNumber n
translateExprInner (HexNumLitExpr h) = translateBinaryNumber h
translateExprInner (LValueExpr lval) = translateLValue lval
translateExprInner (StrLitExpr s) =
  [fromIntegral $ fromEnum '"'] ++ map (fromIntegral . fromEnum) s ++ [fromIntegral $ fromEnum '"']
translateExprInner (FunCallExpr f) = translateFunction f

translateExpr :: Expr BasicType -> [Word8]
translateExpr Expr {exprInner} = translateExprInner exprInner

translatePrintEnding :: PrintEnding -> [Word8]
translatePrintEnding e = map (fromIntegral . fromEnum) (show e)

translateUsingClause :: UsingClause -> [Word8]
translateUsingClause (UsingClause Nothing) =
  -- Code: 0xF085
  [0xF0, 0x85] -- Represents an empty USING clause
translateUsingClause (UsingClause (Just expr)) =
  -- Code: 0xF085
  [0xF0, 0x85] ++ map (fromIntegral . fromEnum) expr

translateAssignment :: Assignment BasicType -> [Word8]
translateAssignment (Assignment lValue expr _) =
  translateLValue lValue ++ [fromIntegral $ fromEnum '='] ++ translateExpr expr

translateDimInner :: DimInner BasicType -> [Word8]
translateDimInner (DimInner1D {dimIdent, dimSize, dimStringLength}) =
  translateIdent dimIdent
    ++ [fromIntegral $ fromEnum '(']
    ++ translateExpr dimSize
    ++ [fromIntegral $ fromEnum ')']
    ++ case dimStringLength of
      Just len -> fromIntegral (fromEnum '*') : translateExpr len
      Nothing -> []
translateDimInner (DimInner2D {dimIdent, dimRows, dimCols, dimStringLength}) =
  translateIdent dimIdent
    ++ [fromIntegral $ fromEnum '(']
    ++ translateExpr dimRows
    ++ [fromIntegral $ fromEnum ',']
    ++ translateExpr dimCols
    ++ [fromIntegral $ fromEnum ')']
    ++ case dimStringLength of
      Just len -> fromIntegral (fromEnum '*') : translateExpr len
      Nothing -> []

translateBeepOptionalParams :: BeepOptionalParams BasicType -> [Word8]
translateBeepOptionalParams (BeepOptionalParams {beepFrequency, beepDuration}) =
  [fromIntegral $ fromEnum ',']
    ++ translateExpr beepFrequency
    ++ case beepDuration of
      Just duration -> fromIntegral (fromEnum ',') : translateExpr duration
      Nothing -> []

translatePrintCommaFormat :: PrintCommaFormat BasicType -> [Word8]
translatePrintCommaFormat (PrintCommaFormat e1 e2) =
  translateExpr e1 ++ [fromIntegral $ fromEnum ','] ++ translateExpr e2
translatePrintCommaFormat (PrintSemicolonFormat maybeUsingClause exprs ending) =
  case maybeUsingClause of
    Just usingClause ->
      translateUsingClause usingClause
        ++ [fromIntegral $ fromEnum ';']
        ++ intercalate [fromIntegral $ fromEnum ';'] (translateExpr <$> exprs)
        ++ translatePrintEnding ending
    Nothing ->
      intercalate [fromIntegral $ fromEnum ';'] (translateExpr <$> exprs)
        ++ translatePrintEnding ending

translateGPrintSeparator :: GPrintSeparator -> [Word8]
translateGPrintSeparator GPrintSeparatorComma = [fromIntegral $ fromEnum ',']
translateGPrintSeparator GPrintSeparatorSemicolon = [fromIntegral $ fromEnum ';']
translateGPrintSeparator GPrintSeparatorEmpty = []

translateLetStmt :: Bool -> [Assignment BasicType] -> [Word8]
translateLetStmt isMandatoryLet assignments =
  if isMandatoryLet
    then
      -- Let code: 0xF198
      [0xF1, 0x98] ++ intercalate [fromIntegral $ fromEnum ','] (translateAssignment <$> assignments)
    else
      intercalate [fromIntegral $ fromEnum ','] (translateAssignment <$> assignments)

translateStmt :: Stmt BasicType -> [Word8]
translateStmt (LetStmt assignments) = translateLetStmt False assignments
translateStmt (IfThenStmt cond s) =
  -- If code: 0xF196H
  [0xF1, 0x96]
    ++ translateExpr cond
    ++ case s of
      LetStmt letStmt -> fromIntegral (fromEnum ' ') : translateLetStmt True letStmt
      GoToStmt l -> fromIntegral (fromEnum ' ') : translateExpr l
      _ -> fromIntegral (fromEnum ' ') : translateStmt s
translateStmt (PrintStmt {printCommaFormat}) =
  -- Print code: 0xF097
  [0xF0, 0x97] ++ maybe [] translatePrintCommaFormat printCommaFormat
translateStmt (PauseStmt {pauseCommaFormat}) =
  -- Pause code: 0xF1A2
  [0xF1, 0xA2] ++ maybe [] translatePrintCommaFormat pauseCommaFormat
translateStmt (UsingStmt usingClause) = translateUsingClause usingClause
translateStmt (InputStmt maybePrintExpr me) =
  -- Input code: 0xF091
  [0xF0, 0x91]
    ++ maybe [] (\e -> map (fromIntegral . fromEnum) e ++ [fromIntegral $ fromEnum ';']) maybePrintExpr
    ++ translateLValue me
translateStmt EndStmt =
  -- End code: 0xF18E
  [0xF1, 0x8E]
translateStmt Comment =
  -- Comment code: 0xF1AB
  [0xF1, 0xAB]
translateStmt (ForStmt assign to step) =
  -- For code: 0xF1A5
  -- To code: 0xF1B1
  -- Step code: 0xF1AD
  -- Then code: 0xF1AE
  [0xF1, 0xA5]
    ++ translateAssignment assign
    ++ [0xF1, 0xB1] -- "TO"
    ++ translateExpr to
    ++ case step of
      Just s -> [0xF1, 0xAD] ++ translateExpr s -- "STEP"
      Nothing -> []
translateStmt (NextStmt i) =
  -- Next code: 0xF19A
  [0xF1, 0x9A] ++ translateIdent i
translateStmt ClearStmt =
  -- Clear code: 0xF187
  [0xF1, 0x87]
translateStmt (GoToStmt target) =
  -- GoTo code: 0xF192
  [0xF1, 0x92] ++ translateExpr target
translateStmt (GoSubStmt target) =
  -- GoSub code: 0xF194
  [0xF1, 0x94] ++ translateExpr target
translateStmt (WaitStmt maybeExpr) =
  -- Wait code: 0xF1B3
  [0xF1, 0xB3] ++ maybe [] translateExpr maybeExpr
translateStmt ClsStmt =
  -- Cls code: 0xF088
  [0xF0, 0x88]
translateStmt RandomStmt =
  -- Random code: 0xF1A8
  [0xF1, 0xA8]
translateStmt (GprintStmt exprs) =
  -- GPrint code: 0xF09F
  [0xF0, 0x9F]
    ++ concatMap (\(e, sep) -> translateExpr e ++ translateGPrintSeparator sep) (init exprs)
    ++ ( \(e, sep) ->
           translateExpr e ++ translateGPrintSeparator sep
       )
      (last exprs)
translateStmt (GCursorStmt e) =
  -- GCursor code: 0xF093
  [0xF0, 0x93] ++ translateExpr e
translateStmt (BeepStmt repetitions optionalParams) =
  -- Beep code: 0xF182
  [0xF1, 0x82] ++ translateExpr repetitions ++ maybe [] translateBeepOptionalParams optionalParams
translateStmt (BeepOnOffStmt beepOn) =
  -- Beep code: 0xF182
  -- On code: 0xF19C
  -- Off code: 0xF19E
  [0xF1, 0x82]
    ++ [0xF1, if beepOn then 0x9C else 0x9E]
translateStmt (CursorStmt e) =
  -- Cursor code: 0xF084
  [0xF0, 0x84] ++ translateExpr e
translateStmt ReturnStmt =
  -- Return code: 0xF199
  [0xF1, 0x99]
translateStmt (PokeStmt kind exprs) =
  -- Poke code: 0xF1A1
  -- Poke# code: 0xF1A0
  [0xF1, if kind == Me0 then 0xA1 else 0xA0]
    ++ intercalate [fromIntegral $ fromEnum ','] (translateExpr <$> exprs)
translateStmt (DimStmt decls) =
  -- Dim code: 0xF18B
  [0xF1, 0x8B] ++ intercalate [fromIntegral $ fromEnum ','] (translateDimInner <$> decls)
translateStmt (ReadStmt ids) =
  -- Read code: 0xF1A6
  [0xF1, 0xA6] ++ intercalate [fromIntegral $ fromEnum ','] (translateLValue <$> ids)
translateStmt (DataStmt exprs) =
  -- Data code: 0xF18D
  [0xF1, 0x8D] ++ intercalate [fromIntegral $ fromEnum ','] (translateExpr <$> exprs)
translateStmt (RestoreStmt n) =
  -- Restore code: 0xF1A7
  [0xF1, 0xA7] ++ maybe [] translateExpr n
translateStmt ArunStmt =
  -- Arun code: 0xF181
  [0xF1, 0x81]
translateStmt LockStmt =
  -- Lock code: 0xF1B5
  [0xF1, 0xB5]
translateStmt UnlockStmt =
  -- Unlock code: 0xF1B6
  [0xF1, 0xB6]
translateStmt (OnGoToStmt expr targets) =
  -- On code: 0xF19C
  -- Goto code: 0xF192
  [0xF1, 0x9C] ++ translateExpr expr ++ [0xF1, 0x92] ++ intercalate [fromIntegral $ fromEnum ','] (translateExpr <$> targets)
translateStmt (OnGoSubStmt expr targets) =
  -- On code: 0xF19C
  -- Gosub code: 0xF194
  [0xF1, 0x9C] ++ translateExpr expr ++ [0xF1, 0x94] ++ intercalate [fromIntegral $ fromEnum ','] (translateExpr <$> targets)
translateStmt (OnErrorGotoStmt target) =
  -- On code: 0xF19C
  -- Error code: 0xF1B4
  -- Goto code: 0xF192
  [0xF1, 0x9C, 0xF1, 0xB4, 0xF1, 0x92] ++ translateExpr target
translateStmt (CallStmt expr) =
  -- Call code: 0xF18A
  [0xF1, 0x8A] ++ translateExpr expr
translateStmt (LPrintStmt maybeCommaFormat) =
  -- LPrint code: 0xF0B9
  [0xF0, 0xB9] ++ maybe [] translatePrintCommaFormat maybeCommaFormat

-- FIXME: translate line labels
-- First we put the word16 line number in two adjacent bytes, then the length of the line in bytes,
-- then the translated statements, and we end with a carriage return byte (0x0D).
translateLine :: Line BasicType -> [Word8]
translateLine Line {lineNumber, lineLabel = _, lineStmts} =
  let lineNumBytes = [fromIntegral (lineNumber `div` 256), fromIntegral (lineNumber `mod` 256)]
      stmtsBytes = concatMap translateStmt lineStmts
      lengthBytes = [fromIntegral (length stmtsBytes)]
      carriageReturnByte = [0x0D] -- Carriage return byte
   in lineNumBytes ++ lengthBytes ++ stmtsBytes ++ carriageReturnByte

-- At the end of the program we add a 0xFF byte to indicate the end of the program.
translateProgram :: Program BasicType -> [Word8]
translateProgram Program {programLines} =
  concatMap translateLine programLines ++ [0xFF] -- End of program byte
