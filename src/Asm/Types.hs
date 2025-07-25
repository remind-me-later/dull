module Asm.Types
  ( AsmInst (..),
    AsmProgram (..),
    AsmGeneralReg8 (..),
    AsmGeneralReg16 (..),
    AsmSpecialReg8 (..),
    AsmSpecialReg16 (..),
    AsmIndirectReg16Access (..),
    AsmIndirectAccess (..),
    AsmOperand8 (..),
  )
where

import Data.Int (Int8)
import Data.Word (Word16)

data AsmGeneralReg8 where
  AsmRegXL :: AsmGeneralReg8
  AsmRegXH :: AsmGeneralReg8
  AsmRegYL :: AsmGeneralReg8
  AsmRegYH :: AsmGeneralReg8
  AsmRegUL :: AsmGeneralReg8
  AsmRegUH :: AsmGeneralReg8
  deriving (Eq)

instance Show AsmGeneralReg8 where
  show AsmRegXL = "xl"
  show AsmRegXH = "xh"
  show AsmRegYL = "yl"
  show AsmRegYH = "yh"
  show AsmRegUL = "ul"
  show AsmRegUH = "uh"

data AsmGeneralReg16 where
  AsmRegX :: AsmGeneralReg16
  AsmRegY :: AsmGeneralReg16
  AsmRegU :: AsmGeneralReg16
  deriving (Eq)

instance Show AsmGeneralReg16 where
  show AsmRegX = "x"
  show AsmRegY = "y"
  show AsmRegU = "u"

data AsmSpecialReg8
  = AsmRegSL
  | AsmRegSH
  | AsmRegPL
  | AsmRegPH
  deriving (Eq)

instance Show AsmSpecialReg8 where
  show AsmRegSL = "sl"
  show AsmRegSH = "sh"
  show AsmRegPL = "pl"
  show AsmRegPH = "ph"

data AsmSpecialReg16
  = AsmRegP -- Program Counter
  | AsmRegS -- Stack Pointer
  deriving (Eq)

instance Show AsmSpecialReg16 where
  show AsmRegP = "p"
  show AsmRegS = "s"

data AsmIndirectReg16Access
  = AsmIndirectReg16Access
  { indirectReg16 :: AsmGeneralReg16,
    isMe1 :: Bool
  }
  deriving (Eq)

instance Show AsmIndirectReg16Access where
  show (AsmIndirectReg16Access reg isMe1) = case isMe1 of
    True -> "#(" ++ show reg ++ ")"
    False -> "( " ++ show reg ++ ")"

data AsmIndirectAccess where
  AsmOperandIndirectImm16 ::
    { indirectImm16 :: Word16,
      indirectImm16isMe1 :: Bool
    } ->
    AsmIndirectAccess
  AsmOperandIndirectReg16 :: AsmIndirectReg16Access -> AsmIndirectAccess
  deriving (Eq)

instance Show AsmIndirectAccess where
  show (AsmOperandIndirectImm16 imm isMe1) = case isMe1 of
    True -> "#(" ++ show imm ++ ")"
    False -> "(  " ++ show imm ++ ")"
  show (AsmOperandIndirectReg16 access) = show access

data AsmOperand8
  = AsmOperandReg8 AsmGeneralReg8
  | AsmOperandIndirect AsmIndirectAccess
  deriving (Eq)

instance Show AsmOperand8 where
  show (AsmOperandReg8 reg) = show reg
  show (AsmOperandIndirect access) = show access

data AsmInst where
  AsmAdc :: AsmOperand8 -> AsmInst
  AsmAdi :: AsmOperand8 -> Int8 -> AsmInst
  AsmDca :: AsmIndirectReg16Access -> AsmInst
  AsmAdr :: AsmGeneralReg16 -> AsmInst
  AsmSbc :: AsmOperand8 -> AsmInst
  AsmSbiA :: Int8 -> AsmInst
  AsmDcs :: AsmIndirectReg16Access -> AsmInst
  AsmAnd :: AsmIndirectAccess -> AsmInst
  AsmAni :: AsmOperand8 -> Int8 -> AsmInst
  AsmOra :: AsmIndirectAccess -> AsmInst
  AsmOri :: AsmOperand8 -> Int8 -> AsmInst
  AsmEor :: AsmIndirectAccess -> AsmInst
  AsmEai :: Int8 -> AsmInst
  AsmInca :: AsmOperand8 -> AsmInst
  AsmIncReg8 :: AsmGeneralReg8 -> AsmInst
  AsmIncReg16 :: AsmGeneralReg16 -> AsmInst
  AsmDeca :: AsmOperand8 -> AsmInst
  AsmDecReg8 :: AsmGeneralReg8 -> AsmInst
  AsmDecReg16 :: AsmGeneralReg16 -> AsmInst
  AsmCpa :: AsmOperand8 -> AsmInst
  AsmCpiA :: Int8 -> AsmInst
  AsmCpiReg8 :: AsmGeneralReg8 -> Int8 -> AsmInst
  AsmBit :: AsmIndirectAccess -> AsmInst
  AsmBiiA :: Int8 -> AsmInst
  AsmBiiInd :: AsmIndirectAccess -> Int8 -> AsmInst
  AsmLda :: AsmOperand8 -> AsmInst
  AsmLde :: AsmGeneralReg16 -> AsmInst
  AsmLin :: AsmGeneralReg16 -> AsmInst
  AsmLdiA :: Int8 -> AsmInst
  AsmLdiReg8 :: AsmGeneralReg8 -> Int8 -> AsmInst
  AsmLdiS :: Word16 -> AsmInst
  AsmLdxReg16 :: AsmGeneralReg16 -> AsmInst
  AsmLdxRegS :: AsmGeneralReg16 -> AsmInst
  AsmLdxRegP :: AsmGeneralReg16 -> AsmInst
  AsmSta :: AsmOperand8 -> AsmInst
  AsmSde :: AsmGeneralReg16 -> AsmInst
  AsmSin :: AsmGeneralReg16 -> AsmInst
  AsmStxReg16 :: AsmGeneralReg16 -> AsmInst
  AsmStxRegS :: AsmGeneralReg16 -> AsmInst
  AsmStxRegP :: AsmGeneralReg16 -> AsmInst
  AsmPshA :: AsmInst
  AsmPshReg16 :: AsmGeneralReg16 -> AsmInst
  AsmPopA :: AsmInst
  AsmPopReg16 :: AsmGeneralReg16 -> AsmInst
  -- Jumps
  AsmSjp :: Word16 -> AsmInst
  deriving (Eq)

instance Show AsmInst where
  show (AsmAdc op) = "\tadc " ++ show op ++ "\n"
  show (AsmAdi op imm) = "\tadi " ++ show op ++ ", " ++ show imm ++ "\n"
  show (AsmDca access) = "\tdca " ++ show access ++ "\n"
  show (AsmAdr reg) = "\tadr " ++ show reg ++ "\n"
  show (AsmSbc op) = "\tsbc " ++ show op ++ "\n"
  show (AsmSbiA imm) = "\tsbi a, " ++ show imm ++ "\n"
  show (AsmDcs access) = "\tdcs " ++ show access ++ "\n"
  show (AsmAnd access) = "\tand " ++ show access ++ "\n"
  show (AsmAni op imm) = "\tani " ++ show op ++ ", " ++ show imm ++ "\n"
  show (AsmOra access) = "\tora " ++ show access ++ "\n"
  show (AsmOri op imm) = "\tori " ++ show op ++ ", " ++ show imm ++ "\n"
  show (AsmEor access) = "\teor " ++ show access ++ "\n"
  show (AsmEai imm) = "\teai " ++ show imm ++ "\n"
  show (AsmInca op) = "\tinc a " ++ show op ++ "\n"
  show (AsmIncReg8 reg) = "\tinc " ++ show reg ++ "\n"
  show (AsmIncReg16 reg) = "\tinc " ++ show reg ++ "\n"
  show (AsmDeca op) = "\tdec a " ++ show op ++ "\n"
  show (AsmDecReg8 reg) = "\tdec " ++ show reg ++ "\n"
  show (AsmDecReg16 reg) = "\tdec " ++ show reg ++ "\n"
  show (AsmCpa op) = "\tcpa " ++ show op ++ "\n"
  show (AsmCpiA imm) = "\tcpi a, " ++ show imm ++ "\n"
  show (AsmCpiReg8 reg imm) = "\tcpi " ++ show reg ++ ", " ++ show imm ++ "\n"
  show (AsmBit access) = "\tbit " ++ show access ++ "\n"
  show (AsmBiiA imm) = "\tbii a, " ++ show imm ++ "\n"
  show (AsmBiiInd access imm) = "\tbii " ++ show access ++ ", " ++ show imm ++ "\n"
  show (AsmLda op) = "\tlda " ++ show op ++ "\n"
  show (AsmLde reg) = "\tlde " ++ show reg ++ "\n"
  show (AsmLin reg) = "\tlin " ++ show reg ++ "\n"
  show (AsmLdiA imm) = "\tldi a, " ++ show imm ++ "\n"
  show (AsmLdiReg8 reg imm) = "\tldi " ++ show reg ++ ", " ++ show imm ++ "\n"
  show (AsmLdiS imm) = "\tldi s, " ++ show imm ++ "\n"
  show (AsmLdxReg16 reg) = "\tldx " ++ show reg ++ "\n"
  show (AsmLdxRegS reg) = "\tldx s, " ++ show reg ++ "\n"
  show (AsmLdxRegP reg) = "\tldx p, " ++ show reg ++ "\n"
  show (AsmSta op) = "\tsta " ++ show op ++ "\n"
  show (AsmSde reg) = "\tsde " ++ show reg ++ "\n"
  show (AsmSin reg) = "\tsin " ++ show reg ++ "\n"
  show (AsmStxReg16 reg) = "\tstx " ++ show reg ++ "\n"
  show (AsmStxRegS reg) = "\tstx s, " ++ show reg ++ "\n"
  show (AsmStxRegP reg) = "\tstx p, " ++ show reg ++ "\n"
  show AsmPshA = "\tpsh a\n"
  show (AsmPshReg16 reg) = "\tpsh " ++ show reg ++ "\n"
  show AsmPopA = "\tpop a\n"
  show (AsmPopReg16 reg) = "\tpop " ++ show reg ++ "\n"
  show (AsmSjp addr) = "\tsjp " ++ show addr ++ "\n"

data AsmProgram = AsmProgram
  { asmInstructions :: [AsmInst],
    asmBeginAddress :: Word16,
    asmStringLiteralBeginAddress :: Word16
  }
  deriving (Eq)