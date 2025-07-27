-- In this representation all "words" are 8 bytes long, the size of the BASIC
-- variables

module Hir.Types
  ( HirIntrinsic (..),
    HirFun (..),
    HirInst (..),
    HirProgram (..),
    HirIdent (..),
  )
where

import Data.Word (Word16)

data HirIdent where
  HirBasicIdent ::
    { hirIdentName :: Char,
      hirIdentHasDollar :: Bool
    } ->
    HirIdent
  -- These are identifiers that we include in the Hir code but that
  -- don't exist in the original source code.
  HirFakeIdent ::
    { hirFakeIdentName :: String
    } ->
    HirIdent
  deriving (Eq)

instance Show HirIdent where
  show (HirBasicIdent name hasDollar) =
    if hasDollar then [name, '$'] else [name]
  show (HirFakeIdent name) = '!' : name

data HirIntrinsic where
  HirPrintNum :: HirIntrinsic
  HirPrintStr :: HirIntrinsic
  HirUsing :: String -> HirIntrinsic
  HirInputNum :: HirIntrinsic
  HirInputStr :: HirIntrinsic
  HirGPrintNum :: HirIntrinsic
  HirGPrintStr :: HirIntrinsic
  HirClear :: HirIntrinsic
  HirCls :: HirIntrinsic
  HirRandom :: HirIntrinsic
  HirSleep :: HirIntrinsic
  HirSetPokeAddress :: HirIntrinsic
  HirPoke :: {hirPokeMemoryArea :: Int} -> HirIntrinsic
  HirCursor :: HirIntrinsic
  HirGCursor :: HirIntrinsic
  HirBeepStmt :: {hirBeepHasOptParams :: Bool} -> HirIntrinsic
  deriving (Eq)

instance Show HirIntrinsic where
  show HirPrintNum = "print_num"
  show HirPrintStr = "print_str"
  show (HirUsing usingClause) = "using_fmt(" ++ show usingClause ++ ")"
  show HirInputNum = "input_num"
  show HirInputStr = "input_str"
  show HirGPrintNum = "gprint_num"
  show HirGPrintStr = "gprint_str"
  show HirClear = "clear_vars"
  show HirCls = "cls"
  show HirRandom = "gen_random_seed"
  show HirSleep = "sleep"
  show (HirPoke memArea) =
    "poke_memory_area_" ++ show memArea
  show HirSetPokeAddress = "set_poke_address"
  show HirCursor = "set_cursor"
  show HirGCursor = "set_gcursor"
  show (HirBeepStmt hasOptParams) =
    "beep" ++ (if hasOptParams then "_with_params" else "")

data HirFun where
  -- Arithmetic operations
  HirAddOp :: HirFun
  HirSubOp :: HirFun
  HirMulOp :: HirFun
  HirDivOp :: HirFun
  HirExponentOp :: HirFun
  HirAndOp :: HirFun
  HirOrOp :: HirFun
  -- Comparison operations
  HirEqOp :: HirFun
  HirNeqOp :: HirFun
  HirLtOp :: HirFun
  HirLeqOp :: HirFun
  HirGtOp :: HirFun
  HirGeqOp :: HirFun
  -- "Special" operations
  HirMidOp :: HirFun
  HirLeftOp :: HirFun
  HirRightOp :: HirFun
  HirAsciiOp :: HirFun
  HirPointOp :: HirFun
  HirRndOp :: HirFun
  HirIntOp :: HirFun
  HirSgnOp :: HirFun
  deriving (Eq)

instance Show HirFun where
  show HirMidOp = "mid" -- TODO: docs page 126 wtf does that mean?
  show HirLeftOp = "left AL-X, 7890H"
  show HirRightOp = "right AL-X, 7890H"
  show HirAsciiOp = "ascii AL-X"
  show HirPointOp = "point" -- TODO:
  show HirRndOp = "rnd" -- TODO:
  show HirIntOp = "int AL-X"
  show HirSgnOp = "sgn AL-X"
  show HirAddOp = "add AL-X AL-Y"
  show HirSubOp = "sub AL-X AL-Y"
  show HirMulOp = "mul AL-X AL-Y"
  show HirDivOp = "div AL-X AL-Y"
  show HirExponentOp = "exp AL-X AL-Y"
  show HirAndOp = "and AL-X AL-Y"
  show HirOrOp = "or AL-X AL-Y"
  show HirEqOp = "eq AL-X AL-Y"
  show HirNeqOp = "neq AL-X AL-Y"
  show HirLtOp = "lt AL-X AL-Y"
  show HirLeqOp = "leq AL-X AL-Y"
  show HirGtOp = "gt AL-X AL-Y"
  show HirGeqOp = "geq AL-X AL-Y"

type Label = Int

data HirInst where
  HirLdImmIntoAlX :: Double -> HirInst
  HirLdImmIntoAlY :: Double -> HirInst
  HirLdImmIndirectIntoAlX :: Word16 -> HirInst
  HirLdImmIndirectIntoAlY :: Word16 -> HirInst
  HirAddrInUregIntoAlX :: HirInst
  HirStAlXInUreg :: HirInst
  HirAddrInAlXToUreg :: HirInst
  HirAlXToAlY :: HirInst
  HirAlYToAlX :: HirInst
  HirImmToUreg :: Word16 -> HirInst
  HirFun :: HirFun -> HirInst
  -- Labels
  HirLabel :: Label -> HirInst
  -- Jumps
  HirGoto :: Label -> HirInst
  HirCall :: Label -> HirInst
  HirCondGoto :: Label -> HirInst
  HirCondCall :: Label -> HirInst
  HirReturn :: HirInst
  -- Intrinsics
  HirIntrinsicCall :: HirIntrinsic -> HirInst
  deriving (Eq)

instance Show HirInst where
  show (HirLdImmIntoAlX num) =
    "\tAL-X = " ++ show num
  show (HirLdImmIntoAlY num) =
    "\tAL-Y = " ++ show num
  show (HirLdImmIndirectIntoAlX addr) =
    "\tAL-X = (" ++ show addr ++ ")"
  show (HirLdImmIndirectIntoAlY addr) =
    "\tAL-Y = (" ++ show addr ++ ")"
  show HirStAlXInUreg = "\t(Ureg) = AL-X"
  show HirAddrInAlXToUreg = "\tUreg = (AL-X)"
  show HirAddrInUregIntoAlX = "\tAL-X = (Ureg)"
  show (HirImmToUreg ident) =
    "\tUreg = " ++ show ident
  show HirAlXToAlY = "\tAL-Y = AL-X"
  show HirAlYToAlX = "\tAL-X = AL-Y"
  show (HirLabel idx) = "L" ++ show idx ++ ":"
  show (HirGoto idx) = "\tgoto L" ++ show idx
  show (HirCall idx) = "\tcall L" ++ show idx
  show (HirCondGoto idx) = "\tif Z goto L" ++ show idx
  show (HirCondCall idx) = "\tif Z call L" ++ show idx
  show HirReturn = "\treturn"
  show (HirIntrinsicCall intrinsic) = "\t@" ++ show intrinsic
  show (HirFun op) = "\t" ++ show op

newtype HirProgram = HirProgram
  { hirProgramStatements :: [HirInst]
  }
  deriving (Eq)

instance Show HirProgram where
  show (HirProgram stmts) = unlines (map show stmts)