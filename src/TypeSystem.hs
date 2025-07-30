module TypeSystem
  ( BasicType (..),
    DecimalNumberRepr8 (..),
    mantissaSignToByte,
    doubleToDecimalNumberRepr8,
    defaultStringLength,
  )
where

import Data.Int (Int8)
import Data.Word (Word8)

data BasicType where
  BasicStringType :: BasicType
  BasicNumericType :: BasicType
  BasicNumArrType ::
    { numericArrSize :: Word8
    } ->
    BasicType
  BasicStrArrType ::
    { strArrSize :: Word8,
      strArrLength :: Word8
    } ->
    BasicType
  BasicNum2DArrType ::
    { num2DArrRows :: Word8,
      num2DArrCols :: Word8
    } ->
    BasicType
  BasicStr2DArrType ::
    { str2DArrRows :: Word8,
      str2DArrCols :: Word8,
      str2DArrLength :: Word8
    } ->
    BasicType
  deriving (Show, Eq)

defaultStringLength :: Word8
defaultStringLength = 16

data MantissaSign = Positive | Negative
  deriving (Show, Eq)

mantissaSignToByte :: MantissaSign -> Word8
mantissaSignToByte Positive = 0x00
mantissaSignToByte Negative = 0x80

-- With range -9.999999999*10^(99) to +9.999999999*10^(99)
data DecimalNumberRepr8 where
  DecimalNumberRepr8 ::
    { decimalNumberRepr8Exponent :: Int8,
      decimalNumberRepr8MantissaSign :: MantissaSign,
      decimalNumberRepr8Mantissa :: [Word8] -- 5 bytes
    } ->
    DecimalNumberRepr8
  deriving (Eq)

instance Show DecimalNumberRepr8 where
  show = printDecimalNumberRepr8

printDecimalNumberRepr8 :: DecimalNumberRepr8 -> String
printDecimalNumberRepr8 DecimalNumberRepr8 {decimalNumberRepr8Exponent, decimalNumberRepr8MantissaSign, decimalNumberRepr8Mantissa} =
  let signStr = case decimalNumberRepr8MantissaSign of
        Positive -> ""
        Negative -> "-"
      exponentStr = if decimalNumberRepr8Exponent >= 0 then "+" ++ show decimalNumberRepr8Exponent else show decimalNumberRepr8Exponent
      mantissaStr = concatMap (show . (fromIntegral :: Word8 -> Int)) decimalNumberRepr8Mantissa
      -- Ensure mantissa is 9 digits long, pad with zeros if necessary
      paddedMantissa =
        if length mantissaStr < 9
          then mantissaStr ++ replicate (9 - length mantissaStr) '0'
          else take 9 mantissaStr -- truncate if longer than 9 digits
   in signStr ++ paddedMantissa ++ "E" ++ exponentStr

doubleToDecimalNumberRepr8 :: Double -> DecimalNumberRepr8
doubleToDecimalNumberRepr8 d =
  let -- Extract sign, exponent, and mantissa from Double
      (sign, exponent', mantissa) = decodeDouble d
      mantissaSign = if sign < 0 then Negative else Positive
      -- Normalize mantissa to 9 digits (as per DecimalNumberRepr8 spec)
      absMantissa = abs mantissa
      mantissaStr = Prelude.take 9 $ show (round (absMantissa * (10 ^ (9 - decimalPlaces absMantissa))) :: Integer)
      mantissaBytes = map (fromIntegral . fromEnum) mantissaStr
      -- Ensure mantissaBytes is exactly 5 bytes (pad or truncate)
      mantissaBytes5 = take 5 $ mantissaBytes ++ replicate 5 0
      exponent8 = fromIntegral exponent' :: Int8
   in DecimalNumberRepr8
        { decimalNumberRepr8Exponent = exponent8,
          decimalNumberRepr8MantissaSign = mantissaSign,
          decimalNumberRepr8Mantissa = mantissaBytes5
        }

-- Helper to decode Double into sign, exponent, mantissa
decodeDouble :: Double -> (Double, Int, Double)
decodeDouble x
  | x == 0 = (1, 0, 0)
  | otherwise =
      let sign = if x < 0 then -1 else 1
          absx = abs x
          exponent' = floor (logBase 10 absx)
          mantissa = absx / (10 ^^ exponent')
       in (sign, exponent', mantissa)

-- Helper to count decimal places in mantissa
decimalPlaces :: Double -> Int
decimalPlaces x = Prelude.length $ Prelude.dropWhile (/= '.') $ show x