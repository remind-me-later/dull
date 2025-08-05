module ComHeader where

import Data.Word (Word8)

headerStartMagicNumber :: Word8
headerStartMagicNumber = 0x01

prependComHeader :: [Word8] -> String -> [Word8]
prependComHeader bytes programName =
  -- Header is 27 bytes long it consists of:
  -- 1 byte for the magic number (0x01)
  -- 4 bytes for the file type, magic number, for binary BASIC files it's "@COM"
  -- 16 bytes for the filename, if name is shorter than 16 bytes, it is padded with NULL (0x00)
  -- 2 bytes for start address, we will use standard user memory (0x4000-0x57FF), 6KB, if program is
  -- larger than that error
  -- 2 bytes for length of the program
  -- 2 bytes for entry address (0x4000)
  let fileType = map (fromIntegral . fromEnum) "@COM"
      paddedName = take 16 (programName ++ replicate 16 '\0')
      startAddress = [0x00, 0x40] -- Standard user memory start
      length' = [fromIntegral (length bytes `div` 256), fromIntegral (length bytes `mod` 256)]
      entryAddress = [0x00, 0x40] -- Entry point at 0x4000
      header =
        [headerStartMagicNumber]
          ++ fileType
          ++ map (fromIntegral . fromEnum) paddedName
          ++ startAddress
          ++ length'
          ++ entryAddress
   in header ++ bytes
