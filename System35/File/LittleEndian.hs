module System35.File.LittleEndian (
  get4BWord32, get3BWord32, Word32,
  get2BWord16, Word16,
) where

import qualified Data.ByteString as S
import Data.Word (Word32, Word16)
import Data.Bits (shiftL)

get4BWord32 :: S.ByteString -> Word32
get4BWord32 s = fromInteger $ toInteger $
                shiftL byte4 24 +
                shiftL byte3 16 +
                shiftL byte2 8 +
                byte1
    where byte1, byte2, byte3, byte4 :: Word32
          [byte1, byte2, byte3, byte4] =
              map fromIntegral $ S.unpack (S.take 4 s)

get3BWord32 :: S.ByteString -> Word32
get3BWord32 s = fromInteger $ toInteger $
                shiftL byte3 16 +
                shiftL byte2 8 +
                byte1
    where byte1, byte2, byte3 :: Word32
          [byte1, byte2, byte3] =
              map fromIntegral $ S.unpack (S.take 3 s)

get2BWord16 :: S.ByteString -> Word16
get2BWord16 s = fromInteger $ toInteger $
                shiftL byte2 8 +
                byte1
    where byte1, byte2 :: Word32
          [byte1, byte2] =
              map fromIntegral $ S.unpack (S.take 2 s)
