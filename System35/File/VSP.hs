module System35.File.VSP (
  getHeader, Header(..),
) where

import System35.File.LittleEndian (get2BWord16)
import qualified Data.ByteString as S
import Data.Bits (shiftL)
import Data.Word (Word8, Word16)

data Color = Color
    { blue  :: Word8
    , red   :: Word8
    , green :: Word8 } deriving (Show)

data Header = Header
    { x           :: Int
    , y           :: Int
    , width       :: Int
    , height      :: Int
    , rsv         :: Int
    , palette     :: [Color] }

getHeader :: [S.ByteString] -> [Header]
getHeader = map getHeader'

getHeader' :: S.ByteString -> Header
getHeader' s = Header
               { x = x
               , y = y
               , width = shiftL (w-x) 3
               , height = h-y
               , rsv = fromIntegral.get2BWord16 $ S.drop 8 s
               , palette = colors }
    where
      colors    = getPalette $ S.drop 10 s
      [x,y,w,h] = map (fromIntegral.get2BWord16.(\e -> S.drop e s)) [0,2,4,6]

getPalette :: S.ByteString -> [Color]
getPalette s = map mkPalette [0..15]
    where
      mkPalette :: Int -> Color
      mkPalette n = Color { blue  = fromIntegral $ shiftL byte1 4
                          , red   = fromIntegral $ shiftL byte2 4
                          , green = fromIntegral $ shiftL byte3 4 }
          where
            palet = S.unpack $ S.take 3 $ S.drop (10+n*3) s
            byte1, byte2, byte3 :: Word8
            [byte1, byte2, byte3] = map fromIntegral palet
