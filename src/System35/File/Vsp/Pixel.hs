module System35.File.Vsp.Pixel where

import System35.File.Vsp.Base
import System35.File.LittleEndian (get2BWord16)
import qualified Data.ByteString as S
import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor)
import Data.Word (Word8)

toRGB :: [PalletNum] -> Pallet -> [RGB]
toRGB [] pal     = []
toRGB (x:xs) pal = col x pal : toRGB xs pal
    where
      col x pal = head $ drop x pal

toRGB' :: [(Word8, Word8, Word8, Word8)] -> [PalletNum]
toRGB' []                 = []
toRGB' ((b0,b1,b2,b3):bs) = b ++ toRGB' bs
    where
      r = shiftR
      l = shiftL
      b = map fromIntegral (l0:l1:l2:l3:l4:l5:l6:l7:[])
      l0 = (r b0 7 .&. 0x01) .|. (r b1 6 .&. 0x02) .|. (r b2 5 .&. 0x04) .|. (r b3 4 .&. 0x08)
      l1 = (r b0 6 .&. 0x01) .|. (r b1 5 .&. 0x02) .|. (r b2 4 .&. 0x04) .|. (r b3 3 .&. 0x08)
      l2 = (r b0 5 .&. 0x01) .|. (r b1 4 .&. 0x02) .|. (r b2 3 .&. 0x04) .|. (r b3 2 .&. 0x08)
      l3 = (r b0 4 .&. 0x01) .|. (r b1 3 .&. 0x02) .|. (r b2 2 .&. 0x04) .|. (r b3 1 .&. 0x08)
      l4 = (r b0 3 .&. 0x01) .|. (r b1 2 .&. 0x02) .|. (r b2 1 .&. 0x04) .|. (  b3   .&. 0x08)
      l5 = (r b0 2 .&. 0x01) .|. (r b1 1 .&. 0x02) .|. (  b2   .&. 0x04) .|. (l b3 1 .&. 0x08)
      l6 = (r b0 1 .&. 0x01) .|. (  b1   .&. 0x02) .|. (l b2 1 .&. 0x04) .|. (l b3 2 .&. 0x08)
      l7 = (  b0   .&. 0x01) .|. (l b1 1 .&. 0x02) .|. (l b2 2 .&. 0x04) .|. (l b3 3 .&. 0x08)

toPalletNum' :: Screen -> [(Word8, Word8, Word8, Word8)]
toPalletNum' []             = []
toPalletNum' (([],_,_,_):xs) = toPalletNum' xs
toPalletNum' ((a:as,b:bs,c:cs,d:ds):xs) = (a,b,c,d) : toPalletNum' ((as,bs,cs,ds):xs)

toPalletNum :: Screen -> Pallet -> [RGB]
toPalletNum = toRGB . toRGB' . toPalletNum'

getHeader :: S.ByteString -> Header
getHeader s = Header
               { x = x
               , y = y
               , width = shiftL (w-x) 3
               , height = h-y
               , rsv = fromIntegral . get2BWord16 $ S.drop 8 s
               , palette = colors }
    where
      colors         = getPalette s
      takeHeaderElem = fromIntegral . get2BWord16 . (\e -> S.drop e s)
      [x,y,w,h]      = map takeHeaderElem [0,2,4,6]

getPalette :: S.ByteString -> [RGB]
getPalette s = map mkPalette [0..15]
    where
      mkPalette :: Int -> RGB
      mkPalette n = RGB { blue  = fromIntegral $ shiftL byte1 4
                        , red   = fromIntegral $ shiftL byte2 4
                        , green = fromIntegral $ shiftL byte3 4 }
          where
            getPaletteArea = S.unpack . (S.take 3) . (S.drop (0x0a + n*3))
            byte1, byte2, byte3 :: Word8
            [byte1, byte2, byte3] = map fromIntegral $ getPaletteArea s
