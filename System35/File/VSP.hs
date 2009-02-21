module System35.File.VSP (
  getHeader, Header(..),
) where

import qualified Data.ByteString as S
import Data.Word (Word8)

data Color = Color
    { blue  :: Word8
    , red   :: Word8
    , green :: Word8 }

data Header = Header
    { x           :: Int
    , y           :: Int
    , width       :: Int
    , height      :: Int
    , rsv         :: Int
    , paletteBank :: Int
    , palette     :: [Color] }

getHeader :: [S.ByteString] -> [Header]
getHeader = map getHeader'
    where
      getHeader' :: S.ByteString -> Header
      getHeader' s = Header
                     { x = 0
                     , y = 0
                     , width = 640
                     , height = 400
                     , rsv = 0
                     , paletteBank = 0
                     , palette = colors }
          where colors = getPalette s

getPalette :: S.ByteString -> [Color]
getPalette s = [Color
                { blue  = 0x00
                , red   = 0x00
                , green = 0x00 }]
