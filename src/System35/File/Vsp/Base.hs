module System35.File.Vsp.Base where
import qualified Data.ByteString as S
import Data.Word (Word8)

type PalletNum = Int
type Pallet    = [RGB]

data RGB = RGB { blue :: Word8, red :: Word8, green :: Word8 }

instance Show RGB where
    show dat = "("++ show (blue  dat) ++ ","
                  ++ show (red   dat) ++ ","
                  ++ show (green dat) ++ ")"

data Header = Header
    { x           :: Int
    , y           :: Int
    , width       :: Int
    , height      :: Int
    , rsv         :: Int
    , palette     :: [RGB] }


data ByteBlock = ByteBlock
    { code  :: Word8,
      bytes :: S.ByteString,
      len   :: Int } deriving (Show, Eq)

type Screen = [Line]
 -- 8 pixsel width, 4 plain, HEIGHT length list
type Line   = (PlainLine, PlainLine, PlainLine, PlainLine)
type PlainLine = [Word8]
