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
      len   :: Int,
      sz    :: Int }
-- len :: rawdata bytes length (0x07,0x01 == [len=1,sz=2])
-- sz  :: byteblock length ((0x9f) == [len=1,sz=1])

instance Show ByteBlock where
    show b = "{f" ++ show (code b) ++
             ",l" ++ show (len b) ++
             ",s" ++ show (sz b) ++ "}"
instance Eq ByteBlock where
    x == y = (len x `div` sz x) == (len y `div` sz y)
instance Ord ByteBlock where
    a < b  = (fromIntegral (len a) / fromIntegral (sz a)) <
                       (fromIntegral (len b) / fromIntegral (sz b))
    a > b  = (fromIntegral (len a) / fromIntegral (sz a)) >
                       (fromIntegral (len b) / fromIntegral (sz b))
    a <= b = (fromIntegral (len a) / fromIntegral (sz a)) <=
                       (fromIntegral (len b) / fromIntegral (sz b))
    a >= b = (fromIntegral (len a) / fromIntegral (sz a)) >=
                       (fromIntegral (len b) / fromIntegral (sz b))

type Screen = [Line]
 -- 8 pixsel width, 4 plain, HEIGHT length list
type Line   = (PlainLine, PlainLine, PlainLine, PlainLine)
type PlainLine = [Word8]
