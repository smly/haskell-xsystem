module System35.File.Vsp.Decode (
  decode, convert, tokenize, getImage,
) where

import System35.File.Vsp.Base
import System35.File.Vsp.Pixel
import System35.File.LittleEndian (get2BWord16)
import qualified Data.ByteString as S
import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor)
import Data.Word (Word8, Word16)

getImage :: S.ByteString -> ((Int, Int), [RGB])
getImage s = (sz, pxs)
    where
      pxs = toPalletNum sc (palette hdr)
      hdr = getHeader s
      sc  = convert s sz
      sz  = (width hdr, height hdr)

convert :: S.ByteString -> (Int, Int) -> Screen
convert = decode . tokenize . (S.drop 0x3a)

decode :: [ByteBlock] -> (Int, Int) -> Screen
decode bb (w, h) = take x $ readScreen bb initLine h
    where
      x = w `div` 8
      initLine = ([], [], [], [])

readScreen :: [ByteBlock] -> Line -> Int -> Screen
readScreen bb prev h = curr : (readScreen (drop sz bb) curr h)
    where (sz, curr) = readLine bb prev h

readLine :: [ByteBlock] -> Line -> Int -> (Int, Line)
readLine bbx prev h = (sk0+sk1+sk2+sk3, (p0, p1, p2, p3)) -- (skipSz, currPlain)
    where
      (sk0, p0) = readByteBlock bbx prev h ([],[],[],[]) 0 0x00
      (sk1, p1) = readByteBlock (drop sk0 bbx) prev h (p0,[],[],[]) 1 0x00
      (sk2, p2) = readByteBlock (drop (sk0+sk1) bbx) prev h (p0,p1,[],[]) 2 0x00
      (sk3, p3) = readByteBlock (drop (sk0+sk1+sk2) bbx) prev h (p0,p1,p2,[]) 3 0x00

-- (Int, PlainLine) = skipByteBlock, resultPlainLine
readByteBlock :: [ByteBlock] -> Line -> Int -> Line -> Int -> Word8 -> (Int, PlainLine)
readByteBlock bb pl h cl ptype mask = (length pl', concat pl')
    where pl' = readByteBlock' bb pl 0 h cl ptype mask

readByteBlock' :: [ByteBlock] -> Line -> Int -> Int -> Line -> Int -> Word8 -> [PlainLine]
readByteBlock' [] _ _ _ _ _ _ = []
readByteBlock' (b:bx) prev y height cl ptype mask
    | y >= height     = []
    | opcode == 0x00 = (copyPlain prev ptype y l) :
                       (readByteBlock' bx prev (y+l) height cl ptype mask)
    | opcode == 0x01 = (take l (repeat (S.index block 2))) :
                       (readByteBlock' bx prev (y+l) height cl ptype mask)
    | opcode == 0x02 = (take l (concat (repeat [S.index block 2, S.index block 3]))) :
                       (readByteBlock' bx prev (y+l) height cl ptype mask)
    | opcode == 0x03 = map (xor mask) (copyPlain cl 0 y l) :
                       (readByteBlock' bx prev (y+l) height cl ptype 0x00)
    | opcode == 0x04 = map (xor mask) (copyPlain cl 1 y l) :
                       (readByteBlock' bx prev (y+l) height cl ptype 0x00)
    | opcode == 0x05 = map (xor mask) (copyPlain cl 2 y l) :
                       (readByteBlock' bx prev (y+l) height cl ptype 0x00)
    | opcode == 0x06 = [] : readByteBlock' bx prev y height cl ptype 0xff
    | opcode == 0x07 = [S.index block 1] :
                       (readByteBlock' bx prev (y+l) height cl ptype mask)
    | otherwise      = [opcode] :
                       (readByteBlock' bx prev (y+l) height cl ptype mask)
    where
      opcode = code b
      block  = bytes b
      l      = len b
      copyPlain prev 0 y l = take l $ drop y ((\(e,_,_,_)->e) prev)
      copyPlain prev 1 y l = take l $ drop y ((\(_,e,_,_)->e) prev)
      copyPlain prev 2 y l = take l $ drop y ((\(_,_,e,_)->e) prev)
      copyPlain prev 3 y l = take l $ drop y ((\(_,_,_,e)->e) prev)

tokenize :: S.ByteString -> [ByteBlock]
tokenize d
    | (S.null d) = []
    | otherwise  = block : tokenize (S.drop skip d)
    where
      (block, skip) = (token, sz token)
      token = getToken d
-- (data, opcode) -> (block, rest)
getToken :: S.ByteString -> ByteBlock
getToken d
    | opcode == 0x00 = ByteBlock { code = 0x00
                                 , bytes = S.take 2 d
                                 , len = getL d
                                 , sz = 2 } -- 0x00, len
    | opcode == 0x01 = ByteBlock { code = 0x01
                                 , bytes = S.take 3 d
                                 , len = getL d
                                 , sz = 3 } -- 0x01, len, pat
    | opcode == 0x02 = ByteBlock { code = 0x02
                                 , bytes = S.take 4 d
                                 , len = 2 * getL d
                                 , sz = 4 }
    | opcode == 0x03 = ByteBlock { code = 0x03
                                 , bytes = S.take 2 d
                                 , len = getL d
                                 , sz = 2 }
    | opcode == 0x04 = ByteBlock { code = 0x04
                                 , bytes = S.take 2 d
                                 , len = getL d
                                 , sz = 2 }
    | opcode == 0x05 = ByteBlock { code = 0x05
                                 , bytes = S.take 2 d
                                 , len = getL d
                                 , sz = 2 }
    | opcode == 0x06 = ByteBlock { code = 0x06
                                 , bytes = S.take 1 d
                                 , len = 0
                                 , sz = 1 }
    | opcode == 0x07 = ByteBlock { code = 0x07
                                 , bytes = S.take 2 d
                                 , len = 1
                                 , sz = 2 }
    | otherwise      = ByteBlock { code = opcode
                                 , bytes = S.empty
                                 , len = 1
                                 , sz = 1 }
    where
      opcode :: Word8
      opcode = S.head d
      getL = (+ 1) . fromIntegral . (\d -> S.index d 1)
