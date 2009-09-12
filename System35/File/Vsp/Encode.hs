module System35.File.Vsp.Encode where

import System35.File.Vsp.Base
import System35.File.Vsp.Pixel
import qualified Data.ByteString as S
import Data.List (group)
import Data.Word

-- [red plain, g, b, i, red plain, g, b, i, ...]
-- list length = 4 * (width / 8)
encode :: Screen -> [ByteBlock]
encode (x:xs) = encodeLine x (empty,empty) ++ encode' xs x
    where empty = ([],[],[],[])
          encode' :: Screen -> Line -> [ByteBlock]
          encode' [] prev = []
          encode' (y:ys) prev = encodeLine y (prev,empty) ++ encode' ys y

type Dict = (Line, Line)
data PlainType = B | R | G | I

encodeLine :: Line -> Dict -> [ByteBlock]
encodeLine (pl0,pl1,pl2,pl3) (prev,curr) = concat $ [blocks0,blocks1,blocks2,blocks3]
    where
      blocks0 = encodePlainLineOne pl0 0 B (prev,([], [], [], []))
      blocks1 = encodePlainLineOne pl1 0 R (prev,(pl0,[], [], []))
      blocks2 = encodePlainLineOne pl2 0 G (prev,(pl0,pl1,[], []))
      blocks3 = encodePlainLineOne pl3 0 I (prev,(pl0,pl1,pl2,[]))

encodePlainLineOne :: PlainLine -> Int -> PlainType -> Dict -> [ByteBlock]
encodePlainLineOne pl y ptype dict
    | length pl <= y = []
    | otherwise      = block : encodePlainLineOne pl (y+len block) ptype dict
    where block = takeBestBlock pl y ptype dict
-- testimage: 16x4

takeBestBlock :: PlainLine -> Int -> PlainType -> Dict -> ByteBlock
takeBestBlock pl y B ((b,_,_,_),curr) = selectBestBlock pl y b curr
takeBestBlock pl y R ((_,r,_,_),curr) = selectBestBlock pl y r curr
takeBestBlock pl y G ((_,_,g,_),curr) = selectBestBlock pl y g curr
takeBestBlock pl y I ((_,_,_,i),curr) = selectBestBlock pl y i curr

-- curr (r,g,b,_) and masked (r,g,b,_)
data DictType = Prev | CurrR | CurrB | CurrG

-- selectBestBlock testpl 0 testline
selectBestBlock :: PlainLine -> Int -> PlainLine -> Line -> ByteBlock
selectBestBlock pl y prev (cb,cr,cg,ci) = maximum [ buildBlock pl y prev Prev  -- 0x00
                                               , buildRunBlock pl y         -- 0x01
                                               , buildDoubleRunBlock pl y   -- 0x02
                                               , buildBlock pl y cb CurrB -- 0x03
                                               , buildBlock pl y cr CurrR -- 0x04
                                               , buildBlock pl y cg CurrG -- 0x05
                                                 -- add masked plain
                                               , buildDirectBlock pl y    -- 0x07, 0x08~0xff
                                               ] -- ignore mask now

buildDirectBlock :: PlainLine -> Int -> ByteBlock
buildDirectBlock pl y
    | target > 0x07 = mkToken target (S.pack [target]) 1 1
    | otherwise     = mkToken 0x07 (S.pack [0x07, target]) 1 2
    where target = head pl

type Flag = Word8
type ByteLength = Int
type TokenSize  = Int
mkToken :: Flag -> S.ByteString -> ByteLength -> TokenSize -> ByteBlock
mkToken flag dat l tokensz = ByteBlock { code = flag, bytes = dat, len = l, sz = tokensz }

buildRunBlock :: PlainLine -> Int -> ByteBlock
buildRunBlock pl y = runLength target (head target)
    where
      target = drop y pl
      runLength :: PlainLine -> Word8 -> ByteBlock
      runLength (p:ps) w
                   | p == w    = case l <= 255 of
                                   True  -> ByteBlock { code = 0x01
                                                      , bytes = S.pack (take l (repeat w))
                                                      , len = l
                                                      , sz = 3 }
                                   False -> ByteBlock { code = 0x01
                                                      , bytes = S.pack (take 255 (repeat w))
                                                      , len = 255
                                                      , sz = 3 }
                   | otherwise = ByteBlock { code = 0x01
                                           , bytes = S.empty
                                           , len = -1
                                           , sz = 3 }  --dummy
          where l = (length . head . group) target

buildDoubleRunBlock :: PlainLine -> Int -> ByteBlock
buildDoubleRunBlock pl y
    | length target >= 2 = runLength target pat
    | otherwise          = ByteBlock { code = 0x02
                                     , bytes = S.empty
                                     , len = -1
                                     , sz = 4 } -- dummy
    where
      target = drop y pl
      pat    = (\(x:y:z) -> (x,y)) target
      runLength :: PlainLine -> (Word8,Word8) -> ByteBlock
      runLength (p0:p1:px) (w0,w1)
          | p0==w0 && p1==w1 = case l <= 255*2 of
                                 True  -> ByteBlock { code  = 0x02
                                                    , bytes = (S.pack.(take l).concat.repeat) [w0,w1]
                                                    , len   = l
                                                    , sz    = 4 }
                                 False -> ByteBlock { code  = 0x02
                                                    , bytes = (S.pack.(take l).concat.repeat) [w0,w1]
                                                    , len   = 255*2
                                                    , sz    = 4 }
          | otherwise        = ByteBlock { code  = 0x02
                                         , bytes = S.empty
                                         , len   = -1
                                         , sz    = 4 } -- dummy
          where l = ((*2) . length . head . group . resizeDouble) target

resizeDouble :: PlainLine -> [[Word8]]
resizeDouble []       = []
resizeDouble [x]      = []
resizeDouble (x:y:xs) = [x,y] : resizeDouble xs

buildBlock :: PlainLine -> Int -> PlainLine -> DictType -> ByteBlock
buildBlock pl y cr dtype = case dtype of
                             Prev  -> ByteBlock { code  = 0x00
                                                , bytes = S.pack matchbytes
                                                , len   = matchlen
                                                , sz    = 2 }
                             CurrB -> ByteBlock { code  = 0x03
                                                , bytes = S.pack matchbytes
                                                , len   = matchlen
                                                , sz    = 2 }
                             CurrR -> ByteBlock { code  = 0x04
                                                , bytes = S.pack matchbytes
                                                , len   = matchlen
                                                , sz    = 2 }
                             CurrG -> ByteBlock { code  = 0x05
                                                , bytes = S.pack matchbytes
                                                , len   = matchlen
                                                , sz    = 2 }
    where (matchlen, matchbytes) = matchLength (drop y pl) (drop y cr)

matchLength :: (Eq a) => [a] -> [a] -> (Int, [a])
matchLength prev curr = (len, take len prev)
    where len = match prev curr
          match :: (Eq a) => [a] -> [a] -> Int
          match [] _ = 0
          match _ [] = 0
          match (x:xs) (y:ys)
              | x == y    = 1 + (match xs ys)
              | otherwise = 0
