module VspDecodeRealdata (testToshin) where

import System35.File.Vsp

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import qualified Data.ByteString as S
import Data.Word (Word8)
import Data.Bits ((.&.), shiftL, shiftR)
import Data.List

import Control.Monad (unless, foldM)

testtest = TestCase $ assertBSEqual "test" expectedBin actualBin

-- length check
-- first 5 error parts

expectedBin :: S.ByteString
expectedBin = S.pack $ [0x00, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80]

actualBin :: S.ByteString
actualBin = S.pack $   [0x00, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x0f]

-- test with debug
assertBSEqual :: String -> S.ByteString -> S.ByteString -> Assertion
assertBSEqual preface expected actual =
    unless (actual == expected) (assertFailure msg)
        where msg = (if null preface then "" else preface ++ "\n") ++
                    "position: " ++ show pos ++ "\n" ++
                    "counting: " ++ show cnt ++ "\n" ++
                    "expected: "   ++ show (take (pos+1) (S.unpack (expected))) ++
                    "\n but got: " ++ show (take (pos+1) (S.unpack (actual)))
              pos = findErrorPos (S.unpack expected) (S.unpack actual) 0
              cnt = findErrorCnt (S.unpack expected) (S.unpack actual) 0

findErrorPos :: [Word8] -> [Word8] -> Int -> Int
findErrorPos [] [] _ = 0
findErrorPos (e:es) (a:as) x
    | (e == a)  = findErrorPos es as (x+1)
    | otherwise = x

findErrorCnt :: [Word8] -> [Word8] -> Int -> Int
findErrorCnt [] [] x = x
findErrorCnt (e:es) (a:as) x
    | (e == a)  = findErrorCnt es as x
    | otherwise = findErrorCnt es as (x+1)

es = S.unpack $ expectedBin
as = S.unpack $ actualBin

{- CG_0001.VSP
[RGB {blue = 0, red = 0, green = 0},
RGB {blue = 80, red = 80, green = 80},
RGB {blue = 112, red = 128, green = 128},
RGB {blue = 176, red = 192, green = 192},
RGB {blue = 112, red = 240, green = 208},
RGB {blue = 48, red = 160, green = 64},
RGB {blue = 80, red = 208, green = 112}
,RGB {blue = 208, red = 224, green = 224}
,RGB {blue = 64, red = 80, green = 112}
,RGB {blue = 96, red = 112, green = 160}
,RGB {blue = 160, red = 80, green = 112}
,RGB {blue = 192, red = 128, green = 160}
,RGB {blue = 64, red = 128, green = 96}
,RGB {blue = 96, red = 176, green = 128}
,RGB {blue = 144, red = 208, green = 176}
,RGB {blue = 240, red = 240, green = 240}]

00 (R,G,B) = (0,0,0)
01 (R,G,B) = (80,80,80)
02 (R,G,B) = (128,128,112)
03 (R,G,B) = (192,192,176)
04 (R,G,B) = (240,208,112)
05 (R,G,B) = (160,64,48)
06 (R,G,B) = (208,112,80)
07 (R,G,B) = (224,224,208)
08 (R,G,B) = (80,112,64)
09 (R,G,B) = (112,160,96)
10 (R,G,B) = (80,112,160)
11 (R,G,B) = (128,160,192)
12 (R,G,B) = (128,96,64)
13 (R,G,B) = (176,128,96)
14 (R,G,B) = (208,176,144)
15 (R,G,B) = (240,240,240)
-}

-- testGroup for ALD archive extraction and VSP image convertion
testToshin = testGroup "Testing real data"
             [ testCase "test for convertion (600x400)" testRealdata1
             , testCase "test for convertion (560x298)" testRealdata2
             ]

testRealdata1 = convertion
    where
      convertion :: IO ()
      convertion = do
        rawdata <- S.readFile image
        let hdr = getHeader rawdata
        let sc = convert rawdata (width hdr, height hdr)
        (length (mkRet sc)) @?= (length (expected (width hdr `div` 8, height hdr)))
            where
              expected (n,h) = take n (repeat (h,h,h,h))
              mkRet = map (\(a,b,c,d) -> (length a, length b, length c, length d))
              image = "/home/smly/gitws/sys35tools/dist/build/aldtool/vsp/CG_0001.VSP"

testRealdata2 = convertion
    where
      convertion :: IO ()
      convertion = do
        rawdata <- S.readFile image
        let hdr = getHeader rawdata
        let sc = convert rawdata (width hdr, height hdr)
        (length (mkRet sc)) @?= (length (expected (width hdr `div` 8, height hdr)))
            where
              expected (n,h) = take n (repeat (h,h,h,h))
              mkRet = map (\(a,b,c,d) -> (length a, length b, length c, length d))
              image = "/home/smly/gitws/sys35tools/dist/build/aldtool/vsp/CG_0697.VSP"