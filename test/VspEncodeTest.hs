module VspEncodeTest (tests) where

import System35.File.Vsp
import System35.Test.Base (assertEqual')
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import qualified Data.ByteString as S
import Data.Word (Word8)
import Data.Bits ((.&.), shiftL, shiftR)
import Control.Monad (unless, foldM)

main = defaultMain tests

tests = [ testGroup "Testing encode"
          [ testCase "test for encode (8x400) plain image" testEncode0 ]
        ]

testEncode0 = assertEqual' "test" expectedBin actualBin
expectedBin = S.pack $ [0x00, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0xff, 0xff]
actualBin   = S.pack $ [0x00, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0xff, 0xff]

-- test with debug
