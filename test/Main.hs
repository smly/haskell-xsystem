module Main (main) where

import qualified VspDecodeTest (tests)
import qualified VspDecodeRealdata (tests)
import qualified VspEncodeTest (tests)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

main = defaultMain tests
tests = VspDecodeTest.tests ++
        VspDecodeRealdata.tests ++
        VspEncodeTest.tests