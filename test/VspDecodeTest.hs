module Main (main) where
import System35.File.Vsp
--import VspDecodeRealdata
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import qualified Data.ByteString as S
import Data.Word (Word8, Word16)
import Data.Bits ((.&.), shiftL, shiftR)
import Data.List

main = defaultMain tests

tests = [ {- testGroup for tokenize -}
          testGroup "Testing tokenizer"
          [ testCase "test for reading rowdata" testReadRowdata0
          , testCase "test for reading rowdata (8x400)" testReadRowdata1
          ]
        , testGroup "Testing decode flags"
          [ testCase "flag 0x00" test0
          , testCase "flag 0x01" test1
          , testCase "flag 0x02" test2
          ]
        , testGroup "Testing decoder" -- testGroup for eval
          [ testCase "test for eval blocks" testEvalBlock0
          , testCase "test for eval blocks (16x400)" testEvalData0
--          , testCase "test for eval blocks (24x400)" testEvalData1 -- check XOR
          ]
        {- download toshin2, extract ald archive,
           and convert vsp image CG_0001.VSP -}
--        , testToshin
        ]

type ImageSize = (Width, Height)
type Width = Int
type Height = Int

testSize :: ImageSize
testSize = (16, 16)

repeat1 k x = take k (repeat x) -- for 0x01
repeat2 k (x1,x2) = concat $ take k (repeat [x1,x2]) -- for 0x02

testReadRowdata0 = ret3 @?= testBlocks0
    where
      ret3 = tokenize rowdata
      rowdata = S.pack [0x01, 0x07, 0x03, 0x02, 0xc3, 0x00, 0x00]

      testBlocks0 :: [ByteBlock]
      testBlocks0 = [ ByteBlock { code = 0x01
                                , bytes = S.pack [0x01, 0x07, 0x03]
                                , len = 8
                                , sz = 3 }
                    , ByteBlock { code = 0x02
                                , bytes = S.pack [0x02, 0xc3, 0x00, 0x00]
                                , len = 392
                                , sz = 4 }
                    ]

testReadRowdata1 = ret @?= expected
    where
      ret = tokenize rowdata
      rowdata = S.pack [0x01,0x07,0x03,  0x02,0xc3,0x00,0x00,
                        0x01,0x06,0x01,  0x02,0xc3,0x00,0x00,  0x07,0x00,
                        0x02,0xc7,0x00,0x00,  0x02,0xc7,0x00,0x00]
      expected = [ ByteBlock { code = 0x01
                             , bytes = S.pack [0x01,0x07,0x03]
                             , len = 8
                             , sz = 3 }
                 , ByteBlock { code = 0x02
                             , bytes = S.pack [0x02,0xc3,0x00,0x00]
                             , len = 392
                             , sz = 4 }
                 , ByteBlock { code = 0x01
                             , bytes = S.pack [0x01,0x06,0x01]
                             , len = 7
                             , sz = 3 }
                 , ByteBlock { code = 0x02
                             , bytes = S.pack [0x02,0xc3,0x00,0x00]
                             , len = 392
                             , sz = 4 }
                 , ByteBlock { code = 0x07
                             , bytes = S.pack [0x07,0x00]
                             , len = 1
                             , sz = 2 }
                 , ByteBlock { code = 0x02
                             , bytes = S.pack [0x02, 0xc7,0x00,0x00]
                             , len = 400
                             , sz = 4 }
                 , ByteBlock { code = 0x02
                             , bytes = S.pack [0x02, 0xc7,0x00,0x00]
                             , len = 400
                             , sz = 4 }
                 ]

testEvalBlock0 = ret @?= expected
    where
      ret = decode blockdata (8, 400)
      blockdata = [ ByteBlock { code = 0x01
                              , bytes = S.pack [0x01,0x07,0x03]
                              , len = 8
                              , sz = 3 }
                  , ByteBlock { code = 0x02
                              , bytes = S.pack [0x02,0xc3,0x00,0x00]
                              , len = 392
                              , sz = 4 }
                  , ByteBlock { code = 0x01
                              , bytes = S.pack [0x01,0x06,0x01]
                              , len = 7
                              , sz = 3 }
                  , ByteBlock { code = 0x02
                              , bytes = S.pack [0x02,0xc3,0x00,0x00]
                              , len = 392
                              , sz = 4 }
                  , ByteBlock { code = 0x07
                              , bytes = S.pack [0x07,0x00]
                              , len = 1
                              , sz = 2 }
                  , ByteBlock { code = 0x02
                              , bytes = S.pack [0x02, 0xc7,0x00,0x00]
                              , len = 400
                              , sz = 4 }
                  , ByteBlock { code = 0x02
                              , bytes = S.pack [0x02, 0xc7,0x00,0x00]
                              , len = 400
                              , sz = 4 }
                  ]

      expected :: Screen
      expected = [ ( concat [repeat1 8 0x03, repeat2 196 (0x00,0x00)]
                   , concat [repeat1 7 0x01, repeat2 196 (0x00,0x00), [0x00]]
                   , concat [repeat2 200 (0x00,0x00)]
                   , concat [repeat2 200 (0x00,0x00)] ) ]


testEvalData0 = ret @?= expected
    where
      ret = decode blockdata (16, 400)
      blockdata = tokenize rowdata
      rowdata = S.pack [ 0x01, 0x07, 0x03, 0x02, 0xc3, 0x00, 0x00, 0x01, 0x06, 0x01
                       , 0x02, 0xc3, 0x00, 0x00, 0x07, 0x00, 0x02, 0xc7, 0x00, 0x00
                       , 0x02, 0xc7, 0x00, 0x00, 0x02, 0x03, 0x00, 0xff, 0xa5, 0x02
                       , 0x90, 0xe7, 0xe7, 0xe0, 0xe0,  0xe7, 0xe7, 0x01, 0x59, 0xe6
                       , 0xe7, 0xe7, 0xe0, 0xe0, 0xff, 0xff, 0xff, 0x01, 0x06, 0xff
                       , 0x07, 0x00, 0x02, 0x8f, 0x42, 0x42, 0x42, 0x43, 0x40, 0x40
                       , 0x40, 0x40, 0x43, 0x01, 0x59, 0x42, 0x43, 0x40, 0x40, 0x40
                       , 0x40, 0x7f, 0x07, 0x00, 0x02, 0xc7, 0x00, 0x00, 0x02, 0xc7
                       , 0x00, 0x00 ]

      expected = [ ( concat [ repeat1 8 0x03, repeat2 196 (0x00,0x00) ]
                   , concat [ repeat1 7 0x01, repeat2 196 (0x00,0x00), [0x00] ]
                   , concat [ repeat2 200 (0x00,0x00) ]
                   , concat [ repeat2 200 (0x00,0x00) ] )
                 , ( concat [ repeat2 4 (0x00,0xff), [0xa5], repeat1 290 0xe7
                            , [0xe0, 0xe0, 0xe7, 0xe7], repeat1 90 0xe6
                            , [0xe7, 0xe7, 0xe0, 0xe0, 0xff, 0xff, 0xff] ]
                   , concat [ repeat1 7 0xff, [0x00], repeat1 288 0x42
                            , [0x42, 0x43, 0x40, 0x40, 0x40, 0x40, 0x43]
                            , repeat1 90 0x42
                            , [0x43, 0x40, 0x40, 0x40, 0x40, 0x7f, 0x00] ]
                   , concat [ repeat2 200 (0x00,0x00) ]
                   , concat [ repeat2 200 (0x00,0x00) ] ) ]

testEvalData1 = ret @?= expected
    where
      ret = decode blockdata (24, 400)
      blockdata = tokenize rawdata
      rawdata = S.pack [ 0x01, 0x07, 0x03, 0x02, 0xc3, 0x00, 0x00, 0x01, 0x06, 0x01, 0x02, 0xc3
                       , 0x00, 0x00, 0x07, 0x00, 0x02, 0xc7, 0x00, 0x00, 0x02, 0xc7, 0x00, 0x00
                       , 0x02, 0x03, 0x00, 0xff, 0xa5, 0x02, 0x90, 0xe7, 0xe7, 0xe0, 0xe0, 0x01
                       , 0x5d, 0xe7, 0xe0, 0xe0, 0xff, 0xff, 0xff, 0x01, 0x06, 0xff, 0x07, 0x00
                       , 0x02, 0x8f, 0x42, 0x42, 0x42, 0x43, 0x40, 0x40, 0x40, 0x40, 0x43, 0x01
                       , 0x59, 0x42, 0x43, 0x40, 0x40, 0x40, 0x40, 0x7f, 0x07, 0x00, 0x02, 0xc7
                       , 0x00, 0x00, 0x02, 0xc7, 0x00, 0x00, 0x00, 0x07, 0x02, 0x8f, 0x00, 0x00
                       , 0xff, 0xff, 0xff, 0x01, 0x01, 0x00, 0x01, 0x5d, 0xff, 0x01, 0x01, 0x00
                       , 0xff, 0xff, 0xff, 0x00, 0x07, 0x02, 0x8f, 0x00, 0x00, 0x07, 0x00, 0xff
                       , 0x01, 0x03, 0x00, 0xff, 0x06, 0x03, 0x59, 0xff, 0x01, 0x03, 0x00, 0xff
                       , 0x07, 0x00, 0x02, 0xc7, 0x00, 0x00, 0x02, 0xc7, 0x00, 0x00 ]

      expected = [ ( concat [ repeat1 8 0x03, repeat2 196 (0x00,0x00) ]
                   , concat [ repeat1 7 0x01, repeat2 196 (0x00,0x00), [0x00] ]
                   , concat [ repeat2 200 (0x00,0x00) ]
                   , concat [ repeat2 200 (0x00,0x00) ] )
                 , ( concat [ repeat2 4 (0x00,0xff), [0xa5], repeat2 145 (0xe7,0xe7)
                            , [0xe0, 0xe0], repeat1 94 0xe7, [0xe0, 0xe0, 0xff, 0xff, 0xff] ]
                   , concat [ repeat1 7 0xff, [0x00], repeat2 144 (0x42,0x42)
                            , [0x42, 0x43, 0x40, 0x40, 0x40, 0x40, 0x43]
                            , repeat1 90 0x42, [0x43, 0x40, 0x40, 0x40, 0x40, 0x7f, 0x00] ]
                   , concat [ repeat2 200 (0x00,0x00) ]
                   , concat [ repeat2 200 (0x00,0x00) ] )
                 , ( concat [ repeat2 4 (0x00,0xff), repeat2 144 (0x00,0x00)
                            , [0xff, 0xff, 0xff], repeat1 2 0x00
                            , repeat1 94 0xff
                            , repeat1 2 0x00, [0xff, 0xff, 0xff] ]
                   , concat [ [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00]
                            , repeat2 144 (0x00,0x00), [0x00, 0xff], repeat1 4 0x00, [0xff]
                            , repeat1 90 0xff, [0xff], repeat1 4 0x00, [0xff, 0x00] ]
                   , concat [ repeat2 200 (0x00,0x00) ]
                   , concat [ repeat2 200 (0x00,0x00) ] )
                 ]

testRealdata0 = convertion
    where
      convertion :: IO ()
      convertion = do
        rawdata <- S.readFile image
        let hdr = getHeader rawdata
        let sc = convert rawdata (width hdr, height hdr)
        (length (mkRet sc)) @?= (length (expected (width hdr `div` 8)))
            where
              expected n = take n (repeat (400,400,400,400))
              mkRet = map (\(a,b,c,d) -> (length a, length b, length c, length d))
              image = "/home/smly/gitws/sys35tools/dist/build/aldtool/vsp/CG_0001.VSP"

{- -}

-- code 0x00
test0 = assertEqual "test for 0x00 flag" testExpectedScreen0 testOutput0
    where
      testBlocks0 :: [ByteBlock]
      testBlocks0 = [ ByteBlock { code = 0x01
                                , bytes = S.pack [0x01, 0x10, 0x01]
                                , len = 16
                                , sz = 3 }
                    , ByteBlock { code = 0x01
                                , bytes = S.pack [0x01, 0x10, 0x02]
                                , len = 16
                                , sz = 3 }
                    , ByteBlock { code = 0x01
                                , bytes = S.pack [0x01, 0x10, 0x03]
                                , len = 16
                                , sz = 3 }
                    , ByteBlock { code = 0x01
                                , bytes = S.pack [0x01, 0x10, 0x04]
                                , len = 16
                                , sz = 3 }
                    , ByteBlock { code = 0x00
                                , bytes = S.pack [0x00, 0x10]
                                , len = 16
                                , sz = 3 }
                    , ByteBlock { code = 0x00
                                , bytes = S.pack [0x00, 0x10]
                                , len = 16
                                , sz = 2 }
                    , ByteBlock { code = 0x00
                                , bytes = S.pack [0x00, 0x10]
                                , len = 16
                                , sz = 2 }
                    , ByteBlock { code = 0x00
                                , bytes = S.pack [0x00, 0x10]
                                , len = 16
                                , sz = 2 } ]

      testOutput0 :: Screen
      testOutput0 = decode testBlocks0 testSize

      testExpectedScreen0 :: Screen
      testExpectedScreen0 = [ ( [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
                              , [2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2]
                              , [3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3]
                              , [4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4] )
                            , ( [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
                              , [2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2]
                              , [3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3]
                              , [4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4] ) ]

-- code 0x01
test1 = assertEqual "test for 0x01 flag" testOutput1 testExpectedScreen1
    where
      testBlocks1 :: [ByteBlock]
      testBlocks1 = [ByteBlock { code = 0x01
                               , bytes = S.pack [0x01, 0x08, 0x0a]
                               , len = 8
                               , sz = 3 }]

      testOutput1 :: Screen
      testOutput1 = decode testBlocks1 (8, 16)

      testExpectedScreen1 :: Screen
      testExpectedScreen1 = [([0x0a, 0x0a, 0x0a, 0x0a, 0x0a, 0x0a, 0x0a, 0x0a], [], [], [])]

-- code 0x02
test2 = assertEqual "test for 0x02 flag" testOutput2 testExpectedScreen2
    where
      testBlocks2 :: [ByteBlock]
      testBlocks2 = [ByteBlock { code = 0x02
                               , bytes = S.pack [0x02, 0x03, 0xff, 0xfa]
                               , len = 3*2
                               , sz = 4 }]

      testOutput2 :: Screen
      testOutput2 = decode testBlocks2 (8, 16)

      testExpectedScreen2 :: Screen
      testExpectedScreen2 = [([0xff, 0xfa, 0xff, 0xfa, 0xff, 0xfa], [], [], [])]

-- decode testBlock (8,16)
{-
testBlock = [ByteBlock { code = 0x01, bytes = S.pack [0x01, 0x02, 0xa0], len = 10 }
            ,ByteBlock { code = 0x02, bytes = S.pack [0x02, 0x02, 0xaa, 0xab], len = 3*2 }
            ,ByteBlock { code = 0x02, bytes = S.pack [0x02, 0x02, 0x01, 0x02], len = 8*2 }
            ,ByteBlock { code = 0x01, bytes = S.pack [0x01, 0x02, 0xa0], len = 10 }
            ,ByteBlock { code = 0x02, bytes = S.pack [0x02, 0x02, 0xaa, 0xab], len = 3*2 }
            ,ByteBlock { code = 0x02, bytes = S.pack [0x02, 0x02, 0x01, 0x02], len = 4*2 }
            ,ByteBlock { code = 0x03, bytes = S.pack [0x03, 0x08], len = 8 }
            ]
-}