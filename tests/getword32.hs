module Main where
import Test.HUnit
import System35.File.LittleEndian (get4BWord32, get3BWord32)
import qualified Data.ByteString as S

tests :: Test
tests = test [ get4BWord32 (S.pack [0x00,0x00,0x00,0x00]) ~?= 0x00000000
             , get4BWord32 (S.pack [0x56,0x34,0x12,0x00]) ~?= 0x00123456
             , get4BWord32 (S.pack [0x00,0x00,0x00,0xff]) ~?= 0xff000000
             , get4BWord32 (S.pack [0x00,0x00,0xff,0x00]) ~?= 0x00ff0000
             , get4BWord32 (S.pack [0xff,0x00,0x00,0x00]) ~?= 0x000000ff
             , get3BWord32 (S.pack [0x00,0x00,0x00])      ~?= 0x000000
             , get3BWord32 (S.pack [0x56,0x34,0x12])      ~?= 0x123456
             , get3BWord32 (S.pack [0x00,0x00,0xff])      ~?= 0xff0000
             , get3BWord32 (S.pack [0x00,0xff,0x00])      ~?= 0x00ff00
             , get3BWord32 (S.pack [0xff,0x00,0x00])      ~?= 0x0000ff ]