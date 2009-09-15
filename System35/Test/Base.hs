module System35.Test.Base (assertEqual') where

import Test.HUnit
import Data.Word (Word8)
import qualified Data.ByteString as S
import Control.Monad (unless, foldM)

assertEqual' :: String -> S.ByteString -> S.ByteString -> Assertion
assertEqual' preface expected actual =
    unless (actual == expected) (assertFailure msg)
        where msg = (if null preface then "" else preface ++ "\n") ++
                    "position: " ++ show pos ++ "\n" ++
                    case pos >= 10 of
                      True  -> "expected: .."   ++ lastMatch pos (S.unpack (expected)) ++
                               "\n but got: .." ++ lastMatch pos (S.unpack (actual))
                      False -> "expected: "   ++ show (take (pos+1) (S.unpack (expected))) ++
                               "\n but got: " ++ show (take (pos+1) (S.unpack (actual)))
              pos = findErrorPos (S.unpack expected) (S.unpack actual) 0
              lastMatch pos seq = show' $ take (pos-1) (drop (pos-9) seq)
              show' []     = ",.."
              show' (x:xs) = "," ++ show x ++ show' xs

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
