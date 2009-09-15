module System35.Test.Base (assertEqual', assertEqualSeq) where

import Test.HUnit
import Data.Word (Word8)
import qualified Data.ByteString as S
import Control.Monad (unless, foldM)

assertEqualSeq :: (Eq a, Show a) => String -> [a] -> [a] -> Assertion
assertEqualSeq preface expected actual =
    unless (actual == expected) (assertFailure msg)
        where msg = (if null preface then "" else preface ++ "\n") ++
                    "position: " ++ show pos ++ "\n" ++
                    case pos >= 10 of
                      True  -> "expected: .."   ++ lastMatch pos expected ++
                               "\n but got: .." ++ lastMatch pos actual
                      False -> case len <= 10 of
                                 True  -> "expected: "   ++ show (take (pos+1) expected) ++
                                          "\n but got: " ++ show (take (pos+1) actual)
                                 False -> "expected: ["   ++ show' (take (pos+1) expected) ++
                                          "\n but got: [" ++ show' (take (pos+1) actual)
              pos = findErrorPos expected actual 0
              lastMatch pos seq = show' $ take (pos-1) (drop (pos-9) seq)
              len = max (length expected) (length actual)
              show' []     = "..."
              show' (x:xs) = show x ++ "," ++ show' xs

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

findErrorPos :: (Eq a) => [a] -> [a] -> Int -> Int
findErrorPos [] [] _ = 0
findErrorPos (e:es) (a:as) x
    | (e == a)  = findErrorPos es as (x+1)
    | otherwise = x

findErrorCnt :: (Eq a) => [a] -> [a] -> Int -> Int
findErrorCnt [] [] x = x
findErrorCnt (e:es) (a:as) x
    | (e == a)  = findErrorCnt es as x
    | otherwise = findErrorCnt es as (x+1)
