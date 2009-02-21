module Main (main) where

import System35.File.VSP (getHeader, Header(..))
import System.IO.Posix.MMap (unsafeMMapFile)
import System.Environment (getArgs)

main :: IO ()
main = do
  images <- getArgs
  ss <- mapM unsafeMMapFile images
  let hdr = head $ getHeader ss
  putStrLn ("(" ++ show (width hdr)  ++ "," ++ show (height hdr) ++ ")")
