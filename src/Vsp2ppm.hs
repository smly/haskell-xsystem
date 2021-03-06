module Main (main) where

import System35.File.Vsp (getHeader, Header(..), convert)
import System.IO.Posix.MMap (unsafeMMapFile)
import System.Environment (getArgs)

main :: IO ()
main = do
--  images <- getArgs
--  s <- unsafeMMapFile $ head images
  s <- unsafeMMapFile image
  let hdr = getHeader s
  putStrLn ("(" ++ show (width hdr)  ++ "," ++ show (height hdr) ++ ")")
--  putStrLn $ show $ palette hdr
  let sc = convert s $ (width hdr, height hdr)
  mapM_ (putStrLn . show . length . (\(a,b,c,d) -> d)) sc
      where
        image = "/home/smly/gitws/sys35tools/dist/build/aldtool/vsp/CG_0001.VSP"
