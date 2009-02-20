--module Aldtool (main) where

import System35.File.Ald (extractArchives, listFiles)

import System.IO.Posix.MMap (unsafeMMapFile)
import System.Environment (getArgs)

main :: IO ()
main = do
  archives <- getArgs
  ss <- mapM unsafeMMapFile archives
--  listFiles ss
  extractArchives ss
