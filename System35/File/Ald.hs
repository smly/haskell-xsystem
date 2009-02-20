module System35.File.Ald (
  extractArchives, listFiles,
  getHeader, getMapping, getPointers,
  getFileName, lookupData,
) where

import System35.File.LittleEndian (get4BWord32, get3BWord32)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC
import System.IO.Posix.MMap (unsafeMMapFile)
import System.Environment (getArgs)
import Data.Bits (shiftL)
import Data.Word (Word32)
import Data.List

data Header = Header {
      mapPos :: Word32,
      datPos :: Word32 }

type ArchiveNo = Int
type FileNo    = Int
type FileId    = (ArchiveNo, FileNo)

getHeader :: [S.ByteString] -> [Header]
getHeader = map getHeader'
    where
      getHeader' :: S.ByteString -> Header
      getHeader' s = Header { mapPos = shiftL mapAddr 8,
                              datPos = shiftL datAddr 8 }
          where
            mapAddr = get4BWord32 s
            datAddr = get4BWord32 (S.drop 3 s)

getMapping :: S.ByteString -> [FileId]
getMapping s = filter (\(a,_) -> if a==0 then False else True)
               [ getMap (S.drop x s) | x <- [0,3..(S.length s)-3]]
    where getMap :: S.ByteString -> FileId
          getMap sdata = (fromInteger (toInteger byte1),
                      fromInteger (toInteger ((shiftL byte3 8) + byte2)))
              where byte1, byte2, byte3 :: Word32
                    [byte1, byte2, byte3] = map fromIntegral $ S.unpack (S.take 3 sdata)

getPointers :: [S.ByteString] -> [Header] -> [[Int]]
getPointers ss hs =
    map getPointer
    [S.take (fromIntegral ((mapPos h)-1)) s | (s,h) <- zip ss hs]
    where
      getPointer :: S.ByteString -> [Int]
      getPointer s = filter (\n -> n/=0)
                     [ fromIntegral (get3BWord32 (S.drop x s)) | x <- [3,6..(S.length s)-3] ]

getFileName :: S.ByteString -> String
getFileName s = takeWhile (/= '\0') $ SC.unpack (S.take (ptr-16) (S.drop 16 s))
    where
      ptr  = fromIntegral $ get4BWord32 s

lookupData :: [S.ByteString] -> [Header] -> [[Int]] -> FileId -> S.ByteString
lookupData ss hs ps (aid,fid) = S.take size $ S.drop targetPos targetObjLinkFile
    where
      size = fromIntegral $ get4BWord32 $ S.drop 4 targetObjLinkFile
      targetObjLinkFile = ss !! (aid-1)
      targetObjLinkPtr  = ps !! (aid-1)
      targetObjPtr      = targetObjLinkPtr !! (fid-1)
      targetPos         = targetObjPtr * 256

listFiles :: [S.ByteString] -> IO ()
listFiles ss = do
  mapM_ (\fid -> putStrLn (show (fst fid) ++ "," ++
                           show (snd fid) ++ "," ++
                           getFileName (lookupData ss hs ps fid))) mapping
    where
      sdata   = head ss
      header  = head hs
      mapPos' = fromIntegral (mapPos header)
      datPos' = fromIntegral (datPos header)
      hs      = getHeader ss
      ps      = getPointers ss hs
      mapping = getMapping $ S.take (datPos' - mapPos') $ S.drop mapPos' sdata

writeDataFile :: S.ByteString -> IO ()
writeDataFile s = do
  putStrLn $ "writing file: "++ filename
  S.writeFile filename (S.take size (S.drop ptr s))
      where
        ptr  = fromIntegral $ get4BWord32 s
        size = fromIntegral $ get4BWord32 (S.drop 4 s)
        filename = SC.unpack (S.take (ptr-16) (S.drop 16 s))

extractArchives :: [S.ByteString] -> IO ()
extractArchives ss = mapM_ (writeDataFile . (lookupData ss hs ps)) mapping
    where
      sdata = head ss
      header = head hs
      map_start = fromIntegral (mapPos header)
      dat_start = fromIntegral (datPos header)
      mapping = getMapping $ S.take (dat_start - map_start) $ S.drop map_start sdata
      hs = getHeader ss
      ps = getPointers ss hs
