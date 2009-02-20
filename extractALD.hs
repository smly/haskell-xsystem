import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC
import System.IO.Posix.MMap (unsafeMMapFile)
import System.Environment
import Data.Bits
import Data.List
import Data.Word

data Header = Header {
      mapPos :: Word32,
      datPos :: Word32 }

type ArchiveNo = Int
type FileNo    = Int
type FileId    = (ArchiveNo, FileNo)

get4BWord32 :: S.ByteString -> Word32
get4BWord32 s = fromInteger $ toInteger $
                shiftL byte4 24 +
                shiftL byte3 16 +
                shiftL byte2 8 +
                byte1
    where byte1, byte2, byte3, byte4 :: Word32
          [byte1, byte2, byte3, byte4] =
              map fromIntegral $ S.unpack (S.take 4 s)

get3BWord32 :: S.ByteString -> Word32
get3BWord32 s = fromInteger $ toInteger $
                shiftL byte3 16 +
                shiftL byte2 8 +
                byte1
    where byte1, byte2, byte3 :: Word32
          [byte1, byte2, byte3] =
              map fromIntegral $ S.unpack (S.take 3 s)

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

getFileInfo :: S.ByteString -> IO ()
getFileInfo s = do
  putStrLn $ "filename: "++ filename
  S.writeFile filename (S.take size (S.drop ptr s))
      where
        ptr  = fromIntegral $ get4BWord32 s
        size = fromIntegral $ get4BWord32 (S.drop 4 s)
        filename = SC.unpack (S.take (ptr-16) (S.drop 16 s))

getFileName :: S.ByteString -> IO (String)
getFileName s = return $ takeWhile (/= '\0') $ SC.unpack (S.take (ptr-16) (S.drop 16 s))
    where
      ptr  = fromIntegral $ get4BWord32 s
      size = fromIntegral $ get4BWord32 (S.drop 4 s)

divide :: [S.ByteString] -> [[Int]] -> [FileId] -> IO ()
divide ss ps m =
    mapM_ (\(aid,fid) -> do
             getFileInfo (S.drop (((ps !! (aid-1)) !! (fid-1))*256) (ss !! (aid-1)))
          ) m

lookupData :: [S.ByteString] -> [Header] -> FileId -> S.ByteString
lookupData ss hs (aid,fid) = S.drop targetPos targetObjLinkFile
    where
      pointers = getPointers ss hs
      targetObjLinkFile = (ss !! (aid-1))
      targetObjLinkPtr  = pointers !! (aid-1)
      targetObjPtr      = targetObjLinkPtr !! (fid-1)
      targetPos            = targetObjPtr * 256

listFiles :: [S.ByteString] -> [Header] -> IO ()
listFiles ss hs = do
  mapM_ (\(aid,fid) -> do
           filename <- getFileName $ lookupData ss hs (aid,fid)
           putStrLn (show aid ++ "," ++ show fid ++ "," ++ filename)
        ) mapping
    where sdata = head ss
          header = head hs
          map_start = fromIntegral (mapPos header)
          dat_start = fromIntegral (datPos header)
          mapping = getMapping $ S.take (dat_start - map_start) $ S.drop map_start sdata

extractArchives :: [S.ByteString] -> [Header] -> IO ()
extractArchives ss hs = do
--  getFileInfo (S.drop dat_start sdata)
  divide ss pointers mapping
    where sdata = head ss
          header = head hs
          map_start = fromIntegral (mapPos header)
          dat_start = fromIntegral (datPos header)
          mapping = getMapping $ S.take (dat_start - map_start) $ S.drop map_start sdata
          pointers = getPointers ss hs

main :: IO ()
main = do
  archives <- getArgs
  ss <- mapM unsafeMMapFile archives
  hs <- mapM getHeader ss
--  extractArchives ss hs
  listFiles ss hs
      where
        getHeader :: S.ByteString -> IO (Header)
        getHeader s = do
          return $ Header {
                        mapPos    = pskip,
                        datPos    = mskip }
                      where
                        mptr = get4BWord32 s
                        dptr = get4BWord32 (S.drop 3 s)
                        pskip = shiftL mptr 8
                        mskip = shiftL dptr 8
