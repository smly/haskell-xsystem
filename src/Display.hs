module Main (main) where

import System35.File.Vsp
import Control.Concurrent (threadDelay)
import Graphics.UI.SDL as SDL
import System.IO.Posix.MMap (unsafeMMapFile)
import System.Environment (getArgs)

loadVspImage :: Surface -> (Int, Int) -> String -> IO Bool
loadVspImage surf (x0,y0) vsp =
    do s <- unsafeMMapFile vsp
       let (sz,pxs) = getImage s
       setAllImagePixel surf (x0,y0) (0,0) sz pxs
       return (True)
    where
      setAllImagePixel :: Surface -> (Int, Int) -> (Int, Int) -> (Int, Int) -> [RGB] -> IO Bool
      setAllImagePixel surf (x0, y0) (x, y) (w, h) (p0:p1:p2:p3:p4:p5:p6:p7:ps) =
          do setPixel surf (x0+x+0) (y0+y) p0
             setPixel surf (x0+x+1) (y0+y) p1
             setPixel surf (x0+x+2) (y0+y) p2
             setPixel surf (x0+x+3) (y0+y) p3
             setPixel surf (x0+x+4) (y0+y) p4
             setPixel surf (x0+x+5) (y0+y) p5
             setPixel surf (x0+x+6) (y0+y) p6
             setPixel surf (x0+x+7) (y0+y) p7
             setAllImagePixel' surf (x0,y0) (x,y) (w,h) ps
                 where
                   setAllImagePixel' surf (x0,y0) (x,y) (w,h) ps
                              | x+8+y+1 == w+h     = return True
                              | y+1 == h           = setAllImagePixel surf (x0,y0) (x+8,0) (w,h) ps
                              | otherwise          = setAllImagePixel surf (x0,y0) (x,y+1) (w,h) ps

setPixel :: Surface -> Int -> Int -> RGB -> IO Bool
setPixel surf x y c =
    do pixel <- mapRGB (surfaceGetPixelFormat surf) (red c) (green c) (blue c)
       fillRect surf (Just (Rect x y 1 1)) pixel

main :: IO ()
main = do
  imagefiles <- getArgs
  SDL.init [InitVideo]
  setVideoMode wndWidth wndHeight wndDepth []
  setCaption wndTitle ""
  surf <- getVideoSurface
  back <- createRGBSurface [] wndWidth wndHeight wndDepth 0 0 0 0
  loadVspImage back (0,0) (head imagefiles)
--  loadVspImage back (0,0) imagefile1
--  loadVspImage back (40,0) imagefile2
  blitSurface back Nothing surf Nothing
  SDL.flip surf
  quitHandler
      where
        (wndWidth, wndHeight, wndDepth) = (640, 400, 32)
        wndTitle = "System3.x in Haskell"
        imagefile1 = "/home/smly/gitws/sys35tools/bag/CG_0070.VSP"
        imagefile2 = "/home/smly/gitws/sys35tools/bag/CG_0697.VSP"

quitHandler :: IO ()
quitHandler =
    do e <- waitEvent
       case e of
         Quit      -> return ()
         KeyDown (Keysym SDLK_RETURN _ _)            -> SDL.quit
         KeyDown (Keysym SDLK_SPACE _ _)             -> SDL.quit
         KeyDown (Keysym SDLK_F4 [KeyModLeftAlt] _)  -> SDL.quit
         KeyDown (Keysym SDLK_F4 [KeyModRightAlt] _) -> SDL.quit
         otherwise ->  quitHandler
