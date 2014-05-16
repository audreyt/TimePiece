module Image where
import Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import qualified Graphics.UI.SDL.Image as SDLImage
import qualified Graphics.UI.SDL.Rotozoomer as SDLRotozoomer

import Zoom
import Cache
import Paths
import Point

initSDL :: (Surface -> Int -> Int -> IO a) -> IO ()
initSDL f = SDL.withInit [InitTimer, InitAudio, InitVideo] $ do 
    info    <- getVideoInfo
    let w = videoInfoWidth info
        h = videoInfoHeight info
    screen  <- setVideoMode w h 32
        [AnyFormat, Fullscreen, HWAccel, HWSurface, DoubleBuf]
    TTF.init
    f screen w h
    TTF.quit

blitTile :: (Show a) => X -> Y -> Surface -> a -> Zoom -> (Point, Int) -> IO Bool
blitTile xOff yOff canvas seed z (MkPoint x y, sz) = do
    img <- loadImageSized (show seed, (sz * 9) *** z)
    let x'  = x * 9 + xOff
        y'  = y * 12 + yOff
        pos = Just (Rect (x' ~~~ z) (y' ||| z) 0 0)
    blitSurface img Nothing canvas pos

{-# NOINLINE _loadImage #-}
_loadImage :: Cache FilePath Surface
_loadImage = initCache newCache
loadImage :: FilePath -> IO Surface
loadImage = cache _loadImage SDLImage.load

{-# NOINLINE _loadImageSized #-}
_loadImageSized :: Cache (String, Int) Surface
_loadImageSized = initCache newCache
loadImageSized :: (String, Int) -> IO Surface
loadImageSized = cache _loadImageSized $ \(fn, sz) -> do
    -- Round to nearest multiples of 10
    img <- getDataFileName $ fn ++ "/" ++ show ((sz + 9) `div` 10) ++ "0.png"
    src <- loadImage img
    let r = (fromIntegral sz + 0.5) / fromIntegral srcWidth
        srcWidth = SDL.surfaceGetWidth src
    if srcWidth == sz
        then return src
        else SDLRotozoomer.zoom src r r True
