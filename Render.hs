module Render where
import Foreign
import Point
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF

type FontName = FilePath

renderChar :: FontName -> Size -> Char -> IO [Point]
renderChar fn size ch = do
    font            <- TTF.openFont fn size
    Just textImage  <- TTF.tryRenderGlyphSolid font ch white
    let width  = SDL.surfaceGetWidth textImage
        height = SDL.surfaceGetHeight textImage
    pixels  <- SDL.surfaceGetPixels textImage
    vals    <- peekArray (width * height) (castPtr pixels)

    let points = [ MkPoint x y | y <- [0..(height-1)], x <- [0..(width-1)] ]

    return [ pt | (pt, v) <- points `zip` vals, v > (0 :: Word8) ]

white :: SDL.Color
white = SDL.Color 255 255 255

showPixels :: SDL.Surface -> IO String
showPixels surface = do
    pixels <- SDL.surfaceGetPixels surface
    let width  = SDL.surfaceGetWidth surface
        height = SDL.surfaceGetHeight surface
        format = SDL.surfaceGetPixelFormat surface
    bpp     <- SDL.pixelFormatGetBytesPerPixel format
    vals    <- peekArray (fromEnum bpp * width * height) (castPtr pixels)
    let part [] = []
        part xs = let (r, rs) = splitAt width xs in (r:part rs)
    return . unlines $ map (map (\x -> if x > (0 :: Word8) then '*' else ' ')) (part vals)

