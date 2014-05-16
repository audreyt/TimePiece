module Main where
import System.Time
import System.Locale
import System.Random
import Control.Monad
import Graphics.UI.SDL as SDL

import Cache
import Image
import Paths
import Tiles
import Render
import Zoom

data Status
    = Static  { sWidth :: Int, sHeight :: Int, sTime :: String }
    | ZoomIn  { sWidth :: Int, sHeight :: Int, sTime :: String, sZoom :: Zoom, sSpeed :: Speed }
    | ZoomOut { sWidth :: Int, sHeight :: Int, sTime :: String, sZoom :: Zoom, sSpeed :: Speed }

main :: IO ()
main = initSDL $ \screen initW initH -> loop (Static initW initH "") $ \status -> do
    time' <- getCurrentTime
    let paint = paintScreen screen (sWidth status) (sHeight status) time'
    case status of
        Static{ sTime = t } | t == time' -> do
            SDL.delay 50
            return status
        Static{} -> do
            paint noZoom
            if last time' == '0'
                then initZoomIn time' (sWidth status) (sHeight status)
                else return status{ sTime = time' }
        ZoomIn{ sZoom = z, sWidth = w, sHeight = h, sSpeed = s } -> do
            paint z
            return $ if ratio z >= maxBound
                then ZoomOut
                    { sTime   = time'
                    , sZoom   = z
                    , sSpeed  = 0.05
                    , sWidth  = w
                    , sHeight = h
                    }
                else status
                    { sTime  = time'
                    , sZoom  = nextZoom z (+ s)
                    , sSpeed = (maxBound - ratio z) / 50 + 0.01
                    }
        ZoomOut{ sZoom = z, sWidth = w, sHeight = h, sSpeed = s } -> do
            paint z
            return $ if ratio z <= minBound
                then Static
                    { sTime   = time'
                    , sWidth  = w
                    , sHeight = h
                    }
                else status
                    { sTime = time'
                    , sZoom  = nextZoom z (subtract s)
                    }

data Tiling = MkTiling
    { tTiles :: Tiles
    , tSeeds :: [Int]
    , tEdge  :: Point
    }

{-# NOINLINE _calculateTiling #-}
_calculateTiling :: CacheOnce String [Tiling]
_calculateTiling = initCache newCacheOnce
calculateTiling :: String -> IO [Tiling]
calculateTiling = cacheOnce _calculateTiling $ \time -> forM time $ \ch -> do
    ttf     <- getDataFileName "TimePiece.ttf"
    pts     <- renderChar ttf (if ch == '4' || ch == ':' then 28 else 30) ch
    tiling  <- makeRandomTiles pts
    seeds   <- forM tiling . const $ randomRIO (1, 3)
    return $ MkTiling tiling seeds (edgePoint (toPointSet pts))

paintScreen :: Surface -> Int -> Int -> String -> Zoom -> IO ()
paintScreen screen w h time zoom = do
    bgColor <- mapRGB (surfaceGetPixelFormat screen) 0x00 0x00 0x33
    fillRect screen Nothing bgColor
    rvs <- calculateTiling time
    let maxX  = 10 -- maximum (map pointX edges)
        maxY  = maximum (map pointY edges)
        edges = map tEdge rvs
    forM_ ([0..] `zip` rvs) $ \(n, MkTiling tiles seeds edge) -> do
        let tiles' = map adjust tiles
            adjust (MkPoint x y, sz) = (MkPoint (x + deltaX) (y + deltaY), sz)
            deltaX = (maxX - pointX edge + 1) `div` 2 + 1
            deltaY = (maxY - pointY edge + 1) `div` 2 + 1
        forM_ (tiles' `zip` seeds) $ \(tile, seed) -> do
            let x = (((16 * 15 * n) `div` 2) + ((w - 964) `div` 2))
                y = (h - 288) `div` 2
            blitTile x y screen seed zoom tile
    SDL.flip screen

getCurrentTime :: IO String
getCurrentTime = do
    fmap (formatCalendarTime defaultTimeLocale "%H:%M:%S")
        . toCalendarTime =<< getClockTime

loop :: Status -> (Status -> IO Status) -> IO ()
loop status f = f status >>= \status' -> do
    event <- pollEvent
    case event of
        Quit -> return ()
        KeyDown (Keysym SDLK_SPACE _ _) -> do
            status'' <- initZoomIn (sTime status') (sWidth status') (sHeight status')
            loop status'' f
        KeyDown (Keysym SDLK_k _ _) -> loop status' f
        KeyDown{}           -> return ()
        MouseButtonDown{}   -> return ()
        _                   -> loop status' f

initZoomIn :: String -> Int -> Int -> IO Status
initZoomIn time' w h = do
    xf <- randomRIO (0, w)
    yf <- randomRIO ((h `div` 2)-96, (h `div` 2)+96)
    return $ ZoomIn
        { sZoom   = MkZoom
            { ratio = 1
            , focus = MkPoint xf yf
            }
        , sTime   = time'
        , sSpeed  = 0
        , sWidth  = w
        , sHeight = h
        }
