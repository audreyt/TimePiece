{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Zoom where
import Point

newtype Ratio = MkRatio { fromRatio :: Double }
    deriving (Show, RealFrac, Fractional, Real, Ord, Num, Eq, Enum)
type Speed = Ratio

data Zoom = MkZoom
    { ratio :: !Ratio
    , focus :: !Point
    }
    deriving Show

noZoom :: Zoom
noZoom = MkZoom 1 (MkPoint 0 0)

nextZoom :: Zoom -> (Ratio -> Ratio) -> Zoom
nextZoom z f = z{ ratio = fixRatio $ f (ratio z) }

fixRatio :: Ratio -> Ratio
fixRatio = min maxBound . max minBound

instance Bounded Ratio where
    minBound = 1
    maxBound = 2

(***) :: (Integral b, Integral a) => a -> Zoom -> b
x *** MkZoom r _ = round (fromIntegral x * r)

(~~~) :: (Integral b, Integral a) => a -> Zoom -> b
x ~~~ MkZoom r (MkPoint dx _) = 
    round (fromIntegral x * r + ((fromIntegral dx - 256) * 2 / pred maxBound) * (1-r))

(|||) :: (Integral b, Integral a) => a -> Zoom -> b
y ||| MkZoom r (MkPoint _ dy) = 
    round (fromIntegral y * r + ((fromIntegral dy - 192) * 2 / pred maxBound) * (1-r))
