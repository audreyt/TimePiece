module Tiles
    ( Point(..), PointSet, Tiles
    , makeRandomTiles, makeTiles
    , tilePoints, showTiles, showPoints
    , toPointSet, fromPointSet, showPointSet, edgePoint
    ) where
import Point
import System.Random
import Data.List (find)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

type Tiles = [(Point, Size)]

-- Map from -X to existence of -Y
type PointSet = IM.IntMap IS.IntSet

tilePoints :: Tiles -> [Point]
tilePoints [] = []
tilePoints ((p@(MkPoint x y), 4):rest) = block ++ (p:tilePoints rest)
    where
    block = [ MkPoint (x+dx) (y+dy) | (dx, dy) <- _size2 ++ _size3 ++ _size4 ]
tilePoints ((p@(MkPoint x y), 3):rest) = block ++ (p:tilePoints rest)
    where
    block = [ MkPoint (x+dx) (y+dy) | (dx, dy) <- _size2 ++ _size3 ]
tilePoints ((p@(MkPoint x y), 2):rest) = block ++ (p:tilePoints rest)
    where
    block = [ MkPoint (x+dx) (y+dy) | (dx, dy) <- _size2 ]
tilePoints ((p, _):rest) = (p:tilePoints rest)

makeTiles :: [Point] -> Tiles
makeTiles [] = []
makeTiles points = (m:makeTiles rest')
    where
    ps = toPointSet points
    (m@(_, msz), rest) = case find ((== 4) . snd) tilings of
        Just m4 -> (m4, points)
        _       -> case find ((== 3) . snd) tilings of
            Just m3 -> (m3, points)
            _       -> case find ((== 2) . snd) tilings of
                Just m2 -> (m2, points)
                _       -> ((head points, 1), tail points)
    rest' = if msz == 1 then rest else filter (not . inArea m) rest
    tilings = [ (p, getTileSize p ps) | p <- points ]

makeRandomTiles :: [Point] -> IO Tiles
makeRandomTiles = fmap makeTiles . shuffle

insertPoint :: Point -> PointSet -> PointSet
insertPoint (MkPoint px py) ps = IM.insert x ys' ps
    where
    x   = -px
    y   = -py
    ys' = case IM.lookup x ps of
        Just ys -> IS.insert y ys
        _       -> IS.singleton y
    

lookupPoint :: Point -> PointSet -> Bool
lookupPoint (MkPoint px py) ps = case IM.lookup x ps of
    Just ys -> IS.member y ys
    _       -> False
    where
    x = -px
    y = -py

{-
deletePoint :: Point -> PointSet -> PointSet
deletePoint (MkPoint px py) = IM.update doUpdate x
    where
    x = -px
    y = -py
    doUpdate ys = if IS.null ys' then Nothing else Just ys'
        where
        ys' = IS.delete y ys
-}

-- At least we have size 1; the goal is to check for bigger sizes. 
getTileSize :: Point -> PointSet -> Size
getTileSize (MkPoint x y) ps
    | all ok _size2 = if all ok _size3 then if all ok _size4 then 4 else 3 else 2
    | otherwise = 1
    where
    ok (dx, dy) = lookupPoint (MkPoint (x+dx) (y+dy)) ps

_size2, _size3, _size4 :: [(X, Y)]
_size2 = [(1, 0), (0, 1), (1, 1)]
_size3 = [(2, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
_size4 = [(3, 0), (3, 1), (3, 2), (0, 3), (1, 3), (2, 3), (3, 3)]

-- The bottom right edge of a point set
edgePoint :: PointSet -> Point
edgePoint ps = MkPoint (-x) (-y)
    where
    x = head (IM.keys ps)
    y = minimum $ map (head . IS.elems) (IM.elems ps)

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle [c] = return [c]
shuffle deck0 = part deck0 [] []
    where
    part [] p0 p1 = do
        s1 <- shuffle p0
        s2 <- shuffle p1
        return (s1 ++ s2)
    part (d : deck) p0 p1 = do
        n <- randomRIO (False, True)
        if n then part deck (d : p0) p1
             else part deck p0 (d : p1)

inArea :: (Point, Size) -> Point -> Bool
inArea (MkPoint x y, sz) (MkPoint tx ty)
    =  dx >= 0 && dx < sz
    && dy >= 0 && dy < sz
    where
    dx = tx - x
    dy = ty - y

-- doMakeTiles :: PointSet -> Tiles

showTiles :: Tiles -> String
showTiles mosaic = unlines
    [   [ case lookup (MkPoint x y) mosaic of
            Just sz -> toEnum (48 + sz)
            _       -> ' '
        | x <- [0..maxX]
        ]
    | y <- [0..maxY]
    ]
    where
    pts  = map fst mosaic
    maxX = maximum (map pointX pts)
    maxY = maximum (map pointY pts)

showPoints :: [Point] -> String
showPoints = showPointSet . toPointSet

showPointSet :: PointSet -> String
showPointSet ps = unlines
    [   [ if lookupPoint (MkPoint x y) ps then '*' else ' '
        | x <- [0..maxX]
        ]
    | y <- [0..maxY]
    ]
    where
    MkPoint maxX maxY = edgePoint ps

toPointSet :: [Point] -> PointSet
toPointSet = foldl (flip insertPoint) IM.empty

fromPointSet :: PointSet -> [Point]
fromPointSet ps = concat [ [ MkPoint x y | y <- IS.elems ys ] | (x, ys) <- IM.assocs ps ]
