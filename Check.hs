module Check where
import Data.List (sort)
import Tiles
import Render
import Test.LazySmallCheck

instance Serial Point where
    series = cons2 MkPoint

prop_tiling points = 
    toPointSet points ==
    toPointSet (tilePoints $ makeTiles points)

main = do
    pts <- renderChar "test.ttf" 24 '龍'
    print pts
    putStrLn $ showPoints pts
    tiles <- makeRandomTiles pts
    putStrLn . showTiles $ tiles
    putStrLn . showPoints $ tilePoints tiles

sampleTiles :: Tiles
sampleTiles = concat
    [ [(MkPoint x 0, 1) | x <- [0..4]]
    , [(MkPoint x 1, 2) | x <- [0..5]]
    , [(MkPoint x 2, 3) | x <- [2..7]]
    , [(MkPoint x 3, 1) | x <- [3..7]]
    ]

samplePoints' :: [Point]
samplePoints' = concat
    [ [MkPoint x 0 | x <- [0..4]]
    , [MkPoint x 1 | x <- [0..5]]
    , [MkPoint x 2 | x <- [2..7]]
    , [MkPoint x 3 | x <- [3..7]]
    ]

samplePoints = 
    [ MkPoint {pointX = 0, pointY = 1}
    , MkPoint {pointX = 0, pointY = 0}
    , MkPoint {pointX = 0, pointY = 0}
    , MkPoint {pointX = 1, pointY = 0}
    , MkPoint {pointX = 1, pointY = 1}
    ]
