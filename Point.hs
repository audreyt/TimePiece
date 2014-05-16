module Point where

type X = Int
type Y = Int
type Size = Int

data Point = MkPoint
    { pointX :: !X
    , pointY :: !Y
    }
    deriving (Show, Eq, Ord)
