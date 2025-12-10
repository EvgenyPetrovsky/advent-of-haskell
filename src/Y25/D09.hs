module Y25.D09
    ( parse
    , solve1
    , solve2
    ) where
import Data.List.Split (splitOn)
import Data.List (sortBy)

type Position = (Int,Int)
type Vector = (Position, Position)
type Tile = Position
type Problem = [Tile]
type Answer = Int



parse :: String -> Problem
parse = map parse1line . lines
    where
        parse1line :: String -> (Int, Int)
        parse1line = (\x -> (head x, x !! 1)) . map read . splitOn ","


solve1 :: Problem -> Answer
solve1 input =
    maximum [(abs (x1-x2) + 1) * (abs (y1-y2) + 1)
                | (x1,y1) <- input
                , (x2,y2) <- input
                , x1 < x2 || x1 == x2 && y1 < y2
                ]


{- 
    for any given pair of positions, create a new pair where
        position 1 is in the top left corner 
        position 2 is on the bottom right corner
    where corners are the corners of rectangle built around 2 positions
    e.g. (5, 1) & (1, 5) -> (1,1) & (5,5)

-}
tlbr :: (Position, Position) -> (Tile, Tile)
tlbr ((x1,y1), (x2,y2)) = ((min x1 x2, min y1 y2), (max x1 x2, max y1 y2))


{-
    transform list of coordintates into segments of polygon so that
    - start of segment located at top left
    - end of segment located at bottom right    
-}
segments :: [Tile] -> [(Position, Position)]
segments ts =
    let te = tail ts ++ [head ts]
    in zipWith (curry tlbr) ts te


{-
    check if any of segments is crossing the rectange drawn around 2 coordinates
-}
check :: Vector -> [Vector] -> Bool
check rect segms =
    let ((xl, yt), (xr, yb)) = tlbr rect
    {- 
        two checks: 
        - rectangle sides do not cross the segments
        - rectangle top left corner is inside 
    -}
        -- vertical line crossing top or vertical line crossing bottom or both or in the middle
        v_cross = any (\((x1, y1), (x2, y2)) -> x1 == x2 && xl < x1 && x1 < xr && y1 < yb && y2 > yt) segms
        -- horizontal line crossing left or horizontal line crossing right or both or in the middle
        h_cross = any (\((x1, y1), (x2, y2)) -> y1 == y2 && yt < y1 && y1 < yb && x1 < xr && x2 > xl) segms
        -- rectangle top left point is "inside"
        tl_inside = odd . length . filter (\((x1, y1), (x2, y2)) -> x1 == x2 && x1 <= xl && y1 <= yt && y2 > yt) $ segms
    in tl_inside && not v_cross && not h_cross


solve2 :: Problem -> Answer
solve2 input =
    let segms = segments input
        -- handy to see rectangle definitions for debugging, otherwise could be just maximum area
        best = take 5 . sortBy (flip compare) $ 
            [((abs (x1-x2) + 1) * (abs (y1-y2) + 1), ((x1,y1),(x2,y2)))
                | (x1,y1) <- input
                , (x2,y2) <- input
                , x1 < x2 || x1 == x2 && y1 < y2
                , check ((x1,y1),(x2,y2)) segms
                ]
    in fst $ head best
