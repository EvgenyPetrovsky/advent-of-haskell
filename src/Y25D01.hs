module Y25D01
    ( parse
    , solve1
    , solve2
    ) where

type Problem = [Rotation]
type Answer = Int

type Distance = Int
data Rotation = L Distance | R Distance deriving (Show)

parseline :: String -> Rotation
parseline ('L':n) = L (read n)
parseline ('R':n) = R (read n)
parseline x = error $ "Unknown line: " ++ x


parse :: String -> Problem
parse = map parseline . lines

solve1 :: Problem -> Answer
solve1 = length . filter (0 ==) . scanl rotate 50

rotate :: Distance -> Rotation -> Distance
rotate z (R n) = (z + n) `mod` 100
rotate z (L n) = (z - n) `mod` 100

rotate2 :: (Distance, Int) -> Rotation -> (Distance, Int)
rotate2 (start, cnt) r@(R n) =
    (rotate start r, cnt + ((start + n) `div` 100))
rotate2 (start, cnt) r@(L n)
    | start == 0 = (new_finish, cnt + full_turns)
    | otherwise  = (new_finish, cnt + full_turns + 1)
    where 
        new_finish = rotate start r
        full_turns = negate (start - n) `div` 100

solve2 :: Problem -> Answer
solve2 = snd . foldl rotate2 (50, 0)
