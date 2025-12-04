module Y25.D04
    ( parse
    , solve1
    , solve2
    ) where

import Util.AOC ((|>))
import Data.Set (Set)
import qualified Data.Set as Set

type Roll = (Int, Int)
type Problem = Set Roll
type Answer = Int


parse :: String -> Problem
parse input_text =
    Set.fromList [(x,y) | (x, ls) <- zip [0 .. ] (lines input_text)
                        , (y, c) <- zip [0 .. ] ls
                        , c == '@']

adjacent :: Roll -> [Roll]
adjacent (x,y) = [(x + xa, y + ya) | xa <- [-1 .. 1]
                                   , ya <- [-1 .. 1]
                                   , not (xa == 0 && ya == 0)]

canRemove :: Set Roll -> Set Roll
canRemove rolls =
    rolls
    |> Set.toList
    |> filter (\r -> 4 > (length . filter (`Set.member` rolls) . adjacent $ r))
    |> Set.fromList


solve1 :: Problem -> Answer
solve1 = Set.size . canRemove


solve2 :: Problem -> Answer
solve2 input
    | free_size == 0 = 0
    | otherwise = free_size + solve2 (Set.difference input free)
    where  
        free = canRemove input
        free_size = Set.size free