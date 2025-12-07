module Y19.D10
    ( parse
    , solve1
    , solve2
    ) where


import Util.AOC ((.-.), (|>))
import Data.List (nub)


type Position = (Int, Int)
type Disposition = (Int, Int)
type Problem = [Position]
type Answer = Int

parse :: String -> Problem
parse text_input =
    [(ridx, cidx) | (row, ridx) <- zip (lines text_input) [0 ..]
                , (col, cidx) <- zip row [0 ..]
                , col == '#']


disposition :: Position -> Position -> Disposition
disposition center object= object .-. center


direction :: Disposition -> Disposition
direction (dr, dc)
    | dr == 0 && dc == 0 = (0,0)
    | dr <  0 && dc == 0 = (-1,0)
    | dr >  0 && dc == 0 = (1,0)
    | dr == 0 && dc <  0 = (0,-1)
    | dr == 0 && dc >  0 = (0,1)
    | otherwise = (dr `div` x, dc `div` x)
    where x = gcd (abs dr) (abs dc)




solve1 :: Problem -> Answer
solve1 input =
    input
        |> map (\x -> (x, length . nub . map (direction . disposition x) $ input))
        |> map snd
        |> maximum
        |> (\x -> x -1)


solve2 :: Problem -> Answer
solve2 = undefined