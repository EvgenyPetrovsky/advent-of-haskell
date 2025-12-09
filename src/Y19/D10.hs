module Y19.D10
    ( parse
    , solve1
    , solve2
    ) where


import Util.AOC ((.-.), (|>))
import Data.List (nub, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map

type Position = (Int, Int)
type Disposition = (Int, Int)
type Angle = Double
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
solve2 input =
    let (a,b) = go 200 0 mpsort
    in a * 100 + b
    where
        bp = bestPosition input
        other = filter (/= bp) input
        dirs = map (direction . disposition bp) other
        angles = map (\(a,b) -> atan2 (fromIntegral a :: Angle) (fromIntegral b :: Angle)) dirs
        mp :: Map Angle [Position]
        mp = foldl (\z (k, a) -> Map.insertWith (++) k [a] z) Map.empty $ zip angles other
        mpsort = Map.map (sortBy (\(a1, b1) (a2, b2) -> compare (a1*a1+b1*b1) (a2*a2+b2*b2))) mp
        next_angle :: Angle -> [Angle] -> Angle
        next_angle _a _as
            | _a == maximum _as = minimum _as
            | otherwise = minimum [a | a <- _as, a > _a]
        go :: Int -> Angle -> Map Angle [Position] -> Position
        go n angle m
            | angle `notElem` Map.keys m = go n next m
            | n == 0 = head $ m Map.! angle
            | otherwise = go (n-1) next newm
            where
                next = next_angle angle (Map.keys m)
                newp = tail $ m Map.! angle
                newm :: Map Angle [Position]
                newm = case newp of
                    [] -> Map.delete angle m
                    ps -> Map.update (\_ -> Just ps) angle m

bestPosition :: Problem -> Position
bestPosition input = 
    let pos_cnt = map (\x -> (x, length . nub . map (direction . disposition x) $ input)) input
        max_cnt = maximum . map snd $ pos_cnt
        bestpos = fst . head . filter (\(_,cnt) -> cnt == max_cnt) $ pos_cnt
    in bestpos

