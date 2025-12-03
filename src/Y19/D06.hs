module Y19.D06
    ( parse
    , solve1
    , solve2
    ) where

import qualified Data.Map as M (fromList, lookup, keys, (!))
import Data.Map (Map)
import Data.Tuple (swap)
import Data.List (intersect)

type Object = String
type Problem = Orbits
type Orbits = Map Object Object
type Answer = Int

parse :: String -> Problem
parse = M.fromList . map (swap . parse1line) . lines
    where
        replace :: Eq a => [a] -> a -> a -> [a]
        replace original from to =
            map (\v -> if v == from then to else v) original
        parse1line :: String -> (String, String)
        parse1line line =
            let ws = words $ replace line ')' ' '
            in (head ws, last ws)

count :: Orbits -> Object -> Int
count orbits obj =
    case M.lookup obj orbits of
        Nothing -> 0
        Just center -> 1 + count orbits center

pathToCOM :: Orbits -> Object -> [Object]
pathToCOM orbits = go
    where
        go :: Object -> [Object]
        go obj =
            case M.lookup obj orbits of
                Nothing -> [obj]
                Just center -> obj : go center


solve1 :: Problem -> Answer
solve1 orbits =
    sum . map (count orbits) . M.keys $ orbits


solve2 :: Problem -> Answer
solve2 orbits =
    let
        path_you = M.fromList . flip zip [0..] . drop 1 $ pathToCOM orbits "YOU"
        path_san = M.fromList . flip zip [0..] . drop 1 $ pathToCOM orbits "SAN"
        common = intersect (M.keys path_you) (M.keys path_san)
        orbital_transfers = map (\obj -> (path_you M.! obj) + (path_san M.! obj)) common
    in minimum orbital_transfers