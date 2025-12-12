{-# LANGUAGE TupleSections #-}

module Y25.D11
    ( parse
    , solve1
    , solve2
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

type Node = String
type Problem = Map Node [Node]
type Answer = Int


parseline :: String -> [(Node, Node)]
parseline line =
    let from = take 3 line
        dest = words $ drop 5 line
    in map (from, ) dest

parse :: String -> Problem
parse =
    foldl (\z (k,a) -> Map.insertWith (++) k [a] z) Map.empty
    . concatMap parseline
    . lines


solve1 :: Problem -> Answer
solve1 = walks "you" "out"


solve2 :: Problem -> Answer
solve2 input =
    (product . zipWith (\a b -> walks a b input) route1 $ tail route1)
    +
    (product . zipWith (\a b -> walks a b input) route2 $ tail route2)
    where
        route1 = ["svr", "fft", "dac", "out"]
        route2 = ["svr", "dac", "fft", "out"]


walks :: Node -> Node -> Problem -> Int
walks from goal connections =
    go Map.empty from Map.! from
    where
        go :: Map Node Int -> Node -> Map Node Int
        go visited node
            | Map.member node visited = visited        -- if this node was reached before - change nothing            
            | node == goal = Map.insert node 1 visited -- if goal is reached - put 1 score
            | null nodes = Map.insert node 0 visited   -- if none of nodes can be reached - put 0 score
            | otherwise = foldl (\v n ->               -- otherwise visit firther nodes and sum all of their scores
                let (new_v, score) = (go v n, new_v Map.! n) 
                in Map.insertWith (+) node score new_v
                ) visited nodes
            where
                nodes = fromMaybe [] $ connections Map.!? node
