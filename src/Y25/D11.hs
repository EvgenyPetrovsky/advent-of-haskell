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
    in map (, from) dest

parse :: String -> Problem
parse =
    foldl (\z (k,a) -> Map.insertWith (++) k [a] z) Map.empty
    . concatMap parseline
    . lines


solve1 :: Problem -> Answer
solve1 input =
    go [] from
    where
        from = "out"
        goal = "you"
        go :: [Node] -> Node -> Int
        go visited node
            | node == goal = 1
            | node `elem` visited = 0
            | otherwise = sum . map (go (node:visited)) $ nodes
            where
                nodes = fromMaybe [] $ input Map.!? node


solve2 :: Problem -> Answer
solve2 = undefined