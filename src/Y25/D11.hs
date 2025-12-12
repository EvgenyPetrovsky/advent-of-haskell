{-# LANGUAGE TupleSections #-}

module Y25.D11
    ( parse
    , solve1
    , solve2
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set, member)
import qualified Data.Set as Set
import qualified Data.IntMap.Merge.Strict as Map (merge)

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
solve1 input =
    go Map.empty from Map.! from
    where
        from = "svr"
        goal = "out"
        go :: Map Node Int -> Node -> Map Node Int
        go visited node
            | node == goal = Map.insert node 1 Map.empty
            | Map.member node visited = Map.update (\a -> if a == 0 then Nothing else Just (a + 1)) node visited
            | null nodes = Map.insert node 0 visited
            | otherwise = foldl (\v n ->
                let new_v = go v n
                in Map.insertWith (+) node (new_v Map.! n) new_v
                ) visited nodes
            where
                nodes = fromMaybe [] $ input Map.!? node


solve2 :: Problem -> Answer
solve2 input =
    go Set.empty from
    where

        from = "svr"
        goal = "out"
        go :: Set Node -> Node -> Int
        go visited node
            | node == goal =
                if "dac" `member` visited && "fft" `member` visited then 1 else 0
            | node `member` visited = error "loop"
            | otherwise = sum . map (go (node `Set.insert` visited)) $ nodes
            where
                nodes = fromMaybe [] $ input Map.!? node


{-
solve1wrong :: Problem -> Answer
solve1wrong input =
    go (0, Map.empty) from Map.! from
    where
        from = "you"
        goal = "out"
        go :: (Int, Map Node Int) -> Node -> (Int, Map Node Int)
        go acc visited node
            | node == goal = Map.insertWith (+) node acc visited
            | otherwise =
                let upd_visit = Map.insertWith (+) node acc visited
                    upd_acc = upd_visit Map.! node
                in foldl (go upd_acc) upd_visit $ fromMaybe [] (input Map.!? node)
-}

