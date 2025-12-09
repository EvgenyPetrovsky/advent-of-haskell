module Y25.D08
    ( parse
    , solve1
    , solve2
    ) where


import Data.List.Split (splitOn)
import Util.AOC ((|>))
import Data.List (sort, sortBy)
import Data.Set (Set, union, member, size, insert, isSubsetOf, singleton)
import qualified Data.Set as Set (fold, filter, toList, fromList)

type Box = (Int, Int, Int)
type Distance = Int
type Problem = [Box]
type Answer = Int
type Circuit = Set Box


parse :: String -> Problem
parse =
    map parse_line . lines
    where
        parse_line :: String -> (Int, Int, Int)
        parse_line str =
            let nums = str |> splitOn "," |> map (\x -> read x :: Int)
            in (head nums, nums !! 1, nums !! 2)


pair :: Box -> Box -> (Box, Box)
pair p1 p2 = (min p1 p2, max p1 p2)


dist :: Box -> Box -> Int
dist (x1,y1,z1) (x2,y2,z2) =
    sq (x1-x2) + sq (y1-y2) + sq (z1-z2)
    where sq x = x * x


inCircuit :: (Box, Box) -> Set Circuit -> Bool
inCircuit (b1, b2) = any (\s -> b1 `member` s && b2 `member` s)


findCircuit :: Box -> Set Circuit -> Circuit
findCircuit b = Set.fold union (singleton b) . Set.filter (member b)


connectPair :: (Box, Box) -> Set Circuit -> Set Circuit
connectPair (b1,b2) circuits =
    let c1 = findCircuit b1 circuits
        c2 = findCircuit b2 circuits
        new_c = c1 `union` c2
        clean_cs = Set.filter (\c -> not $ c `isSubsetOf` new_c) circuits
    in new_c `insert` clean_cs


solve1 :: Problem -> Answer
solve1 input =
    let distances :: [(Distance, (Box, Box))]
        distances = sort [(dist p1 p2, pair p1 p2) | p1 <- input, p2 <- input, p1 < p2]
        new_circs = go 1000 (Set.fromList $ map singleton input) distances
    in new_circs |> Set.toList |> map size |> sortBy (flip compare) |> take 3 |> product
    where
        go :: Int -> Set Circuit -> [(Distance, (Box, Box))] -> Set Circuit
        go _ circuits [] = circuits
        go n circuits (least_dist:rem_dists)
            | n <= 0 = circuits
            | b1_b2 `inCircuit` circuits = go (n-1) circuits rem_dists
            | otherwise = go (n-1) (b1_b2 `connectPair` circuits) rem_dists
            where
                (_, (b1,b2)) = least_dist
                b1_b2 = pair b1 b2


solve2 :: Problem -> Answer
solve2 input =
    let distances :: [(Distance, (Box, Box))]
        distances = sort [(dist p1 p2, pair p1 p2) | p1 <- input, p2 <- input, p1 < p2]
        ((x1,_,_), (x2,_,_)) = go (Set.fromList $ map singleton input) distances
    in x1 * x2
    where
        go :: Set Circuit -> [(Distance, (Box, Box))] -> (Box, Box)
        go _ [] = error "empty distance list"
        go circuits (least_dist:rem_dists)
            | b1_b2 `inCircuit` circuits = go circuits rem_dists
            | size merged_circuits == 1 = (b1, b2)
            | otherwise = go merged_circuits rem_dists
            where
                (_, (b1,b2)) = least_dist
                b1_b2 = pair b1 b2
                merged_circuits = b1_b2 `connectPair` circuits