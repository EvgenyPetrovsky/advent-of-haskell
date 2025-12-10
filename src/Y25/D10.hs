module Y25.D10
    ( parse
    , solve1
    , solve2
    ) where
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Data.Set (Set, delete)

data Toggle = On | Off deriving Eq
instance Show Toggle where
    show On = "#"
    show Off = "."

type Target = [Toggle]
type Position = Int
type Button = [Position]
type Joltage = [Int]

type Problem = [(Target, [Button], Joltage)]
type Answer = Int


{- [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7} -}
parseline :: String -> ([Toggle], [Button], Joltage)
parseline line =
    let pieces = words line
        tgt = head pieces
        bts = tail . init $ pieces
        jlt = last pieces
    in (parse_tgt tgt, map parse_bts bts, parse_jlt jlt)
    where
        parse_tgt :: String -> [Toggle]
        parse_tgt = map (\c -> if c == '#' then On :: Toggle else Off :: Toggle) . tail . init
        parse_bts :: String -> [Position]
        parse_bts = map (\p -> read p :: Int) . splitOn "," . tail . init
        parse_jlt :: String -> Joltage
        parse_jlt = map (\p -> read p :: Int) . splitOn "," . tail . init


parse :: String -> Problem
parse = map parseline . lines


toggle :: Toggle -> Toggle
toggle On = Off
toggle Off = On


updateToggles :: Button -> [Toggle] -> [Toggle]
updateToggles =
    go 0
    where
        go :: Int -> Button -> [Toggle] -> [Toggle]
        go _ [] tgs = tgs
        go _ _ [] = []
        go idx (b:bs) (t:ts)
            | idx == b = toggle t : go (idx + 1) bs ts
            | otherwise = t : go (idx + 1) (b:bs) ts


solve1line :: ([Toggle], [Button], Joltage) -> Int
solve1line (target_state, buttons, _) = go 0 999 (map (const Off) target_state) bts_set
    where
        bts_set = Set.fromList buttons
        go :: Int -> Int -> [Toggle] -> Set Button -> Int
        go acc best tgs bts
            | tgs == target_state = acc
            | acc > best = 999
            | null bts   = 999
            | otherwise  = foldl (\z x -> min best $ go (acc+1) z (updateToggles x tgs) (x `delete` bts)) best bts


solve1 :: Problem -> Answer
solve1 = sum . map solve1line 


updateJoltage :: Button -> Joltage -> Joltage
updateJoltage =
    go 0
    where
        go :: Int -> Button -> Joltage -> Joltage
        go _ [] jlt = jlt
        go _ _ [] = []
        go idx (b:bs) (j:js)
            | idx == b = (j+1) : go (idx + 1) bs js
            | otherwise = j : go (idx + 1) (b:bs) js


solve2line :: ([Toggle], [Button], Joltage) -> Int
solve2line (target_state, buttons, target_joltage) = 
    go 0 (sum target_joltage) (map (const Off) target_state) (map (const 0) target_joltage)
    where
        go :: Int -> Int -> [Toggle] -> Joltage -> Int
        go acc best tgs jlt 
            | acc > best = best
            | or $ zipWith (>) jlt target_joltage = best
            | tgs == target_state && jlt == target_joltage = acc
            | otherwise = foldl (\z x -> go (acc+1) z (updateToggles x tgs) (updateJoltage x jlt)) best buttons


solve2 :: Problem -> Answer
solve2 = sum . map solve2line 