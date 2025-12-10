module Y25.D10
    ( parse
    , solve1
    , solve2
    ) where

import Data.List.Split (splitOn)
import Data.List (sortBy)


type Toggle = Int

type ToggleArray  = [Toggle]
type ButtonWires  = [Toggle]
type JoltageCount = [Int]

type Problem = [(ToggleArray, [ButtonWires], JoltageCount)]
type Answer = Int


{- [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7} -}
parseline :: String -> (ToggleArray, [ButtonWires], JoltageCount)
parseline line =
    ( tgt, bws, jlt)
    where
        pieces = words line
        tgt = parse_tgt $ head pieces
        bws = map parse_bts . tail . init $ pieces
        jlt = parse_jlt $ last pieces
        parse_tgt :: String -> [Toggle]
        parse_tgt = map (\c -> if c == '#' then 1 else 0) . tail . init
        parse_bts :: String -> ButtonWires
        parse_bts = (`wireButton` tgt) . map (\p -> read p :: Int) . splitOn "," . tail . init
        parse_jlt :: String -> JoltageCount
        parse_jlt = map (\p -> read p :: Int) . splitOn "," . tail . init


parse :: String -> Problem
parse = map parseline . lines


updateToggles :: ButtonWires -> ToggleArray -> ToggleArray
updateToggles = zipWith (\x y -> (x+y) `mod` 2)


updateJoltage :: ButtonWires -> JoltageCount -> JoltageCount
updateJoltage = zipWith (+)


wireButton :: [Int] -> ToggleArray -> ToggleArray
wireButton idxs = zipWith (\i _ -> if i `elem` idxs then 1 else 0) [0..]


solve1line :: (ToggleArray, [ButtonWires], JoltageCount) -> Int
solve1line (target_state, buttons, _) =
    go 0 worst (map (const 0) target_state) buttons
    where
        worst = 999
        go :: Int -> Int -> [Toggle] -> [ButtonWires] -> Int
        go acc best tgs bts
            | acc > best = best
            | tgs == target_state =acc
            | null bts   = best
            | otherwise  = 
                go (acc+1) best (updateToggles (head bts) tgs) (tail bts)
                `min`
                go acc best tgs (tail bts)


solve1 :: Problem -> Answer
solve1 = sum . map solve1line


solve2line :: (ToggleArray, [ButtonWires], JoltageCount) -> Int
solve2line (target_state, buttons, target_joltage) =
    let init_state = map (const 0) target_state
        in_buttons = sortBy (flip compare) buttons
        worst_best = sum target_joltage
    in go 0 worst_best init_state in_buttons init_state
    where
        go :: Int -> Int -> ToggleArray -> [ButtonWires] -> JoltageCount -> Int
        go acc best tgs bts jlt
            | acc > best = best
            | {- tgs == target_state && -} jlt == target_joltage = acc
            | null bts = best
            | deadend1 btn_head jlt = best
            | deadend2 jlt = best
            | otherwise =
                go (acc+1) best (updateToggles btn_head tgs) bts (updateJoltage btn_head jlt)
                `min`
                go (acc+0) best tgs btn_tail jlt
            where
                btn_head = head bts
                btn_tail = tail bts
        deadend1 :: ButtonWires -> JoltageCount -> Bool
        deadend1 bws jlt =
            let zeros = length $ takeWhile (== 0) bws
            in or . take zeros $ zipWith (<) jlt target_joltage
        deadend2 :: JoltageCount -> Bool
        deadend2 jlt = 
            or $ zipWith (>) jlt target_joltage


solve2 :: Problem -> Answer
solve2 = sum . map solve2line