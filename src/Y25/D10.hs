module Y25.D10
    ( parse
    , solve1
    , solve2
    ) where

import Data.List.Split (splitOn)


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


wireButton :: [Int] -> ToggleArray -> ToggleArray
wireButton idxs = zipWith (\i _ -> if i `elem` idxs then 1 else 0) [0..]


solve1line :: (ToggleArray, [ButtonWires], JoltageCount) -> Int
solve1line (target_state, buttons, _) =
    go 0 infeasible_solution (map (const 0) target_state) buttons
    where
        infeasible_solution = 1000000
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


allToggleSolutions :: ToggleArray -> [ButtonWires] -> [[ButtonWires]]
allToggleSolutions target_state =
    go [] (map (const 0) target_state)
    where
        go :: [ButtonWires] -> ToggleArray -> [ButtonWires] -> [[ButtonWires]]
        go acc tgs bts
            | tgs == target_state = [acc]
            | null bts = []
            | otherwise =
                let opt1 = go (acc ++ [head bts]) (updateToggles (head bts) tgs) (tail bts)
                    opt2 = go acc tgs (tail bts)
                in opt1 ++ opt2


solve2line :: (ToggleArray, [ButtonWires], JoltageCount) -> Int
solve2line (_dummy_toggles, buttons, target_joltage)
    | all (== 0) target_joltage = 0
    | any (< 0) target_joltage = infeasible_solution
    | null all_solutions = infeasible_solution
    | otherwise =
        minimum $
        zipWith (\solution hlf_joltage ->
            length solution +
            2 * solve2line (_dummy_toggles, buttons, hlf_joltage)
        ) all_solutions hlf_joltages
    where
        infeasible_solution = 1000000
        odd_toggles   = map (`mod` 2) target_joltage
        all_solutions = allToggleSolutions odd_toggles buttons
        rem_joltages  = map (foldl (zipWith (-)) target_joltage) all_solutions
        hlf_joltages  = map (map (`div` 2)) rem_joltages


solve2 :: Problem -> Answer
solve2 = sum . map solve2line