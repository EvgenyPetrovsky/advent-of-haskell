module Y25.D10
    ( parse
    , solve1
    , solve2
    ) where

import Data.List.Split (splitOn)
import Data.List (sortBy, nub)
import Data.Map (Map, insert, empty, (!?))


type Toggle = Int

type Cache_Part2 = Map [Int] Int
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
solve2line (_, buttons, target_joltage) =
    let init_state = map negate target_joltage
        in_buttons = sortBy (flip compare) buttons
        worst_best = 3 * maximum target_joltage
    in fst $ go (0, [0], empty :: Cache_Part2) worst_best in_buttons init_state
    where
        go :: (Int, [Int], Cache_Part2) -> Int -> [ButtonWires] -> JoltageCount -> (Int, Cache_Part2)
        go (acc, lkp_key, cache) best bts jlt
            | acc > best = (best, cache_bst)
            | all (==0) jlt = (acc, cache_acc)
            | null bts = (best, cache_bst)
            | deadend1 btn_head jlt = (best, cache_bst)
            | deadend2 jlt = (best, cache_bst)
            | otherwise =
                let upd_joltage = updateJoltage btn_head jlt
                    lkp_key_next = 0:lkp_key
                    (opt_next, upd_cache_next) = 
                        case cache !? lkp_key_next of
                            Just res -> (res, upd_cache_1)
                            Nothing  -> go (acc+0, lkp_key_next, cache) best btn_tail jlt
                    lkp_key_more = (head lkp_key + 1) : tail lkp_key
                    (opt_more, upd_cache_more) = 
                        case upd_cache_next !? lkp_key_more of
                            Just res -> (res, cache)
                            Nothing  -> go (acc+1, lkp_key_more, cache) best bts upd_joltage
                    upd_cache = insert lkp_key opt_next upd_cache_next -- $ . insert lkp_key_next opt2 . insert lkp_key1 opt1 $ upd_cache_2
                in (best `min` opt_next `min` opt_more, upd_cache)
            where
                btn_head = head bts
                btn_tail = tail bts
                cache_bst = insert lkp_key best cache
                cache_acc = insert lkp_key acc cache
        deadend1 :: ButtonWires -> JoltageCount -> Bool
        deadend1 bws jlt =
            let zeros = length $ takeWhile (== 0) bws
            in any (<0) $ take zeros jlt
        deadend2 :: JoltageCount -> Bool
        deadend2 = any (> 0)



presolve2line :: (ToggleArray, [ButtonWires], JoltageCount) -> Int
presolve2line (_, buttons, target_joltage) =
    {-
    error $ 
        "left side solution: " ++ show lt_sol ++ 
        ", org target = " ++ show target_joltage ++
        ", reduced target = " ++ show lt_joltage ++
        ", reduced button wires = " ++ show lt_buttons
    -- | lt_sol /= rt_sol = error $ "can't find solution for target joltage " ++ show target_joltage
    -- | otherwise = lt_sol
    -}
    lt_sol
    where 
        maxlen = 7
        lt_buttons = nub . filter (any (>0)) . map (take maxlen ) $ buttons 
        lt_joltage = take maxlen target_joltage
        lt_sol = solve2line ([], lt_buttons, lt_joltage)
        {-
        len = length target_joltage
        rt_droplen = max 0 $ len - maxlen
        rt_buttons = nub . filter (any (>0)) . map (drop rt_droplen) $ buttons 
        rt_joltage = drop rt_droplen target_joltage
        rt_sol = solve2line ([], rt_buttons, rt_joltage)
        -}


solve2 :: Problem -> Answer
solve2 = sum . map solve2line