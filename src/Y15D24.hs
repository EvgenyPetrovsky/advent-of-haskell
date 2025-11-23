module Y15D24
    ( parse
    , solve1
    , solve2
    ) where

import Data.Set (fromList, toAscList, difference)
import Data.List (nub)

type Problem = [Int]
type Answer = Int


parse :: String -> Problem
parse = map read . lines

sublists :: [Int] -> Int -> Int -> [[Int]]
sublists list max_sum max_len =
    all_lists list []
    where
        all_lists :: [Int] -> [Int] -> [[Int]]
        all_lists [] acc
            | sum acc == max_sum = [acc]
            | otherwise = []
        all_lists (r:rs) acc
            | sum acc > max_sum = []
            | length acc > max_len = []
            | otherwise = all_lists rs (r:acc) ++ all_lists rs acc


solve1 :: Problem -> Answer
solve1 packages =
    least_quantum_entanglement
    where
        max_sum = sum packages `div` 3
        org_len = length packages
        max_len = org_len `div` 3
        --message1 = "total number: " ++ show (sum packages) ++ "; divMod 3: " ++ show (sum packages `divMod` 3) ++ "."
        --message2 = "number of sublists identified: " ++ show (length all_subs) ++ "; shortest length: " ++ show (minimum [length l | l <- all_subs])
        all_subs = sublists packages max_sum max_len
        val_subs = nub [s |
                      s <- all_subs,
                      ss <- take 1 $ sublists (packages `diff` s) max_sum org_len,
                      not . null $ ss]
        min_len = minimum [length x | x <- val_subs]
        min_len_subs = [x | x <- val_subs, length x == min_len]
        least_quantum_entanglement = minimum [product s | s <- min_len_subs]

diff :: [Int] -> [Int] -> [Int]
diff a b = toAscList (fromList a `difference` fromList b)

solve2 :: Problem -> Answer
solve2 packages =
    least_quantum_entanglement
    where
        max_sum = sum packages `div` 4
        org_len = length packages
        max_len = org_len `div` 4
        all_subs = sublists packages max_sum max_len
        val_subs = [
            s | s <- all_subs,
                ss <- take 1 [
                    ss | ss <- sublists (packages `diff` s) max_sum org_len,
                    sss <- take 1 $ sublists (packages `diff` s `diff` ss) max_sum org_len,
                    not . null $ sss],
                not . null $ ss]

        min_len = minimum [length x | x <- val_subs]
        min_len_subs = [x | x <- val_subs, length x == min_len]
        least_quantum_entanglement = minimum [product s | s <- min_len_subs]
