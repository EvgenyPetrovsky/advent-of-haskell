module Y15D20
    ( parse
    , solve1
    , solve2
    ) where

type Problem = Int

type Answer = Int

parse :: String -> Problem
parse = read

allDivisors :: Int -> [Int]
allDivisors n = filter (\x -> n `rem` x == 0) [1..n]

allDivisors' :: Int -> [Int]
allDivisors' n = map fst . filter (\(_,r) -> r == 0) . map (n `divMod` ) $ [1..50]


solve1 :: Problem -> Answer
solve1 n =
    let f x = 10 * sum (allDivisors x)
    in (+) 1 . length . takeWhile (< n) $ map f [1..(n `div` 10)]

solve2 :: Problem -> Answer
solve2 n =
    let f x = 11 * sum (allDivisors' x)
    in (+) 1 . length . takeWhile (< n) $ map f [1..(n `div` 11)]
