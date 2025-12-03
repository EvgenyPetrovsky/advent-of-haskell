module Y15.D25
    ( parse
    , solve1
    , solve2
    ) where

type Problem = (Int, Int)
type Answer = Int


parse :: String -> Problem
parse _ = (3010, 3019)
--parse _ = (2, 1)

start :: Int
start = 20151125


findIdx :: Int -> Int -> Int
findIdx r c = c + sum [1..(r+c-1-1)]


generate :: Int -> Int -> Int
generate acc n
    | n < 1 = error "something went wrong here"
    | n == 1 = acc
    | otherwise = generate (acc * 252533 `rem` 33554393) (n-1)

solve1 :: Problem -> Answer
solve1 (r,c) =
    let idx = findIdx r c
        answer = generate start idx
    --in error (show (r,c) ++ "|" ++ show answer ++ "|" ++ show idx)
    in answer


solve2 :: Problem -> Answer
solve2 = error "not implemented"