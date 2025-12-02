module Y25D02
    ( parse
    , solve1
    , solve2
    ) where

import Util.Data (replace)


type Problem = [Range]
type Range = (Int, Int)
type Answer = Int


parserange :: String -> Range
parserange str =
    let ints = map read . words $ replace str '-' ' ' :: [Int]
    in (head ints, head $ tail ints)


isInvalid1 :: Int -> Bool
isInvalid1 n =
    let strn = show n
        lenn = length strn
        half = lenn `div` 2
    in uncurry (==) $ splitAt half strn


isInvalid2 :: Int -> Bool
isInvalid2 n =
    let strn = show n
        lenn = length strn
        half = lenn `div` 2
    in any (\l -> uncurry go (splitAt l strn)) [1..half]
    where 
        go :: String -> String -> Bool
        go _ [] = True
        go start remainder =
            let 
                (p1, p2) = splitAt (length start) remainder
            in start == p1 && go start p2


parse :: String -> Problem
parse text = map parserange . words $ replace text ',' ' '


solve1 :: Problem -> Answer
solve1 = sum . concatMap (\(from,to) -> filter isInvalid1 [from..to])


solve2 :: Problem -> Answer
solve2 = sum . concatMap (\(from,to) -> filter isInvalid2 [from..to])