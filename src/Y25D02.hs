module Y25D02
    ( parse
    , solve1
    , solve2
    ) where

import Util.Data (replace)
import Data.List (isPrefixOf)

type Problem = [Range]
type Range = (Int, Int)
type Answer = Int

type Distance = Int
data Rotation = L Distance | R Distance deriving (Show)

parserange :: String -> Range
parserange str =
    let ints = map read . words $ replace str '-' ' ' :: [Int]
    in (head ints, head $ tail ints)


len :: Int -> Int
len n
    | n < 10^(1  :: Int) = 1
    | n < 10^(2  :: Int) = 2
    | n < 10^(3  :: Int) = 3
    | n < 10^(4  :: Int) = 4
    | n < 10^(5  :: Int) = 5
    | n < 10^(6  :: Int) = 6
    | n < 10^(7  :: Int) = 7
    | n < 10^(8  :: Int) = 8
    | n < 10^(9  :: Int) = 9
    | n < 10^(10 :: Int) = 10
    | n < 10^(11 :: Int) = 11
    | n < 10^(12 :: Int) = 12
    | otherwise = error $ "number is too big: " ++ show n

isInvalidold :: Int -> Bool
isInvalidold n =
    go (show n)
    where
        go :: String -> Bool
        go [] = False
        go strn =
            let lenn = length strn
                half = lenn `div` 2
                pairs = map (\l -> (take l strn, take l $ drop l strn)) [1..half]
            in any (uncurry (==)) pairs || go (tail strn)

isInvalid1 :: Int -> Bool
isInvalid1 n =
    let strn = show n
        lenn = length strn
        half = lenn `div` 2
    in take half strn == drop half strn

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
solve1 problem = 
    let invalids = concatMap (\(from,to) -> filter isInvalid1 [from..to]) problem
    in sum invalids

solve2 :: Problem -> Answer
solve2 problem =
    let invalids = concatMap (\(from,to) -> filter isInvalid2 [from..to]) problem
    in sum invalids
