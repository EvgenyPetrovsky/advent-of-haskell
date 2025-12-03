{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Y19.D04
    ( parse
    , solve1
    , solve2
    ) where

type Problem = (Int,Int)
type Answer = Int


parse :: String -> Problem
parse i = (read $ take 6 i, read $ drop 7 i)



count :: (Int, Int) -> Int
count (_min, _max) =
    go 0 []
    where
        (mins, maxs) = (show _min, show _max)
        totl = length maxs
        go :: Int -> String -> Int
        go l x
            | not (rules x)          = 0
            | l == totl && hasAdj x = 1
            | l == totl              = 0
            | l <  totl              = sum [go (l+1) (x ++ [dx]) | dx <- ['0'..'9']]
            | otherwise              = 0
        rules :: String -> Bool
        rules x
            | not (lo <= x && x <= hi) = False
            | l <= 1 = True
            | x1 <= x2 = True
            | otherwise = False
            where
                l = length x
                r = reverse x
                (x2,x1) = (head r, r !! 1)
                (lo, hi) = (take l mins, take l maxs)

count2 :: (Int, Int) -> Int
count2 (_min, _max) =
    go 0 []
    where
        (mins, maxs) = (show _min, show _max)
        totl = length maxs
        go :: Int -> String -> Int
        go l x
            | not (rules x)          = 0
            | l == totl
              && not (null (getAdj x))
              && any ((==) 2 . length) (getAdj x) = 1
            | l == totl              = 0
            | l <  totl              = sum [go (l+1) (x ++ [dx]) | dx <- ['0'..'9']]
            | otherwise              = 0

        rules :: String -> Bool
        rules x
            | not (lo <= x && x <= hi) = False
            | l <= 1 = True
            | x1 <= x2 = True
            | otherwise = False
            where
                l = length x
                r = reverse x
                (x2,x1) = (head r, r !! 1)
                (lo, hi) = (take l mins, take l maxs)



hasAdj :: String -> Bool
hasAdj []  = False
hasAdj [_] = False
hasAdj (x1:x2:xs)
    | x1 == x2  = True
    | otherwise = hasAdj (x2:xs)

getAdj :: String -> [String]
getAdj = go ""
    where
        go :: String -> String -> [String]
        go [] [] = []
        go [_] [] = []
        go acc [] = [acc]
        go [] (s:ss) = go [s] ss
        go (a:as) (s:ss)
            | a == s  = go (s:a:as) ss
            | null as = go [s] ss
            | otherwise = (a:as) : go [s] ss


solve1 :: Problem -> Answer
solve1 = count


solve2 :: Problem -> Answer
solve2 = count2
