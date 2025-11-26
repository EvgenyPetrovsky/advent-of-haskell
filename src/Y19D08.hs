module Y19D08
    ( parse
    , solve1
    , solve2
    ) where

import Data.Char (digitToInt)

type Problem = [Int]
type Answer = String

parse :: String -> Problem
parse = map digitToInt

width :: Int
width  = 25 :: Int
height :: Int
height = 6  :: Int
layersize :: Int
layersize = width * height


split :: Int -> [a] -> [[a]]
split _ [] = []
split n elems = take n elems : split n (drop n elems)


count :: [Int] -> Int -> Int
count [] _ = 0
count (v:vs) val
    | v == val = 1 + count vs val
    | otherwise = count vs val


solve1 :: Problem -> Answer
solve1 input =
    let layers = split layersize input
        count0 = map (`count` 0) layers
        min0ct = minimum count0
        max0lr = fst . head . filter (\(_,cnt) ->  cnt == min0ct) $ zip layers count0
    in show $ (max0lr `count` 1) * (max0lr `count` 2)


solve2 :: Problem -> Answer
solve2 input =
    let blacklayer = replicate layersize 1 :: [Int]
        layers = split layersize input
        digitimage = foldr (zipWith (\top bot -> if top == 2 then bot else top)) blacklayer layers
        textimage  = map convert2text digitimage
        linedtext  = split 25 textimage
    in unlines ("":linedtext)
    where
        pxBlack = ' '
        pxWhite = '*'
        convert2text :: Int -> Char
        convert2text 0 = pxBlack
        convert2text 1 = pxWhite
        convert2text _ = '?'



