module Y15.D02
    ( parse
    , solve1
    , solve2
    ) where

import Data.List.Split (splitOn)

type Dims = (Int, Int, Int)
type Problem = [Dims]
type Answer = Int

{-
(|>) :: a -> (a -> b) -> b
infixl 5 |>

a |> f = f a
-}

parseOneLine :: String -> Dims
parseOneLine l =
    let delimiter = "x"
        nums =  map (\x -> read x :: Int) . splitOn delimiter $ l
    in fillTriple nums
    where
        fillTriple :: [Int] -> Dims
        fillTriple (a:b:c:_) = (a,b,c)
        fillTriple i = error $ "Incorrect input format: " ++ show i ++ ". Expected format is [<Int>, <Int>, <Int>]"

parse :: String -> Problem
parse = map parseOneLine . lines

boxWrapSurface :: Dims -> Int
boxWrapSurface (a,b,c) =
    let sideSurfaces :: [Int]
        sideSurfaces = [a*b, b*c, c*a]
        addon :: Int
        addon = minimum sideSurfaces
    in
        addon + (sum . map (2 *) $ sideSurfaces)

boxRibbonLength :: Dims -> Int
boxRibbonLength (a,b,c) =
    let minPerimeter = minimum . map (2 *) $ [a+b, b+c, c+a]
        bowLength = a * b * c
    in minPerimeter + bowLength

solve1 :: Problem -> Answer
solve1 = sum . map boxWrapSurface

solve2 :: Problem -> Answer
-- we can take length (and not length + 1 because sequence scan will prepend init element)
solve2 = sum . map boxRibbonLength