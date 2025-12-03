module Y19.D01
    ( parse
    , solve1
    , solve2
    ) where

type Problem = [Int]
type Answer = Int


parse :: String -> Problem
parse = map read . lines

solve1 :: Problem -> Answer
solve1 = sum . map calc
    where
        calc :: Int -> Int
        calc x = x `div` 3 - 2

solve2 :: Problem -> Answer
solve2 = sum . map fuel_per_module
    where
        fuel_per_module x = fuel_for_cargo x + fuel_for_fuel (fuel_for_cargo x)

        fuel_for_fuel f
          | f == 0 = 0
          | otherwise = calc f + fuel_for_fuel (calc f)
        fuel_for_cargo = calc
        calc :: Int -> Int
        calc x = max 0 $ x `div` 3 - 2
