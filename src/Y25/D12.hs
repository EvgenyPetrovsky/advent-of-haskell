module Y25.D12
    ( parse
    , solve1
    , solve2
    ) where


import Data.List.Split (splitOn)



type Shape = [String]
data Region = Region { area :: (Int, Int), shape_counts :: [Int] } deriving Show
data Problem = Problem { shapes :: [Shape], regions :: [Region] } deriving Show
type Answer = Int


parse :: String -> Problem
parse text_input =
    let _shapes  = init . splitOn [""] . lines $ text_input
        _regions = last . splitOn [""] . lines $ text_input
    in Problem {
        shapes = _shapes,
        regions = map (\r -> Region {area = parse_area r, shape_counts = parse_counts r}) _regions
    }
    where
        parse_area :: String -> (Int, Int)
        parse_area = (\x -> (head x, x !! 1)) . map read . splitOn "x" . head . splitOn ": "
        parse_counts :: String -> [Int]
        parse_counts = map read . words . last . splitOn ": "


solve1 :: Problem -> Answer
solve1 Problem {shapes = _, regions = _regions} =
    --error $ show _regions
    length $ filter check _regions
    where
        check :: Region -> Bool
        check Region {area = (w,l), shape_counts = _c} =
            w * l >= 7 * sum _c


solve2 :: Problem -> Answer
solve2 = undefined