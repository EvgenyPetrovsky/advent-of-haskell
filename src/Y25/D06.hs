module Y25.D06
    ( parse
    , solve1
    , solve2
    ) where
import Data.List (transpose)
import Data.List.Split (splitWhen)

type Op = Int -> Int -> Int
type Val = Int
type Task = (Op, [Val])
data Problem = Problem { part1 :: [Task], part2 :: [Task] }
type Answer = Int

parse :: String -> Problem
parse input_text =
    Problem { part1 = zip ops vals1, part2 = zip ops vals2 }
    where
        input_lines = lines input_text
        op_line = last input_lines
        val_lines = take (length input_lines - 1) input_lines
        ops = map op_parser . filter (/= "") . words $ op_line
        vals1 = map (map (\x -> read x :: Val)) . transpose . map (filter (/= "") . words) $ val_lines
        vals2 = map (map read) . splitWhen (all (== ' ')) . transpose $ val_lines
        op_parser :: String -> Op
        op_parser "+" = (+)
        op_parser "*" = (*)
        op_parser op  = error $ "unknown operation: " ++ op


solve1 :: Problem -> Answer
solve1 = sum . map (uncurry foldl1) . part1


solve2 :: Problem -> Answer
solve2 = sum . map (uncurry foldl1) . part2
