module Y25.D03
    ( parse
    , solve1
    , solve2
    ) where


type Battery = String
type Problem = [Battery]
type Answer = Int


parse :: String -> Problem
parse = lines

findMaxJoltage :: Battery -> Int
findMaxJoltage b =
    let d1 = maximum . tail $ reverse b
        d2 = maximum . tail $ dropWhile (< d1) b
    in read [d1,d2]


findNarrayJoltage :: Battery -> Int -> Int
findNarrayJoltage bat size =
    read $ go bat size
    where
        go :: Battery -> Int -> Battery
        go _ 0 = []
        go [] _ = error "something went wrong"
        go _bat _size  =
            let maxel = maximum . drop (_size - 1) . reverse $ _bat
                newbt = tail . dropWhile (< maxel) $ _bat
            in maxel : go newbt (_size - 1)


solve1 :: Problem -> Answer
solve1 = sum . map findMaxJoltage


solve2 :: Problem -> Answer
solve2 = sum . map (`findNarrayJoltage` 12)