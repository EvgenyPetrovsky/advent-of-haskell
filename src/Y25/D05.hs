module Y25.D05
    ( parse
    , solve1
    , solve2
    ) where


type Range = (Int, Int)
type Item  = Int
type Problem = ([Range], [Item])
type Answer = Int


parse :: String -> Problem
parse input_text =
    let (rng, num) = span (/= "") $ lines input_text
    in (map parse_rng rng, map read $ tail num)
    where
        parse_rng :: String -> (Int, Int)
        parse_rng = (\(a, b) -> (read a, read $ tail b)) . span ( /= '-' ) 


solve1 :: Problem -> Answer
solve1 (rngs, nums) = 
    length . filter is_fresh $ nums
    where 
        is_fresh :: Int -> Bool 
        is_fresh num = any ( \(lower, upper) -> lower <= num && num <= upper ) rngs


{-
    while merging 
    - find first interval in the list that overlaps with or attaches to interval to insert
      - combine them
      - run merge for newly created interval 
    - otherwise add interval into the list

-}
merge :: Range -> [Range] -> [Range]
merge rng =
    go []
    where 
        can_merge1 :: Range -> Range -> Bool
        can_merge1 (l1,u1) (l2,u2) = l2 <= u1 + 1 && l1 <= u2 + 1
        merge1 :: Range -> Range -> Range
        merge1 r1@(l1,u1) r2@(l2,u2)
            | r1 `can_merge1` r2 = (min l1 l2, max u1 u2)
            | otherwise       = error "Can't merge non-overlapping "
        go :: [Range] -> [Range] -> [Range]
        go acc [] = reverse (rng:acc)
        go acc (r:rs) 
            | rng `can_merge1` r = merge (rng `merge1` r) (reverse acc ++ rs)
            | otherwise = go (r:acc) rs


solve2 :: Problem -> Answer
solve2 (rngs, _) = 
    let new_ranges = foldr merge [] rngs
    in sum . map ( \(l, u) -> u - l + 1) $ new_ranges
