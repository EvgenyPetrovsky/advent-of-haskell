module Util.Data (
      ints
    , replace
    , replaceif
) where 

ints :: String -> [Int]
ints input =
    let f x = x < '0' || x > '9'
    in map read . words $ replaceif input f ' ' 

replaceif :: [a] -> (a -> Bool) -> a -> [a]
replaceif [] _ _ = []
replaceif (x:xs) f b
    | f x       = b : replaceif xs f b
    | otherwise = x : replaceif xs f b

replace :: Eq a => [a] -> a -> a -> [a]
replace [] _ _ = []
replace (x:xs) a b
    | x == a    = b : replace xs a b
    | otherwise = x : replace xs a b

