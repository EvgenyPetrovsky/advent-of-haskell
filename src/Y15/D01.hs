module Y15.D01
    ( parse
    , solve1
    , solve2
    ) where

type Problem = String
type Answer = Int

{-
(|>) :: a -> (a -> b) -> b
infixl 5 |>

a |> f = f a
-}

parse :: String -> Problem
parse = head . lines

parenToNum :: Char -> Int
parenToNum '(' = 1
parenToNum ')' = -1
parenToNum _   = 0

solve1 :: Problem -> Answer
solve1 = sum . map parenToNum

solve2 :: Problem -> Answer
-- we can take length (and not length + 1 because sequence scan will prepend init element)
solve2 = length . takeWhile (>= 0) .scanl (+) 0 . map parenToNum