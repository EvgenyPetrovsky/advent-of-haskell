module Util.AOC
    ( (|>)
    ) where

(|>) :: a -> (a -> b) -> b
infixl 5 |>

a |> f = f a
