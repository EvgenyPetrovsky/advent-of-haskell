module Util.Compute
    ( (|>)
    --, applyfold
    ) where

(|>) :: a -> (a -> b) -> b
infixl 5 |>

a |> f = f a

--applyfold :: (b -> b -> c) -> [a -> b] -> (a -> c)
--applyfold folder fs = foldl' folder . map fs