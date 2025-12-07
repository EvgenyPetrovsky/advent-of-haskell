module Util.AOC
    ( (|>)
    , (.-.)
    , (.+.)
    ) where

(|>) :: a -> (a -> b) -> b
infixl 1 |>
a |> f = f a


(.-.) :: Num a => (a,a) -> (a,a) -> (a,a)
infixl 6 .-.
(a1,b1) .-. (a2,b2) = (a1-a2, b1-b2)


(.+.) :: Num a => (a,a) -> (a,a) -> (a,a)
infixl 6 .+.
(a1,b1) .+. (a2,b2) = (a1+a2, b1+b2)
