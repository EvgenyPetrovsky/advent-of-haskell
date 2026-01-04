module Main (main) where

import Test.Hspec
import qualified Test.Y25.D10 as D10

main :: IO ()
main = hspec $ do
  D10.spec
