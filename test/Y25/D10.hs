module Test.Y25.D10 (spec) where

import Test.Hspec
import Y25.D10

spec :: Spec
spec = describe "Y25.D10" $ do
  let line = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
      (tgt, bts, jlt) = head (parse line)

  it "parses target toggles" $
    tgt `shouldBe` [0,1,1,0]

  it "parses button wires count" $
    length bts `shouldBe` 6

  it "parses joltage counts" $
    jlt `shouldBe` [3,5,4,7]

  it "solve1 for example" $
    solve1 (parse line) `shouldBe` 2