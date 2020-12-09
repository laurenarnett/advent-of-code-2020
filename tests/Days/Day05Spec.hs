module Days.Day05Spec (spec) where

import Days.Day05
import Test

testInputA :: ByteString
testInputA =
  [r|BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL
|]

spec :: Spec
spec = do
  describe "Part a" do
    it "Find the highest seat id of three inputs" $
      runA testInputA === 820
  describe "Part b" do
    pass
