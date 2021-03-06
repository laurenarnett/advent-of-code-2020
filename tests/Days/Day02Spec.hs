module Days.Day02Spec (spec) where

import Days.Day02
import Test

testInput :: ByteString
testInput =
  [r|1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc|]

spec :: Spec
spec = do
  describe "Part a" do
    it "find the number of valid passwords" $
      runA testInput === 2
  describe "Part b" do
    it "find the number of valid passwords under the new policy" $
      runB testInput === 1
