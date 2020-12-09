module Days.Day08Spec (spec) where

import Days.Day08
import Test

testInput =
  [r|nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
|]

spec :: Spec
spec = do
  describe "Part a" do
    it "check the sum of acc after running all instructions" $
      runA testInput === 5
  describe "Part b" do
    it "find the sum of the acc of the non cyclic instruction list" $
      runB testInput === 8
