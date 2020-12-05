module Days.Day03 (runDay, Input, OutputA, OutputB, runA, runB) where

import Data.Attoparsec.ByteString.Char8
import Data.Array
import qualified Data.ByteString.Char8 as B hiding (take)

runDay :: Bool -> String -> IO ()
runDay = run inputParser partA partB

runA :: ByteString -> Either String OutputA
runA input = runPart input inputParser partA

runB :: ByteString -> Either String OutputB
runB input = runPart input inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  rs@(r:_) <- sepBy rowParser endOfLine
  let n = length rs
      m = length r
  pure $ listArray ((0, 0), (n - 1, m - 1)) (concat rs)

rowParser :: Parser [Bool]
rowParser = do
  s <- takeWhile1 (/= '\n')
  return $ B.foldr (\x acc -> (x == '#') : acc) [] s

------------ TYPES ------------
type Input = Array (Int, Int) Bool

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA arr = countTrees arr 0 0

countTrees :: Input -> Int -> Int -> Int
countTrees arr n m = if | n == (rows + 1) -> 0
                        | arr ! (n, m) -> 1 + countTrees arr (n + 1) ((m + 3) `mod` (cols + 1))
                        | not (arr ! (n, m)) -> countTrees arr (n + 1) ((m + 3) `mod` (cols + 1))
  where (_, (rows, cols)) = bounds arr

------------ PART B ------------
partB :: Input -> OutputB
partB arr = countHappyTrees arr 1 1
          * countHappyTrees arr 1 3
          * countHappyTrees arr 1 5
          * countHappyTrees arr 1 7
          * countHappyTrees arr 2 1

countHappyTrees :: Input -> Int -> Int -> Int
countHappyTrees arr rise run = go 0 0 
  where
    go n m = 
        if | n >= (fst (snd (bounds arr)) + 1) -> 0
           | arr ! (n, m) -> 1 + go (n + rise) ((m + run) `mod` (snd (snd (bounds arr)) + 1))
           | not (arr ! (n, m)) -> go (n + rise) ((m + run) `mod` (snd (snd (bounds arr)) + 1))
