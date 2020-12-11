module Days.Day10 (runDay, Input, OutputA, OutputB, runA, runB) where

import Data.Attoparsec.ByteString.Char8
import Data.List
import Control.Monad.Memo

runDay :: Bool -> String -> IO ()
runDay = run inputParser partA partB

runA :: ByteString -> Either String OutputA
runA input = runPart input inputParser partA

runB :: ByteString -> Either String OutputB
runB input = runPart input inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy decimal endOfLine

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input = let (x, y) = (filterAdaptorDiffs . diffAdaptors) (sort (0 : input)) in
  x * y

filterAdaptorDiffs :: [Int] -> (Int, Int)
filterAdaptorDiffs diffs = let (ones, others) = partition (== 1) diffs
                               threes = filter (== 3) others in
                             (length ones, (length threes) + 1)
  

diffAdaptors :: Input -> [Int]
diffAdaptors input@(_:tl) = zipWith (\x y -> y - x) input tl

------------ PART B ------------
partB :: Input -> OutputB
partB input = startEvalMemo (nConfigurations (sort ((0 : input) ++ [(maximum input) + 3]))) `div` 2

nConfigurations :: [Int] -> Memo [Int] Int Int
nConfigurations (_ : [_]) = pure 1
nConfigurations (x:y:xs) =
  if y - x <= 3
     then liftA2 (+) (memo nConfigurations (x : xs)) (memo nConfigurations (y : xs))
     else pure 0
