module Days.Day01 (runDay, Input, OutputA, OutputB, runA, runB) where

import Data.Attoparsec.Text
import qualified Data.Vector as V

runDay :: Bool -> String -> IO ()
runDay = run inputParser partA partB

runA :: Text -> Either String OutputA
runA input = runPart input inputParser partA

runB :: Text -> Either String OutputB
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
partA ns = let sorted = sort ns
               v = V.fromList sorted
               (x, y) = findPair v 0 (V.length v - 1) in
             x * y

findPair :: Vector Int -> Int -> Int -> (Int, Int)
findPair v i j = let l = v V.! i
                     r = v V.! j
                     sum = l + r in
                   case sum of 2020 -> (l, r)
                               x | x > 2020 -> findPair v i (j - 1)
                               x | x < 2020 -> findPair v (i + 1) j

------------ PART B ------------
partB :: Input -> OutputB
partB ns = let sorted = sort ns
               v = V.fromList sorted
               (x, y, z) = findHappyTriple v 0 1 2 in
             x * y * z
               
findHappyTriple :: Vector Int -> Int -> Int -> Int -> (Int, Int, Int)
findHappyTriple v i j k = let l = v V.! i
                              m = v V.! j
                              r = v V.! k
                              sum = l + m + r
                          in if | sum == 2020 -> (l, m, r)
                                | k == (V.length v - 1) && j == k - 1 -> findHappyTriple v (i + 1) (i + 2) (i + 3)
                                | k == (V.length v - 1) -> findHappyTriple v i (j + 1) (j + 2)
                                | otherwise -> findHappyTriple v i j (k + 1)
