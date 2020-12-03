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
  pure $ listArray ((1, 1), (n, m)) (concat rs)

rowParser :: Parser [Bool]
rowParser = do
  s <- takeWhile1 (/= '\n')
  return $ B.foldr (\x acc -> (x == '#') : acc) [] s

------------ TYPES ------------
type Input = Array (Int, Int) Bool

type OutputA = Void

type OutputB = Void

------------ PART A ------------
partA :: Input -> OutputA
partA = error "Not implemented yet!"

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
