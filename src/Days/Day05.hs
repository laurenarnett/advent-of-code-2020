module Days.Day05 (runDay, Input, OutputA, OutputB, runA, runB) where

import Data.Attoparsec.ByteString.Char8
import Prelude hiding (take)
import qualified Data.ByteString.Char8 as B
import Data.List hiding (take)

runDay :: Bool -> String -> IO ()
runDay = run inputParser partA partB

runA :: ByteString -> Either String OutputA
runA input = runPart input inputParser partA

runB :: ByteString -> Either String OutputB
runB input = runPart input inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy1 seatParser endOfLine

seatParser :: Parser ([Row], [Col])
seatParser = do
  row <- take 7
  col <- take 3
  pure (toRow row, toCol col)

toRow :: ByteString -> [Row]
toRow = B.foldr (\x acc -> if x == 'F' then F : acc else B : acc) []
                                   
toCol :: ByteString -> [Col]      
toCol = B.foldr (\x acc -> if x == 'R' then R : acc else L : acc) []
  
------------ TYPES ------------
type Input = [([Row], [Col])]

type OutputA = Int

type OutputB = Int

data Row = F | B deriving (Show)

data Col = R | L deriving (Show)

------------ PART A ------------
partA :: Input -> OutputA
partA = foldr ((\x acc -> if x > acc then x else acc) . computeSeatID) 0

computeRow :: [Row] -> Int
computeRow dirs = go dirs 0 127
  where
    go d lower upper =
      case d of
        [x] -> case x of F -> lower
                         B -> upper
        (x:xs) -> case x of F -> go xs lower ((upper - lower) `div` 2 + lower)
                            B -> go xs (((upper - lower) `div` 2) + lower + 1) upper

computeCol :: [Col] -> Int
computeCol dirs = go dirs 0 7
  where
    go d lower upper =
      case d of
        [x] -> case x of L -> lower
                         R -> upper
        (x:xs) -> case x of L -> go xs lower ((upper - lower) `div` 2 + lower)
                            R -> go xs (((upper - lower) `div` 2) + lower + 1) upper

compute :: Eq a => [a] -> a -> Int -> Int
compute dirs upperDir high = go dirs 0 high
  where
    go [x] lower upper = if x == upperDir then upper else lower
    go (x:xs) lower upper
      | x /= upperDir = go xs lower ((upper - lower) `div` 2 + lower)
      | otherwise = go xs (((upper - lower) `div` 2) + lower + 1) upper

computeSeatID :: ([Row], [Col]) -> Int
computeSeatID (r, c) = computeRow r * 8 + computeCol c
  
------------ PART B ------------
partB :: Input -> OutputB
partB = findMissingSeat . orderSeats

findMissingSeat :: [Int] -> Int
findMissingSeat seatIds = case [(minimum seatIds)..(maximum seatIds)] \\ seatIds of [x] -> x
                                                                                    _ -> error "Failed to find your seat id"
orderSeats :: Input -> [Int]
orderSeats seats = sort (map computeSeatID seats)
