module Days.Day02 (runDay, Input, OutputA, OutputB, runA, runB) where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B

runDay :: Bool -> String -> IO ()
runDay = run inputParser partA partB

runA :: ByteString -> Either String OutputA
runA input = runPart input inputParser partA

runB :: ByteString -> Either String OutputB
runB input = runPart input inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy passwordParser endOfLine

passwordParser :: Parser Password
passwordParser = do
  min <- decimal
  max <- char '-' *> decimal
  tgt <- space *> anyChar <* char ':' <* space
  pwd <- takeTill (== '\n')
  pure $ Password {..}

------------ TYPES ------------
type Input = [Password]

type OutputA = Int

type OutputB = Int

data Password = Password
  { min :: Int, max :: Int, tgt :: Char, pwd :: ByteString }
  deriving (Show)

------------ PART A ------------
partA :: Input -> OutputA
partA = foldr (\x acc -> if checkValidPassword x then acc + 1 else acc) 0

checkValidPassword :: Password -> Bool
checkValidPassword Password {..} = if | B.count tgt pwd < min -> False
                                      | B.count tgt pwd > max -> False
                                      | otherwise -> True
                                               

------------ PART B ------------
partB :: Input -> OutputB
partB = foldr (\x acc -> if checkNewPolicy x then acc + 1 else acc) 0

checkNewPolicy :: Password -> Bool
checkNewPolicy Password {..} = (B.index pwd (min - 1) == tgt) /= (B.index pwd (max - 1) == tgt)
