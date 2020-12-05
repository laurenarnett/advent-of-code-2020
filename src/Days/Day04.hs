{-# LANGUAGE DuplicateRecordFields #-}
module Days.Day04 (runDay, Input, OutputA, OutputB, runA, runB) where

import Data.Attoparsec.ByteString.Char8
import Data.Map ((!?))
import qualified Data.ByteString.Char8 as B

runDay :: Bool -> String -> IO ()
runDay = run inputParser partA partB

runA :: ByteString -> Either String OutputA
runA input = runPart input inputParser partA

runB :: ByteString -> Either String OutputB
runB input = runPart input inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = map passportMaker <$> sepBy1 passportParser endOfLine

passportMaker :: Map ByteString ByteString -> Maybe Passport
passportMaker p =
  do
    pid <- p !? "pid"
    byr <- p !? "byr"
    iyr <- p !? "iyr"
    eyr <- p !? "eyr"
    hgt <- p !? "hgt"
    ecl <- p !? "ecl"
    hcl <- p !? "hcl"
    pure $ Passport {..}

passportParser :: Parser (Map ByteString ByteString)
passportParser =
  let fieldParser = do
        attr <- takeWhile1 isAlpha_ascii <* char ':'
        val <- takeTill (`elem` [' ', '\n'])
        space
        pure (attr, val) in
    do
      fields <- some fieldParser
      pure $ fromList fields

------------ TYPES ------------
type Input = [Maybe Passport]

type OutputA = Int

type OutputB = Int

data Passport = Passport { pid :: ByteString, byr :: ByteString, iyr :: ByteString, eyr :: ByteString, ecl :: ByteString, hcl :: ByteString, hgt :: ByteString } deriving (Show)

------------ PART A ------------
partA :: Input -> OutputA
partA = foldr (\x acc -> countPassport x + acc) 0

countPassport :: Maybe Passport -> Int
countPassport p = case p of Just _ -> 1
                            Nothing -> 0

------------ PART B ------------
partB :: Input -> OutputB
partB = foldr (\x acc -> if validatePassport x then acc + 1 else acc) 0

validatePassport :: Maybe Passport -> Bool
validatePassport p =
  case p of Just Passport {..} -> validByr byr && validIyr iyr && validEyr eyr && validHgt hgt && validHcl hcl && validEcl ecl && validPid pid
            _ -> False

validByr :: ByteString -> Bool
validByr b = case parseOnly decimal' b of
  Right byr -> byr >= 1920 && byr <= 2002
  Left _ -> False

validIyr :: ByteString -> Bool
validIyr i = case parseOnly decimal' i of
  Right iyr -> iyr >= 2010 && iyr <= 2020
  Left _ -> False

validEyr :: ByteString -> Bool
validEyr e = case parseOnly decimal' e of
  Right eyr -> eyr >= 2020 && eyr <= 2030
  Left _ -> False

validHgt :: ByteString -> Bool
validHgt h = case B.readInt h of
  Just (hgt, unit) -> (unit == "cm" && hgt >= 150 && hgt <= 193)
                      || (unit == "in" && hgt >= 59 && hgt <= 76)
  Nothing -> False

validHcl :: ByteString -> Bool
validHcl h = case B.splitAt 1 h of ("#", n) -> B.length n == 6 && B.all (inClass "0-9a-f") n
                                   _ -> False

validEcl :: ByteString -> Bool
validEcl e = e `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validPid :: ByteString -> Bool
validPid p = B.length p == 9 && B.all (inClass "0-9") p

decimal' = decimal <* endOfInput
