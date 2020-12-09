module Days.Day08 (runDay, Input, OutputA, OutputB, runA, runB) where

import Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.Vector as V
import qualified Data.Set as S
import Relude.Extra (bimapBoth)  

runDay :: Bool -> String -> IO ()
runDay = run inputParser partA partB

runA :: ByteString -> Either String OutputA
runA input = runPart input inputParser partA

runB :: ByteString -> Either String OutputB
runB input = runPart input inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy instrParser endOfLine

instrParser :: Parser Instr
instrParser = do
  i <- Atto.take 3 <* space
  let instr = case i of "acc" -> Acc
                        "nop" -> Nop
                        "jmp" -> Jmp
                        _ -> error "bad instruction input"
  s <- Atto.take 1
  let sign = case s of "-" -> Minus
                       "+" -> Plus
                       _ -> error "bad sign input"
                    
  num <- decimal
  pure $ Instr {..}

------------ TYPES ------------
type Input = [Instr]

type OutputA = Int

type OutputB = Int

data Instr = Instr
  { instr :: Instruction,
    sign :: Sign,
    num :: Int
  } deriving (Show, Eq, Ord)

data Sign = Plus | Minus deriving (Show, Eq, Ord)

data Instruction = Acc | Nop | Jmp deriving (Show, Eq, Ord)

------------ PART A ------------
partA :: Input -> OutputA
partA input = case runInstrs (fromList input) of Left x -> x
                                                 _ -> error "we want more cycles"

------------ PART B ------------
partB :: Input -> OutputB
partB input = case rights (map (runInstrs . fromList) (buildOptions input)) of [x] -> x

runInstrs :: Vector Instr -> Either Int Int
runInstrs instrs = do
  go 0 S.empty
  where
    go i s = let instr = instrs V.!? i
                 s' = S.insert i s in
      if i == V.length instrs then Right 0 else
      if S.member i s then Left 0 else
      case instr of
                    Just (Instr Acc sign x) -> bimapBoth (\res -> (case sign of Plus -> (+); Minus -> (-)) res x) (go (i+1) s') 
                    Just (Instr Jmp sign x) -> case sign of Plus -> go (i + x) s'
                                                            Minus -> go (i - x) s'
                    Just (Instr Nop _ _) -> go (i + 1) s'
                    _ -> error "bad instruction type"

buildOptions :: [Instr] -> [[Instr]]
buildOptions = go []
    where
      go _ [] = []
      go left (Instr Nop sign n : is) = flipfoldl' (:) (Instr Jmp sign n : is) left : go (Instr Nop sign n : left) is
      go left (Instr Jmp sign n : is) = flipfoldl' (:) (Instr Nop sign n : is) left : go (Instr Jmp sign n : left) is
      go left (Instr Acc sign n : is) = go (Instr Acc sign n : left) is 
