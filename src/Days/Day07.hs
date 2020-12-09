module Days.Day07 (runDay, Input, OutputA, OutputB, runA, runB) where

import Data.Graph.DGraph
import Data.Graph.Traversal
import Data.Graph.Types
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8

runDay :: Bool -> String -> IO ()
runDay = run inputParser partA partB

runA :: ByteString -> Either String OutputA
runA input = runPart input inputParser partA

runB :: ByteString -> Either String OutputB
runB input = runPart input inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = concat <$> sepBy edgeParser endOfLine

edgeParser :: Parser [Arc ByteString Int]
edgeParser = do
  container <- bag
  void "bags contain "
  containees <- ([] <$ "no other bags") <|> sepBy containeeParser ", "
  void (char '.')
  pure $ map (\(n, containee) -> Arc container containee n) containees
  
word :: Parser ByteString
word = takeWhile1 isAlpha_ascii

bag :: Parser ByteString
bag = do
  adj <- word <* space
  color <- word <* space
  pure (adj <> " " <> color)

containeeParser :: Parser (Int, ByteString)
containeeParser = do
  n <- decimal <* space
  color <- bag
  void "bag"
  peekChar' >>= \case 's' -> void "s"; _ -> pass
  pure (n, color)
  
------------ TYPES ------------
type Input = [Arc ByteString Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA edges =
  let graph = fromArcsList (map (\(Arc v v' e) -> Arc v' v e) edges)
   in length (dfsVertices graph "shiny gold") - 1

------------ PART B ------------
partB :: Input -> OutputB
partB edges = go 1 "shiny gold" - 1
    where
      graph = if isSimple (fromArcsList edges) then fromArcsList edges else error "Not a simple graph"
      go n x = n + n * sum (map (\(_, v', n') -> go n' v') (reachableAdjacentVertices' graph x))
