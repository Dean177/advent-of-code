module Day_12 where

import Data.Graph as G
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.String

integer :: Parser Int
integer = read <$> many digitChar

vertex :: Parser (Int, Int, [Int])
vertex = do
  nodeKey <- integer
  string " <-> "
  connectedKeys <- integer `sepBy` string ", "
  return (nodeKey, nodeKey, connectedKeys)

vertexes = vertex `sepBy` char '\n'

main = do
  puzzleInput <- readFile "./Day_12.txt"
  let (Right parseResult) = runParser vertexes "Graph size" puzzleInput
  let (graph, _, keyToMaybeVertex) = graphFromEdges parseResult
  let (Just zeroVertex) = keyToMaybeVertex 0
  -- Part 1
  print $ length $ reachable graph zeroVertex
  -- Part 2
  print $ length $ components graph
  return ()
