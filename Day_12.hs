{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Day_12 where

import Data.Graph as G
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.String
import Text.RawString.QQ

testInput = [r|0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5|]

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
  print $ length $ reachable graph zeroVertex
  -- Part 2
  print $ length $ components graph
  return ()
