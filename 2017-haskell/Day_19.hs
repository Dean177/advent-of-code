{-# LANGUAGE OverloadedStrings #-}
module Day_19 where

import Data.List
import Data.Maybe
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Text (Parser)

type Position = (Int, Int)
data Direction = N | E | S | W deriving (Eq, Show)
data Piece = V | H | Space | Plus | Letter Char deriving (Eq, Show)
type Pipes = Vector (Vector Piece)

piece :: Parser Piece
piece =
  V <$ char '|' <|>
  H <$ char '-' <|>
  Space <$ char ' ' <|>
  Plus <$ char '+' <|>
  Letter <$> letterChar

parsePipes :: Parser Pipes
parsePipes = V.fromList . fmap V.fromList <$> many piece `sepEndBy` char '\n'

availableTurns N = [E, W]
availableTurns E = [N, S]
availableTurns S = [E, W]
availableTurns W = [N, S]

step (x, y) N = (x, y - 1)
step (x, y) E = (x + 1, y)
step (x, y) S = (x, y + 1)
step (x, y) W = (x - 1, y)

letterToMaybe (Letter c) = Just c
letterToMaybe _ = Nothing

get :: Pipes -> Position -> Piece
get pipes (x,y) = pipes V.! y V.! x

isPositionInBounds (x, y) bounds =
  (0 <= x) && (0 <= y) && (x < V.length (bounds V.! 0)) && (y < V.length bounds)

valid vec position = isPositionInBounds position vec && get vec position /= Space

changeDirection :: Pipes -> Position -> Direction -> [Piece]
changeDirection pipes position direction =
  let newDirection = head $ filter (valid pipes . step position) (availableTurns direction)
  in get pipes position : walk pipes (step position newDirection) newDirection

walk :: Pipes -> Position -> Direction -> [Piece]
walk pipes position direction = case get pipes position of
  Space -> []
  Plus -> changeDirection pipes position direction
  x -> x : walk pipes (step position direction) direction

main :: IO ()
main = do
  input <- TIO.readFile "./Day_19.txt"
  let (Right pipes) = parse parsePipes "Pipes" input
  let (Just startX) = V.findIndex (/= Space) (pipes V.! 0)
  let path = walk pipes (startX, 0) S
  print $ length path
  print $ mapMaybe letterToMaybe path
