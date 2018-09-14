{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Day_22 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Text (Parser)

data Direction = N | E | S | W deriving (Eq, Show)
data Rotation = L | R deriving (Eq, Show)
data Node = Clean | Infected deriving (Eq)
instance Show Node where
  show Clean = "."
  show Infected = "#"

type Coordinate = (Int, Int)
type Grid = Map Coordinate Node
data Virus = Virus { coordinate :: Coordinate, direction :: Direction } deriving (Eq, Show)

node :: Parser Node
node = (const Clean <$> char '.') <|> (const Infected <$> char '#')

grid :: Parser [[Node]]
grid = many node `sepBy` char '\n'

decideTurn :: Node -> Rotation
decideTurn Clean = L
decideTurn Infected = R

turn :: Direction -> Rotation -> Direction
turn N L = W
turn N R = E
turn E L = N
turn E R = S
turn S L = E
turn S R = W
turn W L = S
turn W R = N

move :: Virus -> Virus
move (Virus (x, y) N) = Virus (x, y - 1) N
move (Virus (x, y) E) = Virus (x + 1, y) E
move (Virus (x, y) S) = Virus (x, y + 1) S
move (Virus (x, y) W) = Virus (x - 1, y) W

updateNode Infected = Clean
updateNode Clean = Infected

work :: (Grid, Virus, Int) -> (Grid, Virus, Int)
work (grid, virus@Virus{..}, infectionCount) = (updatedGrid, nextVirus, newInfectionCount)
  where
    currentNode = fromMaybe Clean (Map.lookup coordinate grid)
    updatedNode = updateNode currentNode
    nextVirus = move virus{ direction = turn direction (decideTurn currentNode) }
    updatedGrid = Map.insert coordinate updatedNode grid
    newInfectionCount = case updatedNode of
      Infected -> infectionCount + 1
      _ -> infectionCount

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [0..]

toPairs :: (Int, [a]) -> [((Int, Int), a)]
toPairs (y, zs) = (\(x, z) -> ((x, y), z)) <$> zipWithIndex zs

nodesToGrid :: [[Node]] -> Grid
nodesToGrid nodes = Map.fromList $ zipWithIndex nodes >>= toPairs

main = do
  (Right nodes) <- parse grid "Grid" <$> TIO.readFile "./Day_22.txt"
  -- let (Right nodes) = parse grid "Grid" "..#\n#..\n..."
  let virusStartCoordinates = (length (head nodes) `div` 2, length nodes `div` 2)
  let virus = Virus { coordinate = virusStartCoordinates, direction = N }
  print $ iterate work (nodesToGrid nodes, virus, 0) !! 10000
  return ()
