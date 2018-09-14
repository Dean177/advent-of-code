{-# LANGUAGE OverloadedStrings #-}
module Day_16 where

import qualified Data.Map as Map
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V
import Control.Monad.ST
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.String
import Util (rotateR)

data DanceMove =
  Spin Int |
  Exchange Int Int |
  Partner Char Char deriving (Show)

spin = char 's' *> (Spin . read <$> many digitChar)
exchange = char 'x' *> (Exchange <$> (read <$> many digitChar) <*> (char '/' *> (read <$> many digitChar)))
partner = char 'p' *> (Partner <$> anyChar <*> (char '/' *> anyChar))

danceMoves :: Parser [DanceMove]
danceMoves = (spin <|> exchange <|> partner) `sepBy` char ','

danceStep :: V.MVector s Char -> DanceMove -> ST s (V.MVector s Char)
danceStep programes (Spin n) = rotateR n programes
danceStep programes (Exchange i j) = MV.swap programes i j >> return programes
danceStep programes (Partner x y) = do
  fprogrammes <- V.freeze programes
  let (Just i) = V.elemIndex x fprogrammes
  let (Just j) = V.elemIndex y fprogrammes
  MV.write programes i y
  MV.write programes j x
  return programes

runMoves moves positions = foldM danceStep positions moves

dance startPosition moves = runST $ do
  positions <- V.thaw startPosition
  finalPositions <- runMoves moves positions
  V.freeze finalPositions

findCycle startPosition currentPosition moves moveCycle
  | startPosition == currentPosition = moveCycle
  | otherwise = findCycle startPosition nextPosition moves (nextPosition:moveCycle)
    where nextPosition = dance currentPosition moves

main = do
  puzzleInput <- readFile "./Day_16.txt"
  let (Right moves) = runParser danceMoves "Programe dance" puzzleInput
  let startPosition = V.fromList ['a'..'p']
  let nextPosition = dance startPosition moves

  -- Part 2
  let positionCycle = reverse $ drop 1 $ findCycle startPosition nextPosition moves (nextPosition:[startPosition])
  let dancePosition = 1000000000 `mod` length positionCycle

  print positionCycle
  print dancePosition
  print $ positionCycle !! dancePosition
  return ()
