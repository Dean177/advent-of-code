module Day_25 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

data Binary = Zero | One deriving (Eq)

type Position = Int

data MachineSpec = A | B | C | D | E | F

type MachineState = (Map Position Binary, Position, MachineSpec)

currentValue values position = fromMaybe Zero (Map.lookup position values)

data MoveDirection = L | R

move L currentPosition = currentPosition - 1
move R currentPosition = currentPosition + 1

work :: MachineState -> MachineState
work (values, position, state) = case state of
  A -> case currentValue values position of
    Zero -> (
      Map.insert position One values,
      move R position,
      B)
    One -> (
      Map.insert position One values,
      move L position,
      E)

  B -> case currentValue values position of
    Zero -> (
      Map.insert position One values,
      move R position,
      C)
    One -> (
      Map.insert position One values,
      move R position,
      F)

  C -> case currentValue values position of
    Zero -> (
      Map.insert position One values,
      move L position,
      D)
    One -> (
      Map.insert position Zero values,
      move R position,
      B)

  D -> case currentValue values position of
    Zero -> (
      Map.insert position One values,
      move R position,
      E)
    One -> (
      Map.insert position Zero values,
      move L position,
      C)

  E -> case currentValue values position of
    Zero -> (
      Map.insert position One values,
      move L position,
      A)
    One -> (
      Map.insert position Zero values,
      move R position,
      D)

  F -> case currentValue values position of
    Zero -> (
      Map.insert position One values,
      move R position,
      A)
    One -> (
      Map.insert position One values,
      move R position,
      C)

main = do
  let (values, _, _) = iterate work (Map.empty, 0, A) !! 12523873
  print $ length $ filter ((==) One . snd) (Map.toList values)
