{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Day_23 where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Exception (BlockedIndefinitelyOnMVar(..), bracket, handle)
import Control.Monad (when)
import Control.Monad.State (runState, get, put, State)
import Control.Monad.Writer (execWriterT, tell)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map.Lazy as Map (empty, insert, lookup, singleton)
import Data.Map.Lazy (Map)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid (First(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Text (Parser)

int :: Parser Int
int = do
  sign <- optional $ char '-'
  num <- read <$> many digitChar
  return $ case sign of
    Just _ -> -1 * num
    _ -> num

charOrInt :: Parser (Either Char Int)
charOrInt = Left <$> letterChar <|> Right <$> int

data Ins =
  Set Char (Either Char Int) |
  Sub Char (Either Char Int) |
  Mul Char (Either Char Int) |
  Jnz (Either Char Int) (Either Char Int) deriving (Show)

ins :: Parser Ins
ins =
  (Set <$> (string "set" *> space *> letterChar <* space) <*> charOrInt) <|>
  (Sub <$> (string "sub" *> space *> letterChar <* space) <*> charOrInt) <|>
  (Mul <$> (string "mul" *> space *> letterChar <* space) <*> charOrInt) <|>
  (Jnz <$> (string "jnz" *> space *> charOrInt <* space) <*> charOrInt)

data MachineSpec m = MachineSpec { program :: V.Vector Ins, recordMul :: m () }

data MachineState =
  MachineTerminated |
  MachineState { instructionIndex :: Int, registers :: Map Char Int }

step :: Monad m => MachineSpec m -> MachineState -> m MachineState
step MachineSpec {..} MachineState {..} =  case program V.! instructionIndex of
  Set reg val -> check MachineState {
    instructionIndex = instructionIndex + 1,
    registers = Map.insert reg (loadValue val) registers }

  Sub reg val -> check MachineState {
    instructionIndex = instructionIndex + 1,
    registers = Map.insert reg (loadValue (Left reg) - loadValue val) registers }

  Mul reg val -> recordMul >> check MachineState {
    instructionIndex = instructionIndex + 1,
    registers = Map.insert reg (loadValue (Left reg) * loadValue val) registers }

  Jnz cond dest -> check MachineState {
    instructionIndex = instructionIndex + (if loadValue cond /= 0 then loadValue dest else 1),
    registers }
  where
    valueFromRegister :: Char -> Int
    valueFromRegister chr = fromMaybe 0 (Map.lookup chr registers)

    loadValue :: Either Char Int -> Int
    loadValue = either valueFromRegister id

    check state@MachineState {instructionIndex = nextInstructionIndex} =
      return $
        if nextInstructionIndex >= V.length program || nextInstructionIndex < 0
        then MachineTerminated else state

loop :: Monad m => MachineSpec m -> MachineState -> m ()
loop spec MachineTerminated = return ()
loop spec state = step spec state >>= loop spec


part1 :: [Ins] -> ((), Int)
part1 input =
  runState (loop spec initialState) 0
  where
    initialState = MachineState { instructionIndex = 0, registers = Map.empty }
    spec :: MachineSpec (State Int)
    spec = MachineSpec {
      program = V.fromList input,
      recordMul = do
        currValue <- get
        put $ currValue + 1
      }

isPrime :: Int -> Bool
isPrime k = null [x | x <- [2..(k `div` 2)], k `mod` x  == 0]

{- Psuedo code to count non-primes in steps of 17
b = 107900
c = 124900

loop $ do
  f = 1
  d = 2

  while (d != b) $ do
    e = 2
    while (e != b) $ do
      cond (d * e != b) $ do
        f = 0
      e = e + 1

    d = d + 1

    cond (f == 0) $ do
      h = h + 1

    if b == c
    then exit
    else b = b + 17
-}
part2 =
  print $ length $ filter (not . isPrime) [107900, 107900 + 17..124900]

main = do
  parseResult <- parse (ins `sepBy` char '\n') "Instructions" <$> TIO.readFile "./Day_23.txt"
  case parseResult of
    (Right ins) -> print $ part1 ins
    (Left err) -> print err
