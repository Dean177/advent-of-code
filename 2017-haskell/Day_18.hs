{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Day_18 where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Exception (BlockedIndefinitelyOnMVar(..), bracket, handle)
import Control.Monad (when)
import Control.Monad.State (evalState, get, put)
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
import Text.Megaparsec
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

data Ins
  = Rcv Char
  | Snd (Either Char Int)
  | Set Char (Either Char Int)
  | Add Char (Either Char Int)
  | Mul Char (Either Char Int)
  | Mod Char (Either Char Int)
  | Jgz (Either Char Int) (Either Char Int)

ins :: Parser Ins
ins =
  (Rcv <$> (string "rcv" *> space *> letterChar)) <|>
  (Snd <$> (string "snd" *> space *> charOrInt)) <|>
  (Set <$> (string "set" *> space *> letterChar <* space) <*> charOrInt) <|>
  (Add <$> (string "add" *> space *> letterChar <* space) <*> charOrInt) <|>
  (Mul <$> (string "mul" *> space *> letterChar <* space) <*> charOrInt) <|>
  (Mod <$> (string "mod" *> space *> letterChar <* space) <*> charOrInt) <|>
  (Jgz <$> (string "jgz" *> space *> charOrInt <* space) <*> charOrInt)

instructions :: Parser [Ins]
instructions = ins `sepBy` char '\n'

data MachineSpec m = MachineSpec { program :: V.Vector Ins, send :: Int -> m (), recv :: m Int }

data MachineState = MachineTerminated | MachineState { instructionIndex :: Int, registers :: Map Char Int }

step :: Monad m => MachineSpec m -> MachineState -> m MachineState
step MachineSpec {..} s@MachineState {..} =
  case instruction of
    Rcv reg -> do
      val <- recv
      check s { instructionIndex = instructionIndex + 1, registers = Map.insert reg val registers }
    Snd val -> do
      send $ loadValue val
      check s { instructionIndex = instructionIndex + 1 }
    Set reg val ->
      check s {
        instructionIndex = instructionIndex + 1,
        registers = Map.insert reg (loadValue val) registers }
    Add reg val ->
      check s {
        instructionIndex = instructionIndex + 1,
        registers = Map.insert reg (loadValue (Left reg) + loadValue val) registers }
    Mul reg val ->
      check s {
        instructionIndex = instructionIndex + 1,
        registers = Map.insert reg (loadValue (Left reg) * loadValue val) registers }
    Mod reg val ->
      check s {
        instructionIndex = instructionIndex + 1,
        registers = Map.insert reg (loadValue (Left reg) `mod` loadValue val) registers }
    Jgz cond dest ->
      check s {
        instructionIndex = instructionIndex + (
          if loadValue cond > 0 then fromIntegral (loadValue dest) else 1
        )}
  where
    instruction :: Ins
    instruction = program V.! instructionIndex

    valueFromRegister :: Char -> Int
    valueFromRegister chr = fromMaybe 0 (Map.lookup chr registers)

    loadValue :: Either Char Int -> Int
    loadValue = either valueFromRegister id

    check state@MachineState {instructionIndex} =
      return $
        if instructionIndex >= V.length program || instructionIndex < 0
        then MachineTerminated else state

loop :: Monad m => MachineSpec m -> MachineState -> m ()
loop spec MachineTerminated = return ()
loop spec state = step spec state >>= loop spec

part1 :: [Ins] -> Int
part1 input =
  fromJust . getFirst $ evalState (execWriterT (loop spec initialState)) 0
  where
    initialState = MachineState { instructionIndex = 0, registers = Map.empty }
    spec = MachineSpec {
      program = V.fromList input,
      send = put,
      recv = do
        val <- get
        when (val /= 0) (tell $ First $ Just val)
        return val
      }

part2 :: [Ins] -> IO Int
part2 input = do
  let program = V.fromList input
  counter <- newIORef 0
  chan0 <- newChan
  chan1 <- newChan

  let spec0 = MachineSpec {
    program = V.fromList input,
    send = writeChan chan1,
    recv = readChan chan0
    }
  let machineLoop0 = loop spec0 MachineState { instructionIndex = 0, registers = Map.singleton 'p' 0 }

  let spec1 = spec0 {
    send = (modifyIORef' counter (+ 1) >>) . writeChan chan0,
    recv = readChan chan1
    }
  let machineLoop1 = loop spec1 MachineState { instructionIndex = 0, registers = Map.singleton 'p' 1 }

  handle
    (\BlockedIndefinitelyOnMVar -> return ())
    (bracket (forkIO machineLoop0) killThread (const machineLoop1))

  readIORef counter

main = do
  (Right ins) <- parse instructions "Instructions" <$> TIO.readFile "./Day_18.txt"
  print $ part1 ins
  print =<<T part2 ins
