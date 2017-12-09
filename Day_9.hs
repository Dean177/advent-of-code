{-# LANGUAGE OverloadedStrings #-}
module Day_9 where

import Data.Text hiding (filter, length)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.String

data GarbageContent = CancelledGarbage | GarbageChar Char deriving (Eq, Show)

data Syntax
  = CancelledChar Char
  | Garbage [GarbageContent]
  | CGroup [Syntax]
  deriving (Show)

notChar :: Char -> Parser Char
notChar c = satisfy (/= c)

cancel :: Parser Syntax
cancel = char '!' *> (CancelledChar <$> anyChar)

garbage :: Parser Syntax
garbage = between (char '<') (char '>')
  (Garbage <$> many (
    (char '!' *> anyChar *> pure CancelledGarbage) <|>
    (GarbageChar <$> notChar '>')))

cgroup :: Parser Syntax
cgroup = between (char '{') (char '}')
  (CGroup <$> (cancel <|> garbage <|> cgroup) `sepBy` char ',')

syntax :: Parser [Syntax]
syntax = many $ cancel <|> garbage <|> cgroup

countGroup :: Int -> Syntax -> Int
countGroup _ (CancelledChar _) = 0
countGroup _ (Garbage _) = 0
countGroup depth (CGroup groups) = depth + sum (countGroup (depth + 1) <$> groups)

isGarbageChar (GarbageChar _) = True
isGarbageChar _ = False

countGarbage (CancelledChar _) = 0
countGarbage (Garbage garb) = length $ filter isGarbageChar garb
countGarbage (CGroup groups) = sum $ countGarbage <$> groups

main :: IO ()
main = do
  puzzleInput <- readFile "./Day_9.txt"
  let (Right parseResult) = runParser syntax "'Stream' Processing" puzzleInput
  print $ sum $ countGroup 1 <$> parseResult
  print $ sum $ countGarbage <$> parseResult
  return ()
