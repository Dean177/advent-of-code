{-# LANGUAGE OverloadedStrings #-}
module Day_9 where

import Data.Text hiding (filter, length)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.String

newtype Group = Group [Group] deriving (Show)

data GarbageContent
  = CancelledGarbage
  | GarbageChar Char
  deriving (Eq, Show)

data Syntax
  = CancelledChar Char
  | Garbage [GarbageContent]
  | CGroup [Syntax]
  deriving (Show)

notChar :: Char -> Parser Char
notChar c = satisfy (/= c)

skip1 :: Parser ()
skip1 = anyChar *> pure ()

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

countGroups :: Int -> [Syntax] -> Int
countGroups depth groups = sum (countGroup depth <$> groups)

isGarbageChar :: GarbageContent -> Bool
isGarbageChar (GarbageChar _) = True
isGarbageChar _ = False

countGarbage (CancelledChar _) = 0
countGarbage (Garbage garb) = length $ filter isGarbageChar garb
countGarbage (CGroup groups) = sum $ countGarbage <$> groups

countGarbages syntax = sum $ countGarbage <$> syntax

main :: IO ()
main = do
  -- print $ runParser cancel "!" "!!"
  print $ runParser garbage "<>" "<asd!>>"
  -- print $ runParser cgroup "{}" "{{<>!!}}"
  puzzleInput <- readFile "./Day_9.txt"
  let parseResult = runParser syntax "{}" puzzleInput
  -- print $ countGroups 1 <$> parseResult
  print $ countGarbages <$> parseResult
  -- Part 2
     -- "{{<!>},{<!>},{<!>},{<a>}}"
  return ()
