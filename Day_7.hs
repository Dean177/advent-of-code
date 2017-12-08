{-# LANGUAGE OverloadedStrings #-}
module Day_7 where

import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.ParserCombinators.ReadP 

childlessExample = "ktlj (57)"
example = "fwft (72) -> ktlj, cntj, xhth"

programName :: ReadP String
programName = many1 $ satisfy isAlpha

int :: ReadP Int
int = do
  digits <- many1 $ satisfy isDigit
  return $ read digits

readWeight :: ReadP Int
readWeight = between (char '(') (char ')') int
 
children :: ReadP [String]
children = do 
  string " -> "
  elems <- sepBy1 programName (string ", ")
  return elems


program :: ReadP Program
program = do
  nameCode <- programName <* char ' '
  prgramWeight <- readWeight
  childPrograms <- option [] children
  eof
  return $ Program nameCode prgramWeight childPrograms
 
readProgram = fst . head . readP_to_S program
  
data Program = Program String Int [String] deriving (Show)
name (Program n _ _) = n



insertChildren :: Program -> S.Set String -> S.Set String
insertChildren (Program _ _ childs) set = foldl (flip S.insert) set childs

findRoot :: [Program] -> S.Set String -> S.Set String -> String
findRoot [] parents children = head . S.toList $ S.difference parents children
findRoot (program:programz) parents children = 
  findRoot programz (S.insert (name program) parents) (insertChildren program children)

-- Part 2 
root = "airlri"

findChildren :: M.Map String Program -> [String] -> [Program]
findChildren programs children = (programs M.!) <$> children

weight :: M.Map String Program -> Program -> Int
weight _ (Program _ w []) = w
weight programs (Program _ w chl) = 
  w + (sum (findWeight <$> (findChildren programs chl)))
    where findWeight = (weight programs)

nameToProgram p@(Program n _ _ ) = (n, p)



