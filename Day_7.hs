{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Day_7 where

import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.ParserCombinators.ReadP
import Text.RawString.QQ

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
  sepBy1 programName (string ", ")


program :: ReadP Program
program = do
  nameCode <- programName <* char ' '
  prgramWeight <- readWeight
  childPrograms <- option [] children
  eof
  return $ Program nameCode prgramWeight childPrograms

readProgram = fst . head . readP_to_S program

-- Part 1
data Program = Program String Int [String] deriving (Show)
name (Program n _ _) = n
childNames (Program _ _ ns) = ns

insertChildren :: Program -> S.Set String -> S.Set String
insertChildren (Program _ _ childs) set = foldl (flip S.insert) set childs

findRoot :: [Program] -> S.Set String -> S.Set String -> String
findRoot [] parents children = head . S.toList $ S.difference parents children
findRoot (program:programz) parents children =
  findRoot programz (S.insert (name program) parents) (insertChildren program children)

-- Part 2
findChildren :: M.Map String Program -> [String] -> [Program]
findChildren programs children = (programs M.!) <$> children

weight :: M.Map String Program -> Program -> Int
weight _ (Program _ w []) = w
weight programs (Program _ w chl) =
  w + sum (findWeight <$> findChildren programs chl)
    where findWeight = weight programs

nameToProgram p@(Program n _ _ ) = (n, p)

data PTree = PTree (Int, Int) [PTree]
instance Show PTree where
  show (PTree (w, chw) []) = show w
  show (PTree (w, chw) chlds) = show chw ++ " (" ++ show w  ++ ") " ++ show chlds

construct :: M.Map String Program -> Program -> PTree
construct programs p@(Program _ w _) =
  PTree (w, weight programs p) (construct programs <$> childPrograms)
    where
      childPrograms = findChildren programs (childNames p)

-- main :: IO [Int]
main :: IO ()
main = do
  let testInput = [r|pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)|]
  input <- readFile "./Day_7.txt"
  let puzzleInput = readProgram <$> lines input
  let rootName = "airlri" --findRoot puzzleInput S.empty S.empty
  let codeToProgram = foldl (flip $ uncurry M.insert) M.empty (nameToProgram <$> puzzleInput)
  -- let rootProgram@(Program _ _ chl) = codeToProgram M.! rootName
  let rootProgram = codeToProgram M.! rootName
  -- weight codeToProgram <$> findChildren codeToProgram chl
  putStr $ show $ construct codeToProgram rootProgram

  -- return $
  return ()
