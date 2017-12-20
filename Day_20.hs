{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Day_20 where

import Data.Function (on)
import Data.List
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Text (Parser)

data Vec3 = Vec3 { x :: !Int, y :: !Int, z :: !Int } deriving (Eq, Ord, Show)

add (Vec3 x y z) (Vec3 u v w) = Vec3 (x + u) (y + v) (z + w)

data Particle = Particle { position :: !Vec3, velocity :: !Vec3, acceleration :: !Vec3 } deriving (Eq, Show)

manhattan :: Vec3 -> Int
manhattan Vec3{..} = abs x + abs y + abs z

minParticleIndex :: [Particle] -> Int
minParticleIndex = index . minimumBy (comparing (manhattan . position . particle)) . zip [0..]
  where
    index = fst
    particle = snd

update Particle {..} =
  let newVelocity = velocity `add` acceleration
  in Particle (position `add` newVelocity) newVelocity acceleration

removeCollidedParticles :: [Particle] -> [Particle]
removeCollidedParticles =
  concat .
  filter ((== 1) . length) .
  groupBy ((==) `on` position) .
  sortOn position .
  fmap update

int :: Parser Int
int = do
  sign <- optional $ char '-'
  num <- read <$> many digitChar
  return $ case sign of
    Just _ -> -1 * num
    _ -> num

vec3 :: Parser Vec3
vec3 = do
  [x, y, z] <- int `sepBy` char ','
  return $ Vec3 x y z

particle :: Parser Particle
particle = do
  position <- string "p=<" *> vec3 <* string ">, "
  velocity <- string "v=<" *> vec3 <* string ">, "
  acceleration <- string "a=<" *> vec3 <* string ">"
  return $ Particle position velocity acceleration

puzzleInput :: Parser [Particle]
puzzleInput = particle `sepBy` char '\n'

main = do
  (Right input) <- parse puzzleInput "Points" <$> TIO.readFile "./Day_20.txt"
  let part1 = take 500 . fmap minParticleIndex . iterate (fmap update)
  let part2 = take 50 . fmap length . iterate removeCollidedParticles
  print $ part1 input
  print $ part2 input
