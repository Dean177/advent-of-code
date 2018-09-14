module Day_24 where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Text (Parser)

int :: Parser Int
int = read <$> many digitChar
parseComponents = Set.fromList <$> (Component <$> (int <* char '/') <*> int) `sepEndBy` char '\n'

data Component = Component Int Int deriving (Eq, Ord, Show)
type Bridge = [Component]

strength (Component start end) = start + end
connects port (Component y z) = port == y || port == z
orientMatch port (Component y z)
  | port == y = Component y z
  | otherwise = Component z y
matchingComponents port = Set.filter (connects port)

construct :: Bridge -> Set Component -> [Bridge]
construct [] components = construct [Component 0 0] components
construct bridge@(Component _ port :_) components
  | Set.null connectingComponents = [bridge]
  | otherwise = concatMap go connectingComponents
  where
    connectingComponents = matchingComponents port components
    go match = construct (orientMatch port match : bridge) (Set.delete match components)

strongest :: [Bridge] -> Int
strongest = maximum . fmap (sum . fmap strength)

longest :: [Bridge] -> [Bridge]
longest bridges = filter ((== len) . length) bridges
  where len = maximum . fmap length $ bridges

main = do
  (Right components) <- parse parseComponents "Bridges" <$> TIO.readFile "./Day_24.txt"
  let bridges = construct [] components
  print $ strongest bridges
  print $ strongest . longest $ bridges
