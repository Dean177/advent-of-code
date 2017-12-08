module Day_8 where 
  
import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as M
import Text.ParserCombinators.ReadP 

type RegisterName  = String
type Registers = M.Map RegisterName Int
type Operation = Int -> Int -> Bool
data Condition = Condition RegisterName Operation Int
instance Show Condition  where
  show (Condition nm _ i) = "Condition " ++ nm ++ ", [op], " ++ show i

data Instruction = Instruction RegisterName Int Condition
instance Show Instruction where
  show (Instruction rn val cond) = "Instruction " ++ rn ++ ", " ++ show val ++ ", " ++ show cond

initialRegisters :: Registers
initialRegisters = M.empty

registerValue :: Registers -> RegisterName -> Int
registerValue registers register = Maybe.fromMaybe 0 $ M.lookup register registers 

isSatisfied :: Registers -> Condition -> Bool
isSatisfied registers (Condition registerName operation value) = (registerValue registers registerName) `operation` value

foldStep :: Registers -> Instruction -> Registers
foldStep registers (Instruction registerName modifyBy condition) = 
  if (isSatisfied registers condition) 
  then M.insert registerName (currentValue + modifyBy) registers
  else registers
  where currentValue = registerValue registers registerName
  
solve puzzleInput = foldl foldStep initialRegisters puzzleInput

name :: ReadP RegisterName
name = many1 $ satisfy isAlpha

int :: ReadP Int
int = choice [
  read <$> (many1 $ satisfy isDigit), 
  char '-' *> ((negate . read) <$> (many1 $ satisfy isDigit))
  ]

moveBy :: ReadP Int
moveBy = do
  modifier <- choice [string "inc" *> return id, string "dec" *> return negate]
  char ' '
  val <- int
  return $ modifier val
  
operation :: ReadP Operation
operation = choice [
  string "<" *> return (<),
  string "<=" *> return (<=),
  string "==" *> return (==),
  string ">=" *> return (>=),
  string ">" *> return (>),
  string "!=" *> return (/=)
  ]

condition :: ReadP Condition
condition = do
 string "if "
 regName <- name <* char ' '
 op <- operation <* char ' '
 val <- int
 return $ Condition regName op val

instruction :: ReadP Instruction
instruction = do
  regName <- name <* char ' '
  modBy <- moveBy <* char ' '
  cond <- condition
  eof
  return $ Instruction regName modBy cond
 
parseInstruction = fst . head . readP_to_S instruction
parse input =  parseInstruction <$> (lines input)


-- Part 2
--maxRegister :: Registers -> Int
maxRegister register =  register
  --)

foldStep2 :: [Registers] -> Instruction -> [Registers]
foldStep2 registerss instruc = (foldStep (head registerss) instruc): registerss

solve2 puzzleInput = maximum . 
  fmap (maximum . fmap snd) $
  filter (not . null) $ -- There are no registers for the initial 'registers'
  M.toList <$> 
  (foldl foldStep2 [initialRegisters] puzzleInput)
  
