-----------------------------------------------------------------------------
-- | We use Parsec to parse strings
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser)
import Text.Parsec.Error(ParseError (..))
import Text.Parsec.Char (anyChar, string, digit)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Prim (parse, try, (<|>))
import Data.Map (Map, fromList, elems, insert)
import Control.Monad.State (State (..), get, put, evalState, runState)
import Data.Char (digitToInt)
import Data.Maybe (listToMaybe, fromJust)
import Numeric (readInt)
import Data.Bits ((.&.),(.|.))

data Instruction =
  Mask String 
  | Update Int Int

-- State of Memory
data MemState =
  MemState {
    zeroMask :: Int
    , oneMask :: Int
    , memory :: Map Int Int
  } deriving Show

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map extractInstructions . lines $ input) :: [Instruction]
  putStrLn . show . (\instructions -> evalState (process instructions) 
    (MemState (2^36 - 1) 0 (fromList [(i,0) | i <- [0..35]]))) $ entries

-- | Parse the String into Instruction
extractInstructions :: String -> Instruction
extractInstructions str =
  case (parse programParser "" str) of
    Left err -> error . show $ err 
    Right instruction -> instruction

programParser :: Parser Instruction
programParser = maskParser <|> instructionParser

-- | Parse the mask line
maskParser :: Parser Instruction 
maskParser =
  do try (string "mask = ")
     mask <- many1 anyChar 
     return . Mask $ mask

-- | Parse the instruction line
instructionParser :: Parser Instruction
instructionParser =
 do string "mem["
    position <- many1 digit
    string "] = "
    value <- many1 digit
    return $ Update (read position) (read value)

--  | Execute Instruction
process :: [Instruction] -> State MemState Int
process [] = do state <- get
                return . foldl1 (+) . elems . memory $ state
process ((Update position value):instructions) = 
  do state <- get
     let newValue = (value .&. (zeroMask state) .|. (oneMask state))
     let newMemory = insert position newValue (memory state)
     put state { memory = newMemory }
     process instructions
process ((Mask mask):instructions) =
  do state <- get 
     let zMask = fromJust . readBin . map (\x -> if (x /='0') then '1' else '0') $ mask
     let oMask = fromJust . readBin . map (\x -> if (x /='1') then '0' else '1') $ mask
     put state { zeroMask = zMask , oneMask = oMask }
     process instructions
              
-- | read binary number into Int (form stackoverflow)
readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt
