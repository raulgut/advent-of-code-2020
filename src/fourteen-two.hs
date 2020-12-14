{-# LANGUAGE MultiWayIf #-}
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
import Data.Char (digitToInt, intToDigit)
import Data.Maybe (listToMaybe, fromJust)
import Numeric (readInt, showIntAtBase)
import Data.Bits ((.&.),(.|.))

-- | Instructions are masks or memory updates
data Instruction =
  Mask String 
  | Update Int Int

-- | State of Memory
data MemState =
  MemState {
    stateMask :: String
    , memory :: Map Int Int
  } deriving Show

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map extractInstructions . lines $ input) :: [Instruction]
  putStrLn . show . (\instructions -> evalState (process instructions) 
    (MemState ['X' | i <- [0..35]] (fromList [(i,0) | i <- [0..35]]))) $ entries

-- | Parse the String into Instruction
extractInstructions :: String -> Instruction
extractInstructions str =
  case (parse programParser "" str) of
    Left err -> error . show $ err 
    Right instruction -> instruction

-- | parse masks and updates
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
     let binaryPosition = showIntAtBase 2 intToDigit position ""
     let addresses = 
           [(if | m == '1' -> ['1']
                | m == '0' -> [b] 
                | m == 'X' -> ['0','1']) | (b,m) <- zip ([const '0' i | i <- [0..35 - (length binaryPosition)]] ++ binaryPosition) (stateMask state)]
     let newMemory = foldl (\myMem myPos -> insert myPos value myMem) (memory state) [fromJust . readBin $ position | position <- sequence addresses]
     put state { memory = newMemory }
     process instructions
process ((Mask mask):instructions) =
  do state <- get 
     put state { stateMask = mask }
     process instructions
              
-- | read binary number into Int (form stackoverflow)
readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt
