-----------------------------------------------------------------------------
-- | Use a State Monad
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Control.Monad.State (State (..), get, put, evalState, runState)

-- State of the Ship
data ShipState =
  ShipState {
    direction :: Int -- N=0, E=1, S=2, W=3
    , vertical :: Int
    , horizontal :: Int
  } deriving Show

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map (\(i:value) -> (i, read value)) . lines $ input) :: [(Char,Int)]
  putStrLn . show . (\x -> evalState (process x) (ShipState 1 0 0)) $ entries

--  | Move the Ship
process :: [(Char,Int)] -> State ShipState Int
process [] = do state <- get 
                return $ (abs . vertical $ state) + (abs . horizontal $ state)
process (('F',value):rest) =
  do state <- get
     case (direction state) of
       0 -> put $ state {vertical = (vertical state) + value}
       1 -> put $ state {horizontal = (horizontal state) + value}
       2 -> put $ state {vertical = (vertical state) - value}
       3 -> put $ state {horizontal = (horizontal state) - value}
     process rest
process (('N',value):rest) =
  do state <- get
     put $ state {vertical = (vertical state) + value}
     process rest
process (('E',value):rest) =
  do state <- get
     put $ state {horizontal = (horizontal state) + value}
     process rest
process (('S',value):rest) =
  do state <- get
     put $ state {vertical = (vertical state) - value}
     process rest
process (('W',value):rest) =
  do state <- get
     put $ state {horizontal = (horizontal state) - value}
     process rest
process (('R',value):rest) =
  do state <- get
     let newDirection = mod ((direction state) + (mod (div value 90) 4)) 4
     put $ state {direction = newDirection}
     process rest
process (('L',value):rest) =
  do state <- get
     let newDirection = let newValue = (direction state) - (mod (div value 90) 4)
                        in if newValue < 0 then 4 + newValue else newValue
     put $ state {direction = newDirection}
     process rest
