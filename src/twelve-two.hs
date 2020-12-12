{-# LANGUAGE MultiWayIf #-}
-----------------------------------------------------------------------------
-- | Use a State Monad
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Control.Monad.State (State (..), get, put, evalState, runState)

-- State of Amplifiers
data ShipState =
  ShipState {
    wpVertical :: Int  
    , wpHorizontal :: Int
    , vertical :: Int
    , horizontal :: Int
  } deriving Show

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map (\(i:value) -> (i, read value)) . lines $ input) :: [(Char,Int)]
  putStrLn . show . (\x -> runState (process x) (ShipState 1 10 0 0)) $ entries

--  | Move the Ship
process :: [(Char,Int)] -> State ShipState Int
process [] = do state <- get 
                return $ (abs . vertical $ state) + (abs . horizontal $ state)
process (('F',value):rest) =
  do state <- get
     put $ state { vertical = (vertical state) + ((wpVertical state) * value)
                 , horizontal = (horizontal state) + ((wpHorizontal state) * value)}
     process rest
process (('N',value):rest) =
  do state <- get
     put $ state {wpVertical = (wpVertical state) + value}
     process rest
process (('E',value):rest) =
  do state <- get
     put $ state {wpHorizontal = (wpHorizontal state) + value}
     process rest
process (('S',value):rest) =
  do state <- get
     put $ state {wpVertical = (wpVertical state) - value}
     process rest
process (('W',value):rest) =
  do state <- get
     put $ state {wpHorizontal = (wpHorizontal state) - value}
     process rest
process (('R',value):rest) = -- N=0, E=1, S=2, W=3
  do state <- get
     let actualWPVertical = wpVertical state
         actualWPHorizontal = wpHorizontal state
         vDirection = if (actualWPVertical >= 0) then 0 else 2
         hDirection = if (actualWPHorizontal >= 0) then 1 else 3
         newVDirection = mod (vDirection + (mod (div value 90) 4)) 4 
         newHDirection = mod (hDirection + (mod (div value 90) 4)) 4 
         newWPVertical = if | newVDirection == 0 -> if | vDirection == 0 -> actualWPVertical 
                                                       | vDirection == 2 -> (actualWPVertical * (-1))
                            | newVDirection == 2 -> if | vDirection == 0 -> (actualWPVertical * (-1)) 
                                                       | vDirection == 2 -> actualWPVertical
                            | newHDirection == 0 -> if | hDirection == 1 -> actualWPHorizontal 
                                                       | hDirection == 3 -> (actualWPHorizontal * (-1))
                            | newHDirection == 2 -> if | hDirection == 1 -> (actualWPHorizontal * (-1)) 
                                                       | hDirection == 3 -> actualWPHorizontal
         newWPHorizontal = if | newVDirection == 1 -> if | vDirection == 0 -> actualWPVertical 
                                                         | vDirection == 2 -> (actualWPVertical * (-1))
                              | newVDirection == 3 -> if | vDirection == 0 -> (actualWPVertical * (-1)) 
                                                         | vDirection == 2 -> actualWPVertical
                              | newHDirection == 1 -> if | hDirection == 1 -> actualWPHorizontal 
                                                         | hDirection == 3 -> (actualWPHorizontal * (-1))
                              | newHDirection == 3 -> if | hDirection == 1 -> (actualWPHorizontal * (-1)) 
                                                         | hDirection == 3 -> actualWPHorizontal
     put $ state {wpVertical = newWPVertical, wpHorizontal = newWPHorizontal}
     process rest
process (('L',value):rest) = -- N=0, E=1, S=2, W=3
  do state <- get
     let actualWPVertical = wpVertical state
         actualWPHorizontal = wpHorizontal state
         vDirection = if (actualWPVertical >= 0) then 0 else 2
         hDirection = if (actualWPHorizontal >= 0) then 1 else 3
         newVDirection = let newValue = vDirection - (mod (div value 90) 4)
                         in if newValue < 0 then 4 + newValue else newValue
         newHDirection = let newValue = hDirection - (mod (div value 90) 4)
                         in if newValue < 0 then 4 + newValue else newValue
         newWPVertical = if | newVDirection == 0 -> if | vDirection == 0 -> actualWPVertical 
                                                       | vDirection == 2 -> (actualWPVertical * (-1))
                            | newVDirection == 2 -> if | vDirection == 0 -> (actualWPVertical * (-1)) 
                                                       | vDirection == 2 -> actualWPVertical
                            | newHDirection == 0 -> if | hDirection == 1 -> actualWPHorizontal 
                                                       | hDirection == 3 -> (actualWPHorizontal * (-1))
                            | newHDirection == 2 -> if | hDirection == 1 -> (actualWPHorizontal * (-1)) 
                                                       | hDirection == 3 -> actualWPHorizontal
         newWPHorizontal = if | newVDirection == 1 -> if | vDirection == 0 -> actualWPVertical 
                                                         | vDirection == 2 -> (actualWPVertical * (-1))
                              | newVDirection == 3 -> if | vDirection == 0 -> (actualWPVertical * (-1)) 
                                                         | vDirection == 2 -> actualWPVertical
                              | newHDirection == 1 -> if | hDirection == 1 -> actualWPHorizontal 
                                                         | hDirection == 3 -> (actualWPHorizontal * (-1))
                              | newHDirection == 3 -> if | hDirection == 1 -> (actualWPHorizontal * (-1)) 
                                                         | hDirection == 3 -> actualWPHorizontal
     put $ state {wpVertical = newWPVertical, wpHorizontal = newWPHorizontal}
     process rest
