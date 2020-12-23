-----------------------------------------------------------------------------
-- | Not very proud of this solution. Too slow.
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Char (digitToInt)
import Data.List (sort)
import Data.Map (Map, fromList, insert, lookup)

main = do
  args <- getArgs
  let (first:initial) = map digitToInt . head $ args
  let sorted = sort (first:initial)
  let max = last sorted
  let links = zip ([1000000] ++ (first:initial) ++ [(max + 1)..(1000000 - 1)]) (initial ++ [(max + 1)..1000000] ++ [first])
  let state = fromList $ zip ((first:initial) ++ [(max + 1)..1000000]) links
  let game = play first 10000000 state
  let Just (prev1,next1) = Data.Map.lookup 1 game
  let Just (prev2,next2) = Data.Map.lookup next1 game
  putStrLn . show $ next1 * next2

-- | play the game and return the last configuration
play :: Int -> Int -> Map Int (Int,Int) -> Map Int (Int,Int)
play current step state = 
  let Just (prev1,next1) = Data.Map.lookup current state
      Just (prev2,next2) = Data.Map.lookup next1 state
      Just (prev3,next3) = Data.Map.lookup next2 state
      Just (prev4,next4) = Data.Map.lookup next3 state
      Just (prev5,next5) = Data.Map.lookup next4 state
      delState = insert next4 (current,next5) . insert current (prev1,next4) $ state -- delete 3 next elements
      previous = findDestination current (next1:next2:next3:[])
      Just (prev6,next6) = Data.Map.lookup previous delState
      Just (prev7,next7) = Data.Map.lookup next6 delState
      newState = insert next3 (prev4,next6) $ insert next1 (previous,next2) $ insert next6 (next3,next7) $ insert previous (prev6,next1) delState
  in if step == 0 then state else play next4 (step - 1) newState

-- | find the previous value avoiding deleted
findDestination :: Int -> [Int] -> Int
findDestination current deleted = 
  let min = 1 
      maxList = 1000000
      previous = if (current - 1) < min then maxList else (current - 1)
  in if elem previous deleted then 
       findDestination previous deleted 
     else 
       previous
       