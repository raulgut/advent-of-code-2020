-----------------------------------------------------------------------------
-- | 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Char (digitToInt)
import Data.List (sort)

main = do
  args <- getArgs
  let game = play 100 . map digitToInt . head $ args
  let (left,(d:right)) = span (/=1) game
  putStrLn . concatMap show $ right ++ left

-- | play the game and return the last configuration
play :: Int -> [Int] -> [Int]
play step state@(current:next1:next2:next3:rest) = 
  let previous = findDestination current rest
      (left,(d:right)) = span (/=previous) rest
  in if step == 0 then (current:next1:next2:next3:rest) else play (step - 1) (left ++ (d:next1:next2:next3:right) ++ [current])

-- | find the previous value in the list
findDestination :: Int -> [Int] -> Int
findDestination current list = 
  let max = maximum list
      min = minimum list
      next = if (current - 1) < min then max else (current - 1)
  in last . takeWhile (<= next) . sort $ list
       