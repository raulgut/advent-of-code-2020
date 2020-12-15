-----------------------------------------------------------------------------
-- | 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.Map (Map, empty, insert, lookup)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map read . splitOn "," $ input) :: [Int]
  putStrLn . show . play empty 1 30000000 $ entries

-- | We play the game
play :: Map Int Int -> Int -> Int -> [Int] -> Int
play visited turn goal [entry] 
  | turn == goal = entry
play visited turn goal [entry] =
  case Data.Map.lookup entry visited of
    Nothing -> play (insert entry turn visited) (turn + 1) goal [0]
    (Just oldTurn) -> play (insert entry turn visited) (turn + 1) goal [turn - oldTurn]
play visited turn goal (entry:entries) =
  play (insert entry turn visited) (turn + 1) goal entries
