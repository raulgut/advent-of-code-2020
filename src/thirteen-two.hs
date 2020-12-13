-----------------------------------------------------------------------------
-- | 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (sort)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let busLines = (reverse . sort . (\[myTimestamp,allBuses] -> getNumbers allBuses) . lines $ input) :: [(Int,Int)]
  putStrLn . show . findCondition 1 $ busLines

-- | from String of buses separated by commas to numeric elements
getNumbers :: String -> [(Int, Int)]
getNumbers str = map (\(x,y) -> (read $ y,x)) . filter ((/="x") . snd) . zip [0..] . splitOn "," $ str

-- | We jump from the bigger one
findCondition :: Int -> [(Int,Int)] -> Int
findCondition value ((bus,timestamp):rest) =
  let maxTimestamp = (value * bus)
      result = maxTimestamp - timestamp
  in if isValid result rest then
       result
     else 
       findCondition (value + 1) ((bus,timestamp):rest)

-- | Once we set a timestamp, we check if it is valid for all the buses
isValid :: Int -> [(Int,Int)] -> Bool
isValid _ [] = True
isValid goal ((bus,timestamp):rest) =

  if mod (goal + timestamp) bus == 0 then
    isValid goal rest
  else
    False