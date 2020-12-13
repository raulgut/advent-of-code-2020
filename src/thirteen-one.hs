-----------------------------------------------------------------------------
-- | 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (sort)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let (timestamp,busLines) = ((\[myTimestamp,allBuses] -> (read myTimestamp, getNumbers allBuses)) . lines $ input) :: (Int,[Int])
  putStrLn . show . (\(time,bus) -> time * bus) . head . sort . (\x -> zip x busLines) . map (getNext timestamp) $ busLines

-- | from String of buses separated by commas to numeric elements
getNumbers :: String -> [Int]
getNumbers str = map read . filter (/="x") . splitOn "," $ str

-- | We obtain the next time the bus passes
getNext :: Int -> Int -> Int
getNext timestamp bus = 
  let previous = div timestamp bus
  in (previous * bus) + bus - timestamp