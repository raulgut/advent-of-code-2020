-----------------------------------------------------------------------------
-- | We use Set to avoid duplicates
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Set (Set, empty, fromList, intersection, size)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (foldl extract [(fromList ['a'..'z'])] . lines $ input) :: [Set Char]
  putStrLn . show . foldl1 (+) . map size $ entries

-- | Extract the group answers into Set using intersection
extract :: [Set Char] -> String -> [Set Char]
extract (passenger:passengers) str
  = if null str then
      ((fromList ['a'..'z']):passenger:passengers)
    else
      (intersection (fromList str) passenger):passengers
