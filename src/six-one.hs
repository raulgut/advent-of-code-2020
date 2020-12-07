-----------------------------------------------------------------------------
-- | We use Set to avoid duplicates
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Set (Set, empty, fromList, union, size)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (foldl extract [empty] . lines $ input) :: [Set Char]
  putStrLn . show . foldl1 (+) . map size $ entries

-- | Extract the group answers into Set using union
extract :: [Set Char] -> String -> [Set Char]
extract (passenger:passengers) str
  = if null str then
      (empty:passenger:passengers)
    else
      (union (fromList str) passenger):passengers
