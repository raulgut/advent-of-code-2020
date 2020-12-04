-----------------------------------------------------------------------------
-- | We avoided list comprehensions in order to optimize search
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List (find)
import Data.Maybe (isJust)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map read . lines $ input) :: [Int]
  putStrLn . show . (\(e1,e2,e3) -> e1 * e2 * e3) . process $ entries

-- | Pick one, process with respect to the rest of the list. If not found,
-- discard it and proceed with the next one. Add value as argument
processValue :: Int -> [Int] -> Maybe (Int,Int)
processValue value (e:es) 
  = case find (\(x,y,addition) -> addition == value) . map (\x -> (x, e, e + x)) $ es of 
      Just (e1,e2,_) -> Just (e1,e2)
      Nothing -> processValue value es
processValue _ [] = Nothing

-- | Pick one, process with respect to the rest of the list. If not found,
-- discard it and proceed with the next one.
process :: [Int] -> (Int,Int,Int)
process (e:es)
  = case processValue (2020 - e) $ es of 
      Just (x,y) -> (x,y,e)
      Nothing -> process es
process [] = error "There are not three numbers that adds 2020"
