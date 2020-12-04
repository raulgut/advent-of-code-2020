-----------------------------------------------------------------------------
-- | We avoided list comprehensions in order to optimize search
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List (find)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map read . lines $ input) :: [Int]
  putStrLn . show . (\(e1,e2) -> e1 * e2) . process $ entries

-- | Pick one, process with respect to the rest of the list. If not found,
-- discard it and proceed with the next one
process :: [Int] -> (Int,Int)
process (e:es) 
  = case find (\(x,addition) -> addition == 2020) . map (\x -> (x, e + x)) $ es of 
      Just (x,_) -> (x,e)
      Nothing -> process es
process [] = error "There are not two numbers that adds 2020"
