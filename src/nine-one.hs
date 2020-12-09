-----------------------------------------------------------------------------
-- | Constraint solver by brute force
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Set (Set, fromList, member)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map read . lines $ input) :: [Int]
  let preamble = take 25 entries
  let combinations = fromList [x + y | x <- preamble, y <- preamble, x /= y]
  putStrLn . show . process combinations preamble . drop 25 $ entries

-- | Check validity and process combinations
process :: Set Int -> [Int] -> [Int] -> Int
process combinations (n1:preamble) (n2:rest)
  = if member n2 combinations then
      process newCombinations newPreamble rest
    else
      n2
  where 
    newPreamble = preamble ++ [n2]
    newCombinations = fromList [x + y | x <- newPreamble, y <- newPreamble, x /= y]
process _ _ [] = error "The input is valid."

