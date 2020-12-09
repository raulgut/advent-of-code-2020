{-# LANGUAGE MultiWayIf #-}
-----------------------------------------------------------------------------
-- | Find the subset, find the window
-----------------------------------------------------------------------------
import System.Environment (getArgs)
import Data.Set (Set, fromList, member)
import Data.List (minimum, maximum)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map read . lines $ input) :: [Int]
  let preamble = take 25 entries
  let combinations = fromList [x + y | x <- preamble, y <- preamble, x /= y]
  putStrLn . show . (\subset -> minimum subset + maximum subset) . findSubset [] entries . process combinations preamble . drop 25 $ entries

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

-- | We start by an empty list, we add elements until we reach number or we
-- surpass number. If we surpass, we remove elements from the left until we need
-- to add more elements.
findSubset :: [Int] -> [Int] -> Int -> [Int]
findSubset [] (e2:entries) number =
  if number > 0 then findSubset [e2] entries number
  else error $ "number must be greater than 0."
findSubset _ [] number =
  error $ (show number) ++ " not computable using entries."
findSubset (e1:subset) (e2:entries) number =
  let result = foldl (+) 0 (e1:subset)
  in if | result == number -> e1:subset
        | result > number -> findSubset subset (e2:entries) number
        | result < number -> findSubset ((e1:subset) ++ [e2]) entries number