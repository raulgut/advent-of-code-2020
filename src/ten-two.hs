-----------------------------------------------------------------------------
-- | Compute distance in jolts. Divide combinatory problem into small ones.
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List (sort)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (sort . map read . lines $ input) :: [Int]
  putStrLn . show . foldl1 (*) . map (length . possibilities []) . byGroups [[]] . process 0 [] $ entries

-- | Obtain the maximum distance list of inputs.
process :: Int -> [Int] -> [Int] -> [Int]
process actual distance []
  = distance ++ [3]
process actual distance (next:rest)
  = if next <= (actual + 3) then
      let jolt = next - actual
      in process next (distance ++ [jolt]) rest
    else
      error $ "There is not three jolts difference."

-- | Find groups of 1s
byGroups :: [[Int]] -> [Int] -> [[Int]]
byGroups ([]:groups) [] = groups
byGroups groups [] = groups
byGroups ([]:groups) (3:rest) = byGroups ([]:groups) rest
byGroups (group:groups) (3:rest) = byGroups ([]:group:groups) rest
byGroups (group:groups) (1:rest) = byGroups ((1:group):groups) rest
byGroups groups _ = error $ "Unexpected number" ++ (show groups)

-- | Check possibilities given a set of 1s, 2s and 3s
possibilities :: [Int] -> [Int] -> [[Int]]
possibilities visited [] = [visited]
possibilities visited [value] = [visited ++ [value]]
possibilities visited (value1:value2:rest) =
  if (value1 < 3) && (value1 + value2 <= 3) then
    (possibilities visited ((value1 + value2):rest)) 
     ++ (possibilities (visited ++ [value1]) (value2:rest))
  else
    possibilities (visited ++ [value1]) (value2:rest)
