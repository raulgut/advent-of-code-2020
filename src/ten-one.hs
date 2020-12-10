-----------------------------------------------------------------------------
-- | Compute distance in jolts
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Vector (Vector, fromList, (!), (//))
import Data.List (sort)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (sort . map read . lines $ input) :: [Int]
  putStrLn . show . (\jolts -> (jolts ! 0) * (jolts ! 2)) . process 0 (fromList [0,0,0]) $ entries

-- | Obtain the distance in jolts
process :: Int -> Vector Int -> [Int] -> Vector Int
process actual jolts []
  = jolts // [(2, (jolts ! 2) + 1)]
process actual jolts (next:rest)
  = if next <= (actual + 3) then
      let jolt = (next - actual) - 1
          value = jolts ! jolt
      in process next (jolts // [(jolt, value + 1)]) rest
    else
      error $ "There is not three jolts difference."
