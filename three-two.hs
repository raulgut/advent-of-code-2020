-----------------------------------------------------------------------------
-- | We start using Set, we could use Graphs if needed
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Set (Set, fromList, member)

-- | The shape of the Grid
data Grid
  = Grid { hlength :: Int 
         , vlength :: Int 
         , trees :: Set (Int,Int)
         } deriving Show

main = do
  args <- getArgs
  input <- readFile . head $ args
  let iLines = lines input
  let ts = (concatMap (map (\(i,j,_) -> (i,j)) . filter (\(i,j,c) -> c == '#') . setPositions) . zip [0..] $ iLines) :: [(Int,Int)]
  let myGrid = Grid (length (iLines!!0)) (length iLines) (fromList ts)
  putStrLn . show . foldl1 (*) $ [ move 0 (0,0) 1 1 myGrid
                                 , move 0 (0,0) 3 1 myGrid
                                 , move 0 (0,0) 5 1 myGrid
                                 , move 0 (0,0) 7 1 myGrid
                                 , move 0 (0,0) 1 2 myGrid]

-- | We just enumerate positions in the Matrix
setPositions :: (Int,String) -> [(Int,Int,Char)]
setPositions (i,line) = map (\(j,c) -> (j,i,c)) . zip [0..] $ line

-- | Move on the Grid
move :: Int -> (Int,Int) -> Int -> Int -> Grid -> Int
move count (x,y) xshift yshift grid
  = let next = ((x + xshift) `mod` (hlength grid), y + yshift)
    in if (snd next) >= (vlength grid) then 
         count 
       else 
         move (if (member next (trees grid)) then 
                 count + 1 
               else count) 
              next 
              xshift 
              yshift
              grid
