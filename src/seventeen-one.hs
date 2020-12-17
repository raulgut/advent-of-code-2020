-----------------------------------------------------------------------------
-- | We start using Set, we could use Graphs if needed
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Set (Set, fromList, member, size, elems, difference)
import Data.Map (Map, fromList, insert, lookup)
import Control.Monad.State (State (..), get, put, evalState, runState)
import Data.List (nub)

-- | State of Pocket Dimension
data PocketDimensionState =
  PocketDimensionState {
    grid :: Set (Int,Int,Int)
    , gridNeighbours :: Map (Int,Int,Int) [(Int,Int,Int)]
  } deriving Show

main = do
  args <- getArgs
  input <- readFile . head $ args
  let iLines = lines input
  let entries = (concatMap (map (\(i,j,_) -> (i,j,0)) . filter (\(i,j,c) -> c == '#') . setPositions) . zip [0..] $ iLines) :: [(Int,Int,Int)]
  let ns = map neighbours $ entries
  let mapNeighbours = Data.Map.fromList $ zip entries ns
  putStrLn . show . evalState (evolve 6) $ PocketDimensionState (Data.Set.fromList entries) mapNeighbours

-- | We just enumerate positions in the Matrix
setPositions :: (Int,String) -> [(Int,Int,Char)]
setPositions (i,line) = map (\(j,c) -> (j,i,c)) . zip [0..] $ line

-- | get tridimensional neighbours
neighbours :: (Int,Int,Int) -> [(Int,Int,Int)]
neighbours (x,y,z) = [(x', y', z') | x' <- [x - 1..x + 1], y' <- [y - 1..y + 1], z' <- [z - 1..z + 1], not $ (x == x') && (y == y') && (z == z')]

-- | Evolve n steps
evolve :: Int -> State PocketDimensionState Int
evolve 0 =
  do state <- get 
     return . size . grid $ state 
evolve n = 
  do state <- get 
     neighboursList <- traverse tryNeighbours (elems . grid $ state)
     let actives = map fst . filter ((\value -> value >=2 && value <=3) . snd) . zip (elems . grid $ state) . map (length . filter (==True) . (map (\n -> member n (grid state)))) $ neighboursList
     let possibleActives = elems . (\l -> difference l (grid state)) . Data.Set.fromList . concat $ neighboursList
     newNeighboursList <- traverse tryNeighbours possibleActives
     let newActives = map fst . filter ((==3) . snd) . zip possibleActives . map (length . filter (==True) . (map (\n -> member n (grid state)))) $ newNeighboursList
     newState <- get
     let newGrid = Data.Set.fromList (actives ++ newActives)
     put newState {grid = newGrid}
     evolve (n - 1)

-- | Check if neighbours has been computed; otherwise compute and update State
tryNeighbours :: (Int, Int, Int) -> State PocketDimensionState [(Int,Int,Int)]
tryNeighbours coordinate =
  do state <- get 
     case Data.Map.lookup coordinate (gridNeighbours state) of  
       Just cNeighbours -> return cNeighbours
       Nothing -> do let ns = neighbours coordinate
                     let newGridNeighbours = insert coordinate ns (gridNeighbours state)
                     put $ state { gridNeighbours = newGridNeighbours }
                     return ns 
