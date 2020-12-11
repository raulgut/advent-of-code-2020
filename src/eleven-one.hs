-----------------------------------------------------------------------------
-- | Just Matrix and rules. We compute adjacents at the beginning.
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Matrix (Matrix, fromLists, nrows, ncols, getElem, setElem, toList)
import Data.Map (Map, fromList, lookup)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let iLines = lines input
  let mtx = fromLists iLines
  let rows = nrows mtx 
  let cols = ncols mtx
  let adjacents = fromList . map (getAdjacents rows cols) $ [(y',x') | y' <- [1..rows], x' <- [1..cols]]
  putStrLn . show . length . filter (=='#') . toList . process mtx mtx adjacents $ (1,1)

-- | Iterate using rules
process :: Matrix Char -> Matrix Char -> Map (Int,Int) [(Int,Int)] -> (Int, Int) -> Matrix Char
process prev actual adjacents (y,x)
  | (y > nrows prev) = 
    if (prev == actual) then
      actual 
    else 
      process actual actual adjacents (1,1)
process prev actual adjacents (y,x)
  | (x > ncols prev) = 
    process prev actual adjacents (y + 1, 1)
process prev actual adjacents (y,x)
  | (getElem y x prev) == '.'
  = process prev actual adjacents (y, x + 1)
process prev actual adjacents (y,x) -- Rule 1
  | (getElem y x prev) == 'L'
  = let xyadjacents = 
          case Data.Map.lookup (y,x) adjacents of
            Nothing -> error $ "Invalid position " ++ (show (y,x)) ++ "."
            Just positions -> positions
        values = map (\(y',x') -> getElem y' x' prev) xyadjacents
    in if (and . map (\val -> val == 'L' || val == '.') $ values) then
         process prev (setElem '#' (y,x) actual) adjacents (y, x + 1) 
       else
         process prev actual adjacents (y, x + 1)
process prev actual adjacents (y,x) -- Rule 2
  | (getElem y x prev) == '#'
  = let xyadjacents = 
          case Data.Map.lookup (y,x) adjacents of
            Nothing -> error $ "Invalid position " ++ (show (y,x)) ++ "."
            Just positions -> positions
        values = map (\(y',x') -> getElem y' x' prev) xyadjacents
    in if (length . filter (=='#') $ values) >= 4 then
         process prev (setElem 'L' (y,x) actual) adjacents (y, x + 1) 
       else
         process prev actual adjacents (y, x + 1)

-- | get adjacent valid positions
getAdjacents :: Int -> Int -> (Int, Int) -> ((Int,Int),[(Int, Int)])
getAdjacents maxRows maxCols (y,x)
  = ((y,x),[(y',x') | y' <- [(y - 1)..(y + 1)]
                    , x' <- [(x - 1)..(x + 1)]
                    , (y' > 0) && (x' > 0) && (y' <= maxRows) && (x' <= maxCols) && (x' /= x || y' /= y)])