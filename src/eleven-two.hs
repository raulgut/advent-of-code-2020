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
  let adjacents = fromList . map (getAdjacents mtx) $ [(y',x') | y' <- [1..nrows mtx], x' <- [1..ncols mtx]]
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
    in if (length . filter (=='#') $ values) >= 5 then
         process prev (setElem 'L' (y,x) actual) adjacents (y, x + 1) 
       else
         process prev actual adjacents (y, x + 1)

-- | get adjacent valid positions
getAdjacents :: Matrix Char -> (Int, Int) -> ((Int,Int),[(Int, Int)])
getAdjacents mtx (y,x)
  = let topLeft = getAdjacentTopLeft mtx (y,x)
        top = getAdjacentTop mtx (y,x)
        topRight = getAdjacentTopRight mtx (y,x)
        left = getAdjacentLeft mtx (y,x)
        right = getAdjacentRight mtx (y,x)
        bottomLeft = getAdjacentBottomLeft mtx (y,x)
        bottom = getAdjacentBottom mtx (y,x)
        bottomRight = getAdjacentBottomRight mtx (y,x)
    in ((y,x), topLeft ++ top ++ topRight ++ left ++ right ++ bottomLeft ++ bottom ++ bottomRight)

-- | get adjacent top-left position        
getAdjacentTopLeft :: Matrix Char -> (Int, Int) -> [(Int, Int)]
getAdjacentTopLeft mtx (y,x) 
  | (y - 1 > 0) && (x - 1 > 0) = 
    if getElem (y - 1) (x - 1) mtx == '.' then
      getAdjacentTopLeft mtx (y - 1, x - 1)
    else [(y - 1, x - 1)]
getAdjacentTopLeft mtx (y,x) = []

-- | get adjacent top-right position        
getAdjacentTopRight :: Matrix Char -> (Int, Int) -> [(Int, Int)]
getAdjacentTopRight mtx (y, x) 
  | (y - 1 > 0) && (x + 1 <= ncols mtx) = 
    if getElem (y - 1) (x + 1) mtx == '.' then
      getAdjacentTopRight mtx (y - 1, x + 1)
    else [(y - 1, x + 1)]
getAdjacentTopRight mtx (y,x) = []

-- | get adjacent top position        
getAdjacentTop :: Matrix Char -> (Int, Int) -> [(Int, Int)]
getAdjacentTop mtx (y,x) 
  | (y - 1 > 0) = 
    if getElem (y - 1) x mtx == '.' then
      getAdjacentTop mtx (y - 1, x)
    else [(y - 1, x)]
getAdjacentTop mtx (y,x) = []

-- | get adjacent bottom-left position        
getAdjacentBottomLeft :: Matrix Char -> (Int, Int) -> [(Int, Int)]
getAdjacentBottomLeft mtx (y,x) 
  | (y + 1 <= nrows mtx) && (x - 1 > 0) = 
    if getElem (y + 1) (x - 1) mtx == '.' then
      getAdjacentBottomLeft mtx (y + 1, x - 1)
    else [(y + 1, x - 1)]
getAdjacentBottomLeft mtx (y,x) = []

-- | get adjacent bottom-right position        
getAdjacentBottomRight :: Matrix Char -> (Int, Int) -> [(Int, Int)]
getAdjacentBottomRight mtx (y,x) 
  | (y + 1 <= nrows mtx) && (x + 1 <= ncols mtx) = 
    if getElem (y + 1) (x + 1) mtx == '.' then
      getAdjacentBottomRight mtx (y + 1, x + 1)
    else [(y + 1, x + 1)]
getAdjacentBottomRight mtx (y,x) = []

-- | get adjacent bottom position        
getAdjacentBottom :: Matrix Char -> (Int, Int) -> [(Int, Int)]
getAdjacentBottom mtx (y,x) 
  | (y + 1 <= nrows mtx) = 
    if getElem (y + 1) x mtx == '.' then
      getAdjacentBottom mtx (y + 1, x)
    else [(y + 1, x)]
getAdjacentBottom mtx (y,x) = []

-- | get adjacent left position        
getAdjacentLeft :: Matrix Char -> (Int, Int) -> [(Int, Int)]
getAdjacentLeft mtx (y,x) 
  | (x - 1 > 0) = 
    if getElem y (x - 1) mtx == '.' then
      getAdjacentLeft mtx (y, x - 1)
    else [(y, x - 1)]
getAdjacentLeft mtx (y,x) = []

-- | get adjacent right position        
getAdjacentRight :: Matrix Char -> (Int, Int) -> [(Int, Int)]
getAdjacentRight mtx (y,x) 
  | (x + 1 <= ncols mtx) = 
    if getElem y (x + 1) mtx == '.' then
      getAdjacentRight mtx (y, x + 1)
    else [(y, x + 1)]
getAdjacentRight mtx (y,x) = []
