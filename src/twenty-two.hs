{-# LANGUAGE MultiWayIf #-}
-----------------------------------------------------------------------------
-- | 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List (transpose, partition, find, deleteBy, sort)
import Text.ParserCombinators.Parsec (parse, Parser, ParseError (..), oneOf, newline, try, char, digit, string, (<|>), many)
import Text.Parsec.Combinator (many1, eof)
import Data.MultiSet (MultiSet, fromList, toOccurList)
import Data.Set (Set, fromList, member)
import Data.Map (Map, fromList, lookup, elems, assocs, delete, size)
import Data.Maybe (Maybe, fromJust, isJust)
import Text.PrettyPrint.HughesPJ (Doc, text, hcat, (<>))

main = do
  args <- getArgs
  input <- readFile . head $ args
  let tiles = (extract input) :: [(Int, [String])]
  let borders = map (\(tileId,tileShape) -> (tileId,getBorders tileShape)) tiles
  let dimension = truncate . sqrt . fromIntegral . length $ tiles
  let tileMap = Data.Map.fromList tiles
  let solution = sort . fromJust $ solvePuzzle dimension tileMap (Data.Map.fromList borders) [] 1 1
  let monsterSharp = length . concatMap (filter (=='#')) $ seaMonster
  putStrLn . show . map (\grid -> let number = (findSeaMonster 0 0 grid) in (length . concatMap (filter (=='#')) $ grid) - (number * monsterSharp)) . {-- hcat . map ((Text.PrettyPrint.HughesPJ.<> text "\n") . hcat . map (text . (++"\n"))) . --} getOptions . compound dimension 1 . map (\((x,y),(tileId,arrange)) -> ((x,y),(tileId,rearrange arrange . fromJust . Data.Map.lookup tileId $ tileMap))) $ solution

-- | Parse the puzzle tiles
extract :: String -> [(Int, [String])]
extract str
  = case (parse inputParser "" str) of
      Left err -> error . show $ err 
      Right input -> input

-- | Parse the tiles
inputParser :: Parser [(Int, [String])]
inputParser = 
  do tiles <- (many1 tileParser)
     return tiles

-- | Parse a tile
tileParser :: Parser (Int, [String])
tileParser = 
  do string "Tile "
     tileId <- many1 digit 
     char ':'
     newline
     tiles <- (many1 rowParser)
     return (read tileId,tiles)
  
-- | Parse a tile row
rowParser :: Parser String
rowParser = 
  do row <- many1 (oneOf ".#")
     many newline
     return row

-- | Given a pice, get all possible borders
getBorders :: [String] -> [String]
getBorders piece = let transposed = transpose piece
                   in [head piece, reverse . head $ piece
                      , last piece, reverse . last $ piece
                      , head transposed, reverse . head $ transposed
                      , last transposed, reverse . last $ transposed]

-- | check if the tile is external
isExternal :: Set String -> (Int,[String]) -> (Int,Int)
isExternal corners (tileId,tile) = (tileId, length . filter (==True) . map (\x -> member x corners) $ tile)

-- | solve Puzzle, return coordinates, tileId and tile
solvePuzzle :: Int -> Map Int [String] -> Map Int [String] -> [((Int,Int),(Int,[String]))] -> Int -> Int -> Maybe [((Int,Int),(Int,[String]))]
solvePuzzle dimension tiles borders previous row column = 
  let externalBorders = Data.Set.fromList . map fst . filter ((==1) . snd) . toOccurList . Data.MultiSet.fromList . concat . Data.Map.elems $ borders
      externalTiles = partition ((==0) . snd) $ map (isExternal externalBorders) (assocs borders)
      (corners,sides) = partition ((==4) . snd) (snd externalTiles)
      cornerTiles = map ((\x -> (x,fromJust . Data.Map.lookup x $ borders)) . fst) $ corners    
      sideTiles = map ((\x -> (x,fromJust . Data.Map.lookup x $ borders)) . fst) $ sides
      topLeft = if (row == 1) && (column == 1) then 
                  let topLeftTile = if dimension == 1 then head sideTiles else head cornerTiles 
                  in case (getArrangement (\[a,b,c,d] -> (member a externalBorders) && (member c externalBorders)) topLeftTile)  of
                       Nothing -> error "Not top-left tail."
                       Just tail -> tail
                else
                  let (Just (_,(_,[leftA, leftB, leftC, leftD]))) = find ((==(row, column - 1)) . fst) $ previous
                      (Just (_,(_,[topA, topB, topC, topD]))) = find ((==(row - 1, column)) . fst) $ previous
                      conditionTopLeft = (\[a,b,c,d] -> (a == topB) && (c == leftD))
                  in case filter isJust $ map (getArrangement conditionTopLeft) (if dimension == 1 then sideTiles else cornerTiles) of
                       ((Just tail):_) -> tail
                       otherwise -> error $ "Not top-left tail."
      iteration = if dimension == 1 then Just [((row,column),topLeft)] else rearrangeTop dimension (deleteBy (\x y -> fst x == fst y) topLeft cornerTiles) sideTiles externalBorders tiles 2 [((row,column),topLeft)]
  in if isJust iteration then
       let newTiles = foldl (\x y -> delete (fst y) x) tiles (map snd . fromJust $ iteration)
           newDimension = dimension - 2
           newBorders = foldl (\x y -> delete (fst y) x) borders (map snd . fromJust $ iteration)
       in if size newTiles == 0 then 
            iteration
          else 
            case solvePuzzle newDimension newTiles newBorders (fromJust iteration) (row + 1) (column + 1) of 
              Nothing -> Nothing 
              Just sol -> Just $ (fromJust iteration) ++ sol
     else
       Nothing 

-- | Obtain the right arrangent of the input tile
getArrangement :: ([String] -> Bool) -> (Int,[String]) -> Maybe (Int,[String])
getArrangement condition (tileId, [top, reverseTop
                                  , bottom, reverseBottom
                                  , left, reverseLeft
                                  , right, reverseRight]) =
  case find condition  [[top, bottom, left, right]
                       ,[reverseLeft, reverseRight, bottom, top]
                       ,[reverseBottom, reverseTop, reverseRight, reverseLeft]
                       ,[right, left, reverseTop, reverseBottom]
                       ,[reverseTop, reverseBottom, right, left]
                       ,[reverseRight, reverseLeft, reverseBottom, reverseTop]
                       ,[bottom, top, reverseLeft, reverseRight]
                       ,[left, right, top, bottom]
                       ] of
    Just sol -> Just (tileId, sol)
    Nothing -> Nothing

-- | Solve Puzzle from the top
rearrangeTop :: Int -> [(Int,[String])] -> [(Int,[String])] -> Set String -> Map Int [String] -> Int -> [((Int,Int),(Int,[String]))] -> Maybe [((Int,Int),(Int,[String]))]
rearrangeTop dimension corners sides externalBorders tiles pos (((row,column),(tileId,[top,bottom,left,right])):arranged) = 
  if | dimension == pos -> 
                              let conditionCorner = (\[a,b,c,d] -> (member a externalBorders) && (member d externalBorders) && (c == right))
                                  next tile = rearrangeRight dimension (deleteBy (\x y -> fst x == fst y) tile corners) sides externalBorders tiles 2 (((row, column + 1),tile):((row,column),(tileId,[top,bottom,left,right])):arranged)
                              in case filter isJust . map (next . fromJust) . filter isJust $ map (getArrangement conditionCorner) corners of
                                   [] -> Nothing 
                                   list -> head list
     | otherwise -> let conditionSide = (\[a,b,c,d] -> (member a externalBorders) && (c == right))
                        next tile = rearrangeTop dimension corners (deleteBy (\x y -> fst x == fst y) tile sides) externalBorders tiles (pos + 1) (((row, column + 1),tile):((row,column),(tileId,[top,bottom,left,right])):arranged)
                    in case filter isJust . map (next . fromJust) . filter isJust $ map (getArrangement conditionSide) sides of
                         [] -> Nothing 
                         list -> head list

-- | After top, solve Puzzle from the right
rearrangeRight :: Int -> [(Int,[String])] -> [(Int,[String])] -> Set String -> Map Int [String] -> Int -> [((Int,Int),(Int,[String]))] -> Maybe [((Int,Int),(Int,[String]))]
rearrangeRight dimension corners sides externalBorders tiles pos (((row,column),(tileId,[top,bottom,left,right])):arranged) =
  if | dimension == pos -> 
                           let conditionCorner = (\[a,b,c,d] -> (member d externalBorders) && (member b externalBorders) && (a == bottom))
                               next tile = rearrangeBottom dimension (deleteBy (\x y -> fst x == fst y) tile corners) sides externalBorders tiles 2 (((row + 1, column),tile):((row,column),(tileId,[top,bottom,left,right])):arranged)
                           in case filter isJust . map (next . fromJust) . filter isJust $ map (getArrangement conditionCorner) corners of
                                [] -> Nothing 
                                list -> head list
     | otherwise -> let conditionSide = (\[a,b,c,d] -> (member d externalBorders) && (a == bottom))
                        next tile = rearrangeRight dimension corners (deleteBy (\x y -> fst x == fst y) tile sides) externalBorders tiles (pos + 1) (((row + 1, column),tile):((row,column),(tileId,[top,bottom,left,right])):arranged)
                    in case filter isJust . map (next . fromJust) . filter isJust $ map (getArrangement conditionSide) sides of
                         [] -> Nothing 
                         list -> head list

-- | After right, solve Puzzle from the bottom
rearrangeBottom :: Int -> [(Int,[String])] -> [(Int,[String])] -> Set String -> Map Int [String] -> Int -> [((Int,Int),(Int,[String]))] -> Maybe [((Int,Int),(Int,[String]))]
rearrangeBottom dimension corners sides externalBorders tiles pos (((row,column),(tileId,[top,bottom,left,right])):arranged) =
  if | dimension == pos -> 
                      let conditionCorner = (\[a,b,c,d] -> (member b externalBorders) && (member c externalBorders) && (d == left))
                          next tile = rearrangeLeft dimension (deleteBy (\x y -> fst x == fst y) tile corners) sides externalBorders tiles 2 (((row, column - 1),tile):((row,column),(tileId,[top,bottom,left,right])):arranged)
                      in case filter isJust . map (next . fromJust) . filter isJust $ map (getArrangement conditionCorner) corners of
                           [] -> Nothing 
                           list -> head list
     | otherwise -> let conditionSide = (\[a,b,c,d] -> (member b externalBorders) && (d == left))
                        next tile = rearrangeBottom dimension corners (deleteBy (\x y -> fst x == fst y) tile sides) externalBorders tiles (pos + 1) (((row, column - 1),tile):((row,column),(tileId,[top,bottom,left,right])):arranged)
                    in case filter isJust . map (next . fromJust) . filter isJust $ map (getArrangement conditionSide) sides of
                         [] -> Nothing 
                         list -> head list

-- | After bottom, solve Puzzle from the left
rearrangeLeft :: Int -> [(Int,[String])] -> [(Int,[String])] -> Set String -> Map Int [String] -> Int -> [((Int,Int),(Int,[String]))] -> Maybe [((Int,Int),(Int,[String]))]
rearrangeLeft dimension corners sides externalBorders tiles pos (((row,column),(tileId,[top,bottom,left,right])):arranged) =
  if | dimension == pos -> Just (((row,column),(tileId,[top,bottom,left,right])):arranged)
     | otherwise -> let conditionSide = (\[a,b,c,d] -> (member c externalBorders) && (b == top))
                        next tile = rearrangeLeft dimension corners (deleteBy (\x y -> fst x == fst y) tile sides) externalBorders tiles (pos + 1) (((row - 1, column),tile):((row,column),(tileId,[top,bottom,left,right])):arranged)
                    in case filter isJust . map (next . fromJust) . filter isJust $ map (getArrangement conditionSide) sides of
                         [] -> Nothing 
                         list -> head list

-- | given a tile, get all possible arrangements
getOptions :: [String] -> [[String]]
getOptions tile = [tile, map reverse . transpose $ tile, map reverse . reverse $ tile, reverse . transpose $ tile
                  , map reverse tile, map reverse . reverse . transpose $ tile, reverse tile, transpose tile]

-- | rearrange a tile from its borders
rearrange :: [String] -> [String] -> [String]
rearrange [a,b,c,d] tile =
  let options = getOptions tile
  in case find (\opt -> (head opt == a) && (last opt == b) && (head . transpose $ opt) == c && (last . transpose $ opt) == d) options of
       Nothing -> error "Wrong arrange."
       Just arrange -> transpose . init . tail . transpose . init . tail $ arrange

-- | remove sides, get puzzle
compound :: Int -> Int -> [((Int,Int),(Int,[String]))] -> [String]
compound dimension row puzzle =
  if row > dimension then
    []
  else 
    (foldl1 (zipWith (++)) . map (snd . snd) . filter (\((x,y),_) -> x == row) $ puzzle) ++ (compound dimension (row + 1) puzzle)

-- | The shape of the sea monster
seaMonster :: [String]
seaMonster = ["                  # ","#    ##    ##    ###"," #  #  #  #  #  #   "]

-- | Find the Sea Monster in the grid
findSeaMonster :: Int -> Int -> [String] -> Int
findSeaMonster x y grid = 
  let posGrid = drop y grid 
  in if (length posGrid >= 3) then
       let newGrid = map (take (length . head $ seaMonster) . drop x) (take 3 posGrid)
       in if ((length . head $ newGrid) >= (length . head $ seaMonster)) then
            if checkSeaMonster newGrid then 
              1 + (findSeaMonster (x + 1) y grid)
            else 
              findSeaMonster (x + 1) y grid 
          else 
            findSeaMonster 0 (y + 1) grid
     else
       0

-- | check if the sea monster is the grid
checkSeaMonster :: [String] -> Bool
checkSeaMonster grid = and $ zipWith checkSeaMonsterRow grid seaMonster 

-- | check if the sea monster is the row
checkSeaMonsterRow :: String -> String -> Bool
checkSeaMonsterRow gridrow seaMonsterRow = and $ zipWith checkSeaMonsterPosition gridrow seaMonsterRow

-- | check if the sea monster is the position
checkSeaMonsterPosition :: Char -> Char -> Bool
checkSeaMonsterPosition _ ' ' = True
checkSeaMonsterPosition '#' '#' = True
checkSeaMonsterPosition '.' '#' = False
