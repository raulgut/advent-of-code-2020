-----------------------------------------------------------------------------
-- | We use Parsec to parse strings
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (parse, Parser, ParseError (..), char, (<|>), try)
import Text.Parsec.Combinator (many1)
import Data.MultiSet (MultiSet, fromList, occur)
import Data.List (nub)
import Data.Set (Set, fromList, elems, member, size)
import Data.Maybe (isJust)
import Debug.Trace (trace)

-- | Possible directions
data Direction 
  = East 
  | SouthEast
  | SouthWest 
  | West 
  | NorthWest 
  | NorthEast 
  deriving Show

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map extract . lines $ input) :: [[Direction]]
  let positions = map (process (0,0)) $ entries
  let multisetPos = Data.MultiSet.fromList positions
  let initial = filter (\pos -> odd . occur pos $ multisetPos) . nub $ positions
  putStrLn . show . evolve 100 . Data.Set.fromList $ initial

-- | Parse the entry into Directions
extract :: String -> [Direction]
extract str
  = case (parse directionsParser "" str) of
      Left err -> error . show $ err 
      Right policy -> policy

-- | list of Direction
directionsParser :: Parser [Direction]
directionsParser =
 do directions <- many1 directionParser
    return directions 

-- | Direction
directionParser :: Parser Direction
directionParser =
 do direction <- eastParser <|> westParser <|> southParser <|> northParser 
    return direction 

-- | East Direction
eastParser :: Parser Direction 
eastParser = 
  do char 'e'
     return East

-- | West Direction
westParser :: Parser Direction 
westParser = 
  do char 'w'
     return West

-- | South East and South West Directions
southParser :: Parser Direction 
southParser = 
  do char 's'
     c <- (try (char 'e')) <|> (char 'w')
     return $ if c == 'e' then SouthEast else SouthWest

-- | North East and North West Directions
northParser :: Parser Direction 
northParser = 
  do char 'n'
     c <- (try (char 'e')) <|> (char 'w')
     return $ if c == 'e' then NorthEast else NorthWest

-- | move until reach destination
process :: (Int,Int) -> [Direction] -> (Int,Int)
process (x,y) [] = (x,y)
process (x,y) (East:rest) = process (x + 2,y) rest 
process (x,y) (West:rest) = process (x - 2,y) rest 
process (x,y) (SouthEast:rest) = process (x + 1,y - 1) rest 
process (x,y) (SouthWest:rest) = process (x - 1,y - 1) rest 
process (x,y) (NorthEast:rest) = process (x + 1,y + 1) rest 
process (x,y) (NorthWest:rest) = process (x - 1,y + 1) rest 

-- | change tiles every day, return blacks (size)
evolve :: Int -> Set (Int, Int) -> Int
evolve 0 blacks = size blacks
evolve step blacks =
  let blackList = elems blacks
      allPositions = nub . (++ blackList) . concatMap getAdjacents $ blackList
  in evolve (step - 1) . Data.Set.fromList . filter (isBlack blacks) $ allPositions

-- | get adjacent positions
getAdjacents :: (Int,Int) -> [(Int,Int)]
getAdjacents (x,y) = [(x + 2, y), (x - 2, y), (x + 1, y - 1), (x - 1, y - 1), (x + 1, y + 1), (x - 1, y + 1)]

-- | check if the next day position is black or white
isBlack :: Set (Int,Int) -> (Int,Int) -> Bool
isBlack blacks (x,y) = 
  let adjacents = getAdjacents (x,y)
  in if (member (x,y) blacks) then
       let toWhite = length . filter (==True) . map (\(x',y') -> member (x',y') blacks) $ adjacents
       in if (toWhite == 0) || (toWhite > 2) then 
            False 
          else 
            True
     else
       let toBlack = length . filter (==True) . map (\(x',y') -> member (x',y') blacks) $ adjacents
       in if (toBlack == 2) then 
            True
          else 
            False
