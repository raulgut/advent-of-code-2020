-----------------------------------------------------------------------------
-- | We use Parsec to parse strings
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (parse, Parser, ParseError (..), char, (<|>), try)
import Text.Parsec.Combinator (many1)
import Data.MultiSet (MultiSet, fromList, occur)
import Data.List (nub)

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
  let multisetPos = fromList positions
  putStrLn . show . length . filter (\pos -> odd . occur pos $ multisetPos) . nub $ positions

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

-- | South East and SouthWest Directions
southParser :: Parser Direction 
southParser = 
  do char 's'
     c <- (try (char 'e')) <|> (char 'w')
     return $ if c == 'e' then SouthEast else SouthWest

-- | North East and NorthWest Directions
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
