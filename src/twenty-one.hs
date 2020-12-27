-----------------------------------------------------------------------------
-- | 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List (transpose)
import Text.ParserCombinators.Parsec (parse, Parser, ParseError (..), oneOf, newline, try, char, digit, string, (<|>), many)
import Text.Parsec.Combinator (many1, eof)
import Data.MultiSet (MultiSet, fromList, toOccurList)
import Data.Set (Set, fromList, member)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let tiles = (extract input) :: [(Int, [String])]
  let borders = map (\(tileId,tileShape) -> (tileId,getBorders tileShape)) tiles
  putStrLn . show . foldl1 (*) . map fst . (\corners -> filter (isCorner corners) borders) . Data.Set.fromList . map fst . filter ((==1) . snd) . toOccurList . Data.MultiSet.fromList . concatMap snd $ borders

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

-- |
getBorders :: [String] -> [String]
getBorders piece = let transposed = transpose piece
                   in [head piece, reverse . head $ piece
                      , last piece, reverse . last $ piece
                      , head transposed, reverse . head $ transposed
                      , last transposed, reverse . last $ transposed]

-- |
isCorner :: Set String -> (Int,[String]) -> Bool
isCorner corners (tileId,tile) = (==4) . length . filter (==True) . map (\x -> member x corners) $ tile