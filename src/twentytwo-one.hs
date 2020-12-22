-----------------------------------------------------------------------------
-- | 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List (transpose)
import Text.ParserCombinators.Parsec (parse, Parser, ParseError (..), newline, char, digit, string, (<|>), many)
import Text.Parsec.Combinator (many1)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let game = (extract input) :: [[Int]]
  putStrLn . show . foldl1 (+) . zipWith (*) [1..] . reverse . play $ game

-- | Parse the game
extract :: String -> [[Int]]
extract str
  = case (parse inputParser "" str) of
      Left err -> error . show $ err 
      Right input -> input

-- | Parse the players cards
inputParser :: Parser [[Int]]
inputParser = 
  do players <- (many1 playerParser)
     return players

-- | Parse a player
playerParser :: Parser [Int]
playerParser = 
  do string "Player "
     playerId <- many1 digit 
     char ':'
     newline
     cards <- (many1 cardParser)
     return  cards
  
-- | Parse a card
cardParser :: Parser Int
cardParser = 
  do card <- many1 digit
     many newline
     return . read $ card

-- | play the game and return the winning hand
play :: [[Int]] -> [Int]
play [p1s,[]] = p1s
play [[],p2s] = p2s
play [(p1:p1s),(p2:p2s)] 
  | p1 > p2 = play [p1s ++ [p1,p2], p2s]
  | otherwise = play [p1s, p2s ++ [p2,p1]]
