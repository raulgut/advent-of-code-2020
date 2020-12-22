{-# LANGUAGE MultiWayIf #-}
-----------------------------------------------------------------------------
-- | 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List (transpose)
import Text.ParserCombinators.Parsec (parse, Parser, ParseError (..), newline, char, digit, string, (<|>), many)
import Text.Parsec.Combinator (many1)
import Data.Set (Set, empty, insert, member)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let game = (extract input) :: [[Int]]
  putStrLn . show . foldl1 (+) . zipWith (*) [1..] . reverse . snd . play empty $ game
  
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
play :: Set [[Int]] -> [[Int]] -> (Int,[Int])
play _ [p1s,[]] = (1,p1s)
play _ [[],p2s] = (2,p2s)
play state [(p1:p1s),(p2:p2s)] =
  let newState = insert [(p1:p1s),(p2:p2s)] state
  in
  if | member [(p1:p1s),(p2:p2s)] state -> (1,[]) -- does not matter the cards
     | (length p1s >= p1) && (length p2s >= p2) -> 
         case play empty [take p1 p1s, take p2 p2s] of
           (1,_) -> play newState [p1s ++ [p1,p2], p2s]                     
           (2,_) -> play newState [p1s, p2s ++ [p2,p1]]                              
     | p1 > p2 -> play newState [p1s ++ [p1,p2], p2s]
     | otherwise -> play newState [p1s, p2s ++ [p2,p1]]