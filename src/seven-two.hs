-----------------------------------------------------------------------------
-- | We use Parsec to parse strings, we use just recursion. We could improve the
-- solution by trying to avoid visit the same nodes several times, but this
-- puzzle is small.
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser)
import Text.Parsec.Error(ParseError (..))
import Text.Parsec.Char (alphaNum, string, char, anyChar, space, digit, letter)
import Text.Parsec.Combinator (many1, manyTill, sepBy)
import Text.Parsec.Prim (parse, try, (<|>))
import Data.List (lookup)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map extract . lines $ input) :: [(String, [(Int,String)])]
  putStrLn . show . (\x -> x - 1) . compute "shiny gold bags" $ entries

-- | Parse the entry into pairs (Key,Value)
extract :: String -> (String, [(Int,String)])
extract str
  = case (parse bagParser "" str) of
        Left err -> error . show $ err 
        Right bag -> bag

-- | parse bag
bagParser :: Parser (String, [(Int,String)])
bagParser = 
  do bag <- manyTill anyChar (try (string " contain "))
     content <- containParser
     return (bag, content)

-- | parse containg
containParser :: Parser [(Int,String)]
containParser = noBagParser <|> numberBagsParser

-- | parse "no other bags."
noBagParser :: Parser [(Int,String)]
noBagParser = 
  do string "no other bags."
     return []

-- | parse number bags strings
numberBagsParser :: Parser [(Int,String)]
numberBagsParser = sepBy numberBagParser (string ", ")

-- | parse number space bags string
numberBagParser :: Parser (Int,String)
numberBagParser = 
  do num <- many1 digit
     space
     bag <- many1 (letter <|> space)
     return $ let n = read num in (n, if n == 1 then bag ++ "s" else bag)

compute :: String -> [(String, [(Int,String)])] -> Int
compute bag rels = 
  case lookup bag rels of
    Just bags -> foldl (+) 1 . map (\(n,b) -> n * (compute b rels)) $ bags
    Nothing -> error $ bag ++ " is not a valid bag name."