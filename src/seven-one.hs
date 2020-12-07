-----------------------------------------------------------------------------
-- | We use Parsec to parse strings, graphs to generate relations, maps for
-- mappings
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser)
import Text.Parsec.Error(ParseError (..))
import Text.Parsec.Char (alphaNum, string, char, anyChar, space, digit, letter)
import Text.Parsec.Combinator (many1, manyTill, sepBy)
import Text.Parsec.Prim (parse, try, (<|>))
import Data.Map (Map, fromList, lookup)
import Data.Graph(buildG, Graph, reachable)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map extract . lines $ input) :: [(String, [String])]
  let numberTranslation = fromList . (\x -> zip x [0..]) . map fst $ entries
  let graph = buildG (0, length entries - 1) . concatMap ((\(bag,bags) -> map (getRel numberTranslation bag) bags)) $ entries
  let bagTranslation = fromList . zip [0..] . map fst $ entries
  let shinyGoldNumber = 
        case Data.Map.lookup "shiny gold bags" numberTranslation of
          Just n -> n 
          Nothing -> error $ "shiny gold bags is not a valid bag name."
  putStrLn . show . (\x -> (length x) - 1) . reachable graph $ shinyGoldNumber

-- | Parse the entry into pairs (Key,Value)
extract :: String -> (String, [String])
extract str
  = case (parse bagParser "" str) of
        Left err -> error . show $ err 
        Right bag -> bag

-- | parse bag
bagParser :: Parser (String, [String])
bagParser = 
  do bag <- manyTill anyChar (try (string " contain "))
     content <- containParser
     return (bag, content)

-- | parse containg
containParser :: Parser [String]
containParser = noBagParser <|> numberBagsParser

-- | parse "no other bags."
noBagParser :: Parser [String]
noBagParser = 
  do string "no other bags."
     return []

-- | parse number bags strings
numberBagsParser :: Parser [String]
numberBagsParser = sepBy numberBagParser (string ", ")

-- | parse number space bags string
numberBagParser :: Parser String
numberBagParser = 
  do num <- many1 digit
     space
     bag <- many1 (letter <|> space)
     return $ let n = read num in if n == 1 then bag ++ "s" else bag

-- | we obtain the arcs of the Graph
getRel :: Map String Int -> String -> String -> (Int,Int)
getRel numberTranslation bag1 bag2 =
  case Data.Map.lookup bag1 numberTranslation of
    Just n1 -> case Data.Map.lookup bag2 numberTranslation of
                 Just n2 -> (n2,n1) 
                 Nothing -> error $ bag2 ++ "is not a valid bag name."
    Nothing -> error $ bag1 ++ " is not a valid bag name."