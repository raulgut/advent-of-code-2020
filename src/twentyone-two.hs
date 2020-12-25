-----------------------------------------------------------------------------
-- | We use Parsec to parse strings
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser)
import Text.Parsec.Error(ParseError (..))
import Text.Parsec.Char (alphaNum, string, space)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Prim (parse, try, (<|>))
import Data.List (nub, find, intersect, (\\), partition, sort)
import Data.Maybe (isJust)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map extract . lines $ input) :: [([String], [String])]
  let allAllergens = nub . concatMap snd $ entries
  let allIngredients = nub . concatMap fst $ entries
  let solution = process . zip allAllergens . map (foldl1 intersect . filter (not . null) . (\allergen -> map (contains allergen) entries)) $ allAllergens
  putStrLn . show . concat . (\list -> (map (++ ",") . init $ list) ++ [last list]) . concatMap snd . sort $ solution

-- | Parse the entry into (ingredients, allergens) pairs
extract :: String -> ([String], [String])
extract str
  = case (parse foodParser "" str) of
        Left err -> error . show $ err 
        Right food -> food

-- | parse food
foodParser :: Parser ([String], [String])
foodParser = 
  do ingredients <- ingredientListParser
     string "(contains "
     allergens <- allergenListParser
     return (ingredients, allergens)

-- | parse ingredients
ingredientListParser :: Parser [String]
ingredientListParser = many1 ingredientParser

-- | parse allergents
allergenListParser :: Parser [String]
allergenListParser = many1 allergentParser

-- | parse ingredient
ingredientParser :: Parser String
ingredientParser = 
  do ingredient <- many1 alphaNum
     space
     return ingredient

-- | parse ingredient
allergentParser :: Parser String
allergentParser = 
  do allergen <- many1 alphaNum
     try (string ", ") <|> (string ")")
     return allergen

-- | obtain all the ingredients that can contains the allergen
contains :: String -> ([String], [String]) -> [String]
contains allergen (ingredients,allergens) = 
  if (isJust . find (==allergen) $ allergens) then 
    ingredients 
  else 
    [] 

-- | get solution
process :: [(String,[String])] -> [(String,[String])]
process options =
  let (fixed, nonfixed) = partition ((==1) . length . snd) options
      ingredientsFixed = concatMap snd fixed
      newOptions = map (\(allergen,ingredients) -> (allergen, ingredients \\ ingredientsFixed)) $ nonfixed
  in if fixed == [] then 
       fixed ++ nonfixed 
     else 
       fixed ++ (process newOptions)