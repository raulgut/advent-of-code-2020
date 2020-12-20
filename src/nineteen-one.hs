----------------------------------------------------------------------------
-- | We use Parsec to parse strings
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List (find)
import Text.ParserCombinators.Parsec.Expr (buildExpressionParser, Operator (..), Assoc (..))
import Text.ParserCombinators.Parsec (parse, Parser, ParseError (..), many, alphaNum, string, char, anyChar, space, digit, letter, newline, noneOf, (<|>), try, oneOf)
import Text.Parsec.Combinator (many1, manyTill)
import Data.Maybe (isJust, isNothing, catMaybes, fromJust)
import Data.Map (Map, fromList, lookup)

-- | Rules can have strings, links to other rules, '|' (or), or ' ' (and)
data Type 
  = Value Char
  | And [Int]
  | Or Type Type 
  deriving Show

main = do
  args <- getArgs
  input <- readFile . head $ args
  let (rules,messages) = (extract input) :: (Map Int Type,[String])
  putStrLn . show . length . concatMap (filter (\(Just str) -> null str) . check (fromJust . Data.Map.lookup 0 $ rules) rules) $ messages

-- | Parse the entry into input data
extract :: String -> (Map Int Type, [String])
extract str
  = case (parse inputParser "" str) of
      Left err -> error . show $ err 
      Right input -> input

-- | Parse the three blocks of information
inputParser :: Parser (Map Int Type, [String])
inputParser = 
  do rules <- (many1 ruleParser)
     newline
     messages <- (many1 messageParser)
     return (fromList rules, messages)

-- | Parse rules
ruleParser :: Parser (Int,Type)
ruleParser =
 do identifier <- many1 digit
    string ": "
    ruleType <- (try charParser) <|> typeParser
    newline
    return $ (read identifier,ruleType)

-- | Parse number range
typeParser :: Parser Type
typeParser = buildExpressionParser table intListParser
  where table = [[ binary "|" Or AssocLeft]
                ]
        binary s op assoc = Infix (do {string s; return op}) assoc

-- | parse char between \"
charParser :: Parser Type
charParser = do string "\""
                myChar <- letter
                string "\""
                return . Value $ myChar

-- | parse list of Int separated by space
intListParser :: Parser Type
intListParser = do values <- many1 intParser
                   return . And $ values

-- | parse integer with possible spaces
intParser :: Parser Int
intParser = do many (char ' ')
               value <- many1 digit 
               many (char ' ')
               return . read $ value

-- | Parse number range
messageParser :: Parser String 
messageParser = manyTill anyChar newline

-- | check if expression is valid
check :: Type -> Map Int Type -> String -> [Maybe String]
check (Value c1) rules (c2:rest) |  c1 == c2 = [Just rest]
check (Value _) _ _ = [Nothing]
check (And []) _ str = [Just str]
check (And (cond1:rest)) rules str = let result = check (fromJust . Data.Map.lookup cond1 $ rules) rules str
                                     in concatMap (\(Just strRest) -> check (And rest) rules strRest) . filter isJust $ result
check (Or cond1 cond2) rules str = (check cond1 rules str) 
                                     ++ (check cond2 rules str)
