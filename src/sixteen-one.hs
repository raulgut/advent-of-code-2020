-----------------------------------------------------------------------------
-- | We use Parsec to parse strings
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List (find)
import Text.ParserCombinators.Parsec (parse, Parser, ParseError (..), alphaNum, string, char, anyChar, space, digit, newline, noneOf)
import Text.Parsec.Combinator (many1, manyTill, sepBy)
import Data.Maybe (isJust, isNothing, catMaybes)

-- | Our Rules consists on a name and condition that returns Nothing if
-- satisfied
data Rule 
  = Rule { ruleName :: String
         , condition :: Int -> Maybe Int
         }

main = do
  args <- getArgs
  input <- readFile . head $ args
  let (rules,_,nearby) = (extract input) :: ([Rule], [Int], [[Int]])
  putStrLn . show . foldl (+) 0 . catMaybes . map (validate rules) $ nearby

-- | Parse the entry into input data
extract :: String -> ([Rule], [Int], [[Int]])
extract str
  = case (parse inputParser "" str) of
      Left err -> error . show $ err 
      Right input -> input

-- | Parse the three blocks of information
inputParser :: Parser ([Rule], [Int], [[Int]])
inputParser = 
  do rules <- (many1 ruleParser)
     newline
     myTicket <- myTicketParser
     newline
     nearby <- nearbyParser
     return (rules, myTicket, nearby)

-- | Parse rules
ruleParser :: Parser Rule
ruleParser =
 do name <- many1 (noneOf ":\n")
    string ": "
    (from1,to1) <- rangeParser
    string " or "
    (from2,to2) <- rangeParser
    newline
    return $ Rule name (\i -> if ((i >= from1) && (i <=to1)) || ((i >= from2) && (i <=to2)) then Nothing else Just i)

-- | Parse number range
rangeParser :: Parser (Int, Int)
rangeParser =
 do from <- many1 digit
    char '-'
    to <- many1 digit
    return $ (read from, read to)

-- | Parse my ticket
myTicketParser :: Parser [Int]
myTicketParser = 
  do string "your ticket:"
     newline
     list <- ticketParser
     return list

-- | Parse ticket
ticketParser :: Parser [Int]
ticketParser = 
  do elements <- sepBy (many1 digit) (char ',')
     newline
     return . map read $ elements

-- | Parse nearby tickets
nearbyParser :: Parser [[Int]]
nearbyParser = 
  do string "nearby tickets:"
     newline
     list <- many1 ticketParser
     return list

-- | validate tickets with respect to rules
validate :: [Rule] -> [Int] -> Maybe Int 
validate rules values =
  let fieldsCheck = 
        filter isJust $ map (\value -> let fieldCheck = filter isNothing [condition r $ value | r <- rules]
                                       in if null fieldCheck then Just value else Nothing) values
  in if null fieldsCheck then Nothing else head fieldsCheck
                                        