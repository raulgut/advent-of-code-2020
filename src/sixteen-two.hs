-----------------------------------------------------------------------------
-- | We use Parsec to parse strings, check combinations after ordering
-- possibilities
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List (find, intersect, nub, transpose, sortBy, sort)
import Text.ParserCombinators.Parsec (parse, Parser, ParseError (..), alphaNum, string, char, anyChar, space, digit, newline, noneOf)
import Text.Parsec.Combinator (many1, manyTill, sepBy)
import Data.Maybe (fromJust, isNothing, isJust, catMaybes)

-- | Our Rules consists on a name and condition that returns Nothing if
-- satisfied
data Rule 
  = Rule { ruleName :: String
         , condition :: Int -> Maybe String
         }

main = do
  args <- getArgs
  input <- readFile . head $ args
  let (rules,myTicket,nearby) = (extract input) :: ([Rule], [Int], [[Int]])
  putStrLn . show . foldl multiplyDeparture 1 . zip myTicket . map snd . sort . fromJust . getSolution [] . sortBy (\(_,a) (_,b) -> compare (length a) (length b)) . zip [1..] . map (foldl1 intersect) . transpose . catMaybes . map (validate rules) $ nearby

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
    return $ Rule name (\i -> if ((i >= from1) && (i <=to1)) || ((i >= from2) && (i <=to2)) then Just name else Nothing)

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
validate :: [Rule] -> [Int] -> Maybe [[String]] 
validate rules values =
  let fieldsCheck = 
        map (\value -> let fieldCheck = filter isJust [condition r $ value | r <- rules]
                       in if null fieldCheck then Nothing else Just (catMaybes fieldCheck)) values
  in if (not . null . filter isNothing) fieldsCheck then Nothing else Just . catMaybes $ fieldsCheck

-- | check combinations
getSolution :: [(Int,String)] -> [(Int,[String])] -> Maybe [(Int,String)]
getSolution processed [] = 
  Just processed 
getSolution processed ((pos,(field:fields)):option) = 
  if elem field (map snd processed) then 
    getSolution processed ((pos,fields):option)
  else
    case getSolution (processed ++ [(pos,field)]) option of 
      Nothing -> getSolution processed ((pos,fields):option)
      Just sol -> Just sol
getSolution processed ((pos,[]):option) = Nothing

-- | find "departure" fields and multiply
multiplyDeparture :: Int -> (Int,String) -> Int
multiplyDeparture result (value,('d':'e':'p':'a':'r':'t':'u':'r':'e':_)) = result * value 
multiplyDeparture result (value,_) = result 