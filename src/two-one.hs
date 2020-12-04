-----------------------------------------------------------------------------
-- | We use Parsec to parse strings
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List (find)
import Text.ParserCombinators.Parsec (parse, Parser, ParseError (..), alphaNum, string, char, anyChar, space, digit)
import Text.Parsec.Combinator (many1)

-- | Our Policy contains a minimum and maximum value, the character and the
-- password that must satisfy the policy
data Policy 
  = Policy { minValue :: Int
           , maxValue :: Int
           , character :: Char 
           , password :: String
           } deriving Show

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map extract . lines $ input) :: [Policy]
  putStrLn . show . length . filter (==True) . map isValid $ entries

-- | Parse the entry into Policy
extract :: String -> Policy
extract str
  = case (parse policyParser "" str) of
      Left err -> error . show $ err 
      Right policy -> policy

-- | A Policy has the format "NATURAL-NATURAL CHAR: PASSWORD"
policyParser :: Parser Policy
policyParser =
 do n1 <- many1 digit
    char '-'
    n2 <- many1 digit
    space 
    c <- anyChar
    string ": "
    pwd <- many1 alphaNum
    return (Policy (read n1) (read n2) c pwd)

-- | check if Policy is valid
isValid :: Policy -> Bool
isValid Policy { minValue = minv 
               , maxValue = maxv 
               , character = c 
               , password = pwd } 
  = (\x -> (x >= minv) && (x <= maxv)) . length . filter (==c) $ pwd
