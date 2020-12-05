{-# LANGUAGE MultiWayIf #-}
-----------------------------------------------------------------------------
-- | We use Parsec to parse strings
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser)
import Text.Parsec.Error(ParseError (..))
import Text.Parsec.Char (alphaNum, string, char, anyChar, space, digit)
import Text.Parsec.Combinator (many1, manyTill)
import Text.Parsec.Prim (parse, try, (<|>))
import Data.Char (isDigit)
import Data.List (splitAt, elem)

-- | Available Keys
data Key 
  = BYR -- ^ Birth Year
  | IYR -- ^ Issue Year
  | EYR -- ^ Expiration Year
  | HGT -- ^ Height
  | HCL -- ^ Hair Color
  | ECL -- ^ Eye Color
  | PID -- ^ Passport ID
  | CID -- ^ Country ID
  | Other String -- ^ Other
  deriving Show

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (foldl extract [[]] . lines $ input) :: [[(Key, String)]]
  putStrLn . show . length . filter (==7) . map (length . filter (isRequiredAndValid)) $ entries

-- | Parse the entry into pairs (Key,Value)
extract :: [[(Key, String)]] -> String -> [[(Key, String)]]
extract (passport:passports) str
  = if null str then
      ([]:passport:passports)
    else
      case (parse sequenceParser "" str) of
        Left err -> error . show $ err 
        Right keyValues -> (passport ++ keyValues):passports

-- | parse sequences of (Key,Value) pairs
sequenceParser :: Parser [(Key, String)]
sequenceParser = many1 keyValueParser

-- | parse (Key,Value) pairs
keyValueParser :: Parser (Key, String)
keyValueParser =
  try (byrParser <|> iyrParser <|> eclParser <|> eyrParser <|> hclParser <|> hgtParser <|> pidParser <|> cidParser) <|> otherParser

-- | byr key
byrParser :: Parser (Key, String)
byrParser =
 do string "byr:"
    value <- try (manyTill digit (try space)) <|> (many1 digit)
    return (BYR, value)

-- | iyr key
iyrParser :: Parser (Key, String)
iyrParser =
 do string "iyr:"
    value <- try (manyTill anyChar (try space)) <|> (many1 anyChar)
    return (IYR,value)

-- | eyr key
eyrParser :: Parser (Key, String)
eyrParser =
 do try (string "eyr:")
    value <- try (manyTill anyChar (try space)) <|> (many1 anyChar)
    return (EYR,value)

-- | hgt key
hgtParser :: Parser (Key, String)
hgtParser =
 do try (string "hgt:")
    value <- try (manyTill anyChar (try space)) <|> (many1 anyChar)
    return (HGT,value)

-- | hcl key
hclParser :: Parser (Key, String)
hclParser =
 do try (string "hcl:")
    value <- try (manyTill anyChar (try space)) <|> (many1 anyChar)
    return (HCL,value)

-- | ecl key
eclParser :: Parser (Key, String)
eclParser =
 do try (string "ecl:")
    value <- try (manyTill anyChar (try space)) <|> (many1 anyChar)
    return (ECL,value)

-- | pid key
pidParser :: Parser (Key, String)
pidParser =
 do string "pid:"
    value <- try (manyTill anyChar (try space)) <|> (many1 anyChar)
    return (PID,value)

-- | cid key
cidParser :: Parser (Key, String)
cidParser =
 do string "cid:"
    value <- try (manyTill anyChar (try space)) <|> (many1 anyChar)
    return (CID,value)

-- | Other, possible?
otherParser :: Parser (Key, String)
otherParser =
 do key <- manyTill anyChar (try (char ':'))
    value <- try (manyTill anyChar (try space)) <|> (many1 anyChar)
    return (Other key,value)

-- | Check if the pair (Key,Value) is a required and valid field
isRequiredAndValid :: (Key,String) -> Bool
isRequiredAndValid (CID,_) = False
isRequiredAndValid ((Other _),_) = False
isRequiredAndValid (BYR,value) 
  = (length value == 4) && (and . map isDigit $ value) && (let year = read value in year >= 1920 && year <= 2002)
isRequiredAndValid (IYR,value) 
  = (length value == 4) && (and . map isDigit $ value) && (let year = read value in year >= 2010 && year <= 2020)
isRequiredAndValid (EYR,value) 
  = (length value == 4) && (and . map isDigit $ value) && (let year = read value in year >= 2020 && year <= 2030)
isRequiredAndValid (HGT,value) 
  = let (hgt,unit) = splitAt ((length value) - 2 ) value in (if | unit == "cm" -> (let height = read hgt in height >= 150 && height <= 193)
                                                                | unit == "in" -> (let height = read hgt in height >= 59 && height <= 76)
                                                                | otherwise -> False)
isRequiredAndValid (HCL,'#':value) 
  = (length value == 6) && (and . map (\x -> (isDigit x) || (x >= 'a' && x <= 'f')) $ value)
isRequiredAndValid (HCL,_) = False 
isRequiredAndValid (ECL,value) 
  = elem value ["amb","blu","brn","gry","grn","hzl","oth"] 
isRequiredAndValid (PID,value) 
  = (length value == 9) && (and . map isDigit $ value)
