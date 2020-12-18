-----------------------------------------------------------------------------
-- | We use buildExpressionParser to parse strings
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec.Expr (buildExpressionParser, Operator (..), Assoc (..))
import Text.ParserCombinators.Parsec (parse, Parser, ParseError (..), string, digit, space, many, char, (<|>))
import Text.Parsec.Combinator (many1)

-- | An expression that is computed
data Expression a =
              Value a
            | Plus (Expression a) (Expression a)
            | Mul (Expression a) (Expression a)

instance Show a => Show (Expression a) where
  show (Value a) = show a 
  show (Plus a b) = "(" ++ (show a) ++ " + " ++ (show b) ++ ")"
  show (Mul a b) = "(" ++ (show a) ++ " * " ++ (show b) ++ ")"

main = do
  args <- getArgs
  input <- readFile . head $ args
  let expressions = (map extract . lines $ input) :: [Expression Int]
  putStrLn . show . foldl (+) 0 . map eval $ expressions

-- | Parse the entry into expression
extract :: String -> Expression Int
extract str
  = case (parse expressionParser "" str) of
      Left err -> error . show $ err 
      Right expression -> expression

-- | Parse expression with addition and multiplication
expressionParser :: Parser (Expression Int)
expressionParser 
  = buildExpressionParser table expr
  where table = [[ binary "+" Plus AssocLeft]
                ,[binary "*" Mul AssocLeft]
                ]
        binary s op assoc = Infix (do {string s; return op}) assoc

-- | An expression is a number or an expression wrapped in parens
expr :: Parser (Expression Int)
expr = do many space
          (do {char '('; myExpr <- expressionParser; char ')'; many space; return myExpr})
            <|> (do {value <- many1 digit; many space; return . Value . read $ value})

-- | Evaluate expression
eval :: Expression Int -> Int
eval (Value i) = i 
eval (Plus a b) = (eval a) + (eval b) 
eval (Mul a b) = (eval a) * (eval b)