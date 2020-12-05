{-# LANGUAGE MultiWayIf #-}
-----------------------------------------------------------------------------
-- | Translate from Binary to Int
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List (find, sort)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (lines $ input) :: [String]
  putStrLn . show . findMissing . map ((\(x,y) -> x * 8 + y) . (\(x,y) -> (process ('F','B') 0 x, process ('L','R') 0 y)) . splitAt 7) $ entries

-- | We  obtain the ID from a binary input represented by letters
process :: (Char, Char) -> Int -> String -> Int
process _ pos [] = pos
process (left,right) pos (c:cs)
  = let middle = div (2^(length (c:cs))) 2
    in if | c == left -> process (left,right) pos cs 
          | c == right -> process (left,right) (pos + middle) cs 
          | otherwise -> error $ "Character " ++ (c:" is not a valid entry character.")

-- | We start from the minimum ID and try to find the missing one
findMissing :: [Int] -> Int
findMissing list = 
  let sList = (\(minID:ids) -> zip [minID..] (minID:ids)) . sort $ list 
  in case find (\(x,y) -> x /= y) sList of
       Just (x,y) -> x
       Nothing -> error "The plane is full"