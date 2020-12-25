-----------------------------------------------------------------------------
-- | 
-----------------------------------------------------------------------------

import System.Environment (getArgs)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let [cardPublicKey,doorPublicKey] = (map read . lines $ input) :: [Int]
  let subjectNumber = 7
  let reminder = 20201227
  let cardLoopSize = findLoopSize subjectNumber reminder 0 1 $ cardPublicKey
  let doorLoopSize = findLoopSize subjectNumber reminder 0 1 $ doorPublicKey
  putStrLn . show $ applyLoopSize doorPublicKey reminder cardLoopSize 1
  putStrLn . show $ applyLoopSize cardPublicKey reminder doorLoopSize 1

-- | find the loop size - avoid overflow by applying modulo
findLoopSize :: Int -> Int -> Int -> Int -> Int -> Int
findLoopSize g p loop value publicKey =
  if value == publicKey then 
    loop 
  else 
    findLoopSize g p (loop + 1) (mod (value * g) p) publicKey

-- | apply the loop size - avoid overflow by applying modulo
applyLoopSize :: Int -> Int -> Int -> Int -> Int
applyLoopSize g p loop value =
  if loop == 0 then
    value
  else 
    applyLoopSize g p (loop - 1) (mod (value * g) p)
