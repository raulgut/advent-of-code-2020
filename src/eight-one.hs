{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- | Playing with State Monads
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Control.Monad.State (MonadState (..), evalState)
import Data.Set (Set, empty, insert, member)

-- | Our System has Instructions and an accumulator
data System = System {
  pid :: Int
  , prev :: [Instruction]
  , current :: [Instruction]
  , acc :: Int
  , visited :: Set Int
  }

-- | Show System
instance Show System where
  show sys = "\n(" ++ show (pid sys) ++ ")\n" ++ (show . head $ current sys) ++ " | acc: " ++ (show . acc $ sys)

-- | Instruction Types
data Type =
  ACC
  | JMP
  | NOP
  deriving (Show, Eq)

-- | An Instruction has a type and a value
data Instruction = Instruction {
  typ :: Type
  , value :: Int
  } deriving Eq

-- | Show Instruction
instance Show Instruction where
  show i = "\n" ++ (show . typ $ i) ++ " " ++ (show . value $ i)
  
main = do
  args <- getArgs
  input <- readFile . head $ args
  let is = (map  words . lines $ input)
  let instructions = map processInput $ is
  let result = evalState processInstructions $ System 0 [] instructions 0 empty
  putStrLn . show $ result

-- | Translate instructions  
processInput :: [String] -> Instruction
processInput (inst:('+':val):[]) =
  Instruction {
  typ =
      case inst of
        "acc" -> ACC
        "jmp" -> JMP
        "nop" -> NOP
        _ -> error "Instruction invalid."
  , value = read val
  }
processInput (inst:('-':val):[]) =
  Instruction {
  typ =
      case inst of
        "acc" -> ACC
        "jmp" -> JMP
        "nop" -> NOP
        _ -> error "Instruction invalid."
  , value = (read val) * (-1)
  }
processInput _ = error "Instruction invalid."

-- | Execute the program
processInstructions :: MonadState System m => m Int
processInstructions =
  do {
    sys <- get
    ; if member (pid sys) (visited sys) then 
        return (acc sys)
      else 
        do {  
          case (current sys) of
            [] -> error $ "End of instructions."
            (i:is) ->
              case (typ i) of
                NOP -> put $ sys { prev = (prev sys) ++ [i] 
                                 , current = is 
                                 , visited = insert (pid sys) (visited sys) 
                                 , pid = (pid sys) + 1 
                                 }
                ACC -> put $ sys { prev = (prev sys) ++ [i] 
                                 , current = is 
                                 , acc = (acc sys) + (value i) 
                                 , visited = insert (pid sys) (visited sys) 
                                 , pid = (pid sys) + 1 
                                 }
                JMP -> 
                  let offset = value i
                  in if | offset > 0 -> put $ sys { prev = (prev sys) ++ (take offset (i:is)) 
                                                  , current = drop offset (i:is)
                                                  , visited = insert (pid sys) (visited sys) 
                                                  , pid = (pid sys) + offset 
                                                  }
                        | offset < 0 -> put $ sys { prev = take (length (prev sys) - (abs offset)) . prev $ sys 
                                                  , current = (drop (length (prev sys) - (abs offset)) $ prev sys) ++ (i:is)
                                                  , visited = insert (pid sys) (visited sys) 
                                                  , pid = (pid sys) + offset 
                                                  }
                        | otherwise -> error $ "Offset cannot be 0."      
          ; processInstructions
          }
    }
  