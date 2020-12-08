{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- | Playing with State Monads
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Control.Monad.State (MonadState (..), evalState)
import Data.Set (Set, empty, insert, member)
import Data.Maybe (isJust, catMaybes)
import Data.List (find)

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
  let possibilities = map (changeInstruction instructions) . filter (isJMPorNOP . snd) . zip [0..] $ instructions
  let result = map (\is -> evalState processInstructions $ System 0 [] is 0 empty) possibilities
  putStrLn . show . head . catMaybes $ result

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
processInstructions :: MonadState System m => m (Maybe Int)
processInstructions =
  do {
    sys <- get
    ; if member (pid sys) (visited sys) then 
        return Nothing
      else 
        case (current sys) of
          [] -> return . Just . acc $ sys
          (i:is) ->
            do { 
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

-- | The instruction is JMP or NOP
isJMPorNOP :: Instruction -> Bool
isJMPorNOP i | (typ i == JMP) = True
isJMPorNOP i | (typ i == NOP) && (value i /= 0) = True
isJMPorNOP _ = False 

-- | Change JMP by NOP and viceversa
changeInstruction :: [Instruction] -> (Int, Instruction) -> [Instruction]
changeInstruction instructions (pos,i) =
  (take pos instructions) ++ (i{ typ = if (typ i) == JMP then NOP else JMP }:(drop (pos + 1) instructions))